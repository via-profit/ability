import AbilityPolicy from './AbilityPolicy';
import { AbilityError } from './AbilityError';
import { AbilityResult } from './AbilityResult';
import { AbilityMatch } from './AbilityMatch';
import { AbilityStrategy } from '../strategy/AbilityStrategy';
import { EnvironmentObject, ResourceObject } from '~/core/AbilityTypeGenerator';

export interface AbilityResolverOptions<TTags extends string> {
  tags?: readonly TTags[];
  readonly onDeny?: EnforceOnDeny;
  readonly onAllow?: EnforceOnAllow;
}

export type ExtractResources<P> = P extends AbilityPolicy<infer R, any, any> ? R : never;

export type ExtractEnvironment<P> = P extends AbilityPolicy<any, infer E, any> ? E : never;

export type ExtractPermission<R> =
  R extends AbilityResolver<infer P, any, any> ? keyof ExtractResources<P> & string : never;


export type ExtractResourceByPermission<P, Perm extends string> =
  P extends AbilityPolicy<infer R, any, any> ? (Perm extends keyof R ? R[Perm] : never) : never;

export type ExtractEnvironmentByPermission<P, Perm extends string> =
  P extends AbilityPolicy<any, infer E, any> ? (Perm extends keyof E ? E[Perm] : never) : never;


export type EnforceOptions<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
> = {
  readonly onDeny?: EnforceOnDeny<R, E>;
  readonly onAllow?: EnforceOnAllow<R, E>;
};

export type EnforceOnDeny<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
> = (result: AbilityResult<R, E>) => void;
export type EnforceOnAllow<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
> = (result: AbilityResult<R, E>) => void;

export class AbilityResolver<
  P extends AbilityPolicy<any, any, any>,
  S extends AbilityStrategy<
    P extends AbilityPolicy<infer R, infer E, any> ? R : never,
    P extends AbilityPolicy<any, infer E, any> ? E : never
  >,
  TTags extends string = P extends AbilityPolicy<any, any, infer T> ? T : never,
> {
  private readonly onDeny?: EnforceOnDeny;
  private readonly onAllow?: EnforceOnAllow;
  private readonly StrategyClass: new (policies: readonly P[]) => S;
  /**
   * Maximum number of permission keys in the selection cache.
   * The cache is cleared when the limit is reached, so arbitrary keys
   * (for example, passed from the client) can not cause a memory leak
   */
  public static readonly SELECTION_CACHE_LIMIT = 1024;

  /**
   * Policies selected by the requested permission key.
   * The selection depends on the key only, so it is computed once per key
   */
  private readonly selectionCache = new Map<string, readonly P[]>();
  private readonly policyEntries: readonly {
    policy: P;
    normalizedPermission: string;
    segments: string[];
  }[];

  public constructor(
    /**
     * `Important!` The incorrect Resources type was intentionally passed to AbilityPolicy so that TypeScript could suggest the name of the permission and the structure of its resource in the parse method.
     */
    policyOrListOfPolicies: readonly P[] | P,
    strategy: new (policies: readonly P[]) => S,
    options: AbilityResolverOptions<TTags> = {},
  ) {
    const policies = this.toArray(policyOrListOfPolicies);
    this.onDeny = options.onDeny;
    this.onAllow = options.onAllow;
    // Policies without tags are always used
    const filtered = options.tags
      ? policies.filter(
          p => p.tags.length === 0 || p.tags.some(tag => options.tags!.includes(tag as TTags)),
        )
      : policies;

    const sorted = [...filtered].sort((a, b) => b.priority - a.priority);

    this.policyEntries = sorted.map(policy => ({
      policy,
      normalizedPermission: AbilityResolver.normalizePermission(policy.permission),
      segments: AbilityResolver.normalizePermission(policy.permission).split('.'),
    }));

    this.StrategyClass = strategy;
  }

  /**
   * Resolve policy for the resource and permission key
   *
   * @param permission - Permission key
   * @param resource - Resource
   * @param environment
   */
  public resolve<Permission extends keyof ExtractResources<P> & string>(
    permission: Permission,
    resource: ExtractResourceByPermission<P, Permission>,
    environment?: ExtractEnvironmentByPermission<P, Permission>,
  ): AbilityResult<ExtractResourceByPermission<P, Permission>, ExtractEnvironment<P>> {
    const filteredPolicies = this.selectPolicies(String(permission));

    // 2. check the policies
    // disabled policies are checked too: `check` resets their state to `disabled`,
    // otherwise the state of the previous check would be used by the strategy
    for (const policy of filteredPolicies) {
      const policyMatchState = policy.check(resource, environment);

      if (policyMatchState === AbilityMatch.pending) {
        throw new AbilityError(
          `The policy "${policy.name}" is still in a pending state. Make sure to call "check" to evaluate the policy before resolving permissions.`,
        );
      }
    }

    // 3. Use strategy
    const strategy = new this.StrategyClass(filteredPolicies);
    const effect = strategy.evaluate();

    return new AbilityResult(permission, effect, strategy) as AbilityResult<
      ExtractResourceByPermission<P, Permission>,
      ExtractEnvironment<P>
    >;
  }

  /**
   * Returns the policies whose permission key matches the requested one
   */
  private selectPolicies(permission: string): readonly P[] {
    const cached = this.selectionCache.get(permission);
    if (cached) {
      return cached;
    }

    const inputSegments = AbilityResolver.normalizePermission(permission).split('.');
    const selected = this.policyEntries
      .filter(entry => AbilityResolver.matchPermissions(entry.segments, inputSegments))
      .map(entry => entry.policy);

    if (this.selectionCache.size >= AbilityResolver.SELECTION_CACHE_LIMIT) {
      this.selectionCache.clear();
    }
    this.selectionCache.set(permission, selected);

    return selected;
  }

  public enforce<Permission extends keyof ExtractResources<P> & string>(
    permission: Permission,
    resource: ExtractResourceByPermission<P, Permission>,
    environment?: ExtractEnvironmentByPermission<P, Permission>,
    options?: EnforceOptions,
  ): void | never {
    const result = this.resolve(permission, resource, environment);

    if (result.isDenied()) {
      // local callback first: the global one may throw its own error
      options?.onDeny?.(result);
      this.onDeny?.(result);

      throw new AbilityError(`Permission denied`);
    }

    options?.onAllow?.(result);
    this.onAllow?.(result);
  }

  /**
   * @deprecated - will be removed
   *
   * Check if the permission key is contained in another permission key
   * @param permissionA - The first permission to check
   * @param permissionB - The second permission to check
   */
  public static isInPermissionContain(permissionA: string, permissionB: string) {
    const A = permissionA.split('.');
    const B = permissionB.split('.');

    const [longer, shorter] = A.length >= B.length ? [A, B] : [B, A];

    return shorter.every((chunk, i) => {
      return chunk === '*' || longer[i] === '*' || chunk === longer[i];
    });
  }

  private toArray<T>(value: T | readonly T[]): readonly T[] {
    return [...(Array.isArray(value) ? value : [value])];
  }

  public static normalizePermission(permission: string): string {
    return permission
      .trim()
      .replace(/^permission\./, '') // remove prefix
      .replace(/\.+/g, '.') // collapse multiple dots
      .toLowerCase(); // optional: make case-insensitive
  }

  /**
   * Checks whether the permission key of the policy matches the requested key.
   *
   * - `*` in the middle of the key matches exactly one segment (`*.create` → `order.create`)
   * - `*` at the end of the key matches all the remaining segments (`order.*` → `order.item.update`)
   */
  public static matchPermissions(policySegments: string[], inputSegments: string[]): boolean {
    const lastIdx = policySegments.length - 1;

    for (let i = 0; i < policySegments.length; i++) {
      const pSeg = policySegments[i];
      const iSeg = inputSegments[i];

      // trailing wildcard matches everything after
      if (pSeg === '*' && i === lastIdx) {
        return true;
      }

      // input ended earlier — mismatch
      if (iSeg === undefined) {
        return false;
      }

      // wildcard in the middle matches exactly one segment
      if (pSeg === '*') {
        continue;
      }

      if (pSeg !== iSeg) {
        return false;
      }
    }

    // policy ended, input must end too
    return policySegments.length === inputSegments.length;
  }
}

export default AbilityResolver;
