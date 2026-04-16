import AbilityPolicy from './AbilityPolicy';
import { AbilityError } from './AbilityError';
import { AbilityResult } from './AbilityResult';
import { AbilityMatch } from './AbilityMatch';
import { AbilityStrategy } from '../strategy/AbilityStrategy';

export interface AbilityResolverOptions<TTags extends string> {
  tags?: readonly TTags[];
}

export type ExtractResources<P> = P extends AbilityPolicy<infer R, any, any> ? R : never;

export type ExtractEnvironment<P> = P extends AbilityPolicy<any, infer E, any> ? E : never;

export type ExtractPermission<R> =
  R extends AbilityResolver<infer P, any, any> ? keyof ExtractResources<P> & string : never;


export type ExtractResourceByPermission<P, Perm extends string> =
  P extends AbilityPolicy<infer R, any, any> ? (Perm extends keyof R ? R[Perm] : never) : never;

export type ExtractEnvironmentByPermission<P, Perm extends string> =
  P extends AbilityPolicy<any, infer E, any> ? (Perm extends keyof E ? E[Perm] : never) : never;


export class AbilityResolver<
  P extends AbilityPolicy<any, any, any>,
  S extends AbilityStrategy<
    P extends AbilityPolicy<infer R, infer E, any> ? R : never,
    P extends AbilityPolicy<any, infer E, any> ? E : never
  >,
  TTags extends string = P extends AbilityPolicy<any, any, infer T> ? T : never,
> {
  private readonly StrategyClass: new (policies: readonly P[]) => S;
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

    const filtered = options.tags
      ? policies.filter(p => p.tags.some(tag => options.tags!.includes(tag as TTags)))
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
    const inputNormalized = AbilityResolver.normalizePermission(String(permission));
    const inputSegments = inputNormalized.split('.');

    const filteredPolicies = this.policyEntries
      .filter(entry => AbilityResolver.matchPermissions(entry.segments, inputSegments))
      .map(entry => entry.policy);

    // 2. check the policies
    for (const policy of filteredPolicies) {
      if (policy.disabled) {
        continue;
      }

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

    return new AbilityResult(effect, strategy) as AbilityResult<
      ExtractResourceByPermission<P, Permission>,
      ExtractEnvironment<P>
    >;
  }

  public enforce<Permission extends keyof ExtractResources<P> & string>(
    permission: Permission,
    resource: ExtractResourceByPermission<P, Permission>,
    environment?: ExtractEnvironmentByPermission<P, Permission>,
  ): void | never {
    const result = this.resolve(permission, resource, environment);

    if (result.isDenied()) {
      throw new AbilityError(`Permission denied`);
    }
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

  public static matchPermissions(policySegments: string[], inputSegments: string[]): boolean {
    let i = 0;

    for (; i < policySegments.length; i++) {
      const pSeg = policySegments[i];
      const iSeg = inputSegments[i];

      // '*' — глобальный wildcard: матчим всё, что дальше
      if (pSeg === '*') {
        return true;
      }

      // input закончился раньше — mismatch
      if (iSeg === undefined) {
        return false;
      }

      // обычное сравнение
      if (pSeg !== iSeg) {
        return false;
      }
    }

    // Если политика закончилась, но input длиннее — match только если последний сегмент был '*'
    return i === inputSegments.length;
  }
}

export default AbilityResolver;
