import AbilityPolicy from '~/core/AbilityPolicy';
import { AbilityError } from '~/core/AbilityError';
import { AbilityResult } from '~/core/AbilityResult';
import AbilityMatch from '~/core/AbilityMatch';
import { AbilityStrategy } from '~/strategy/AbilityStrategy';

export interface AbilityResolverOptions<TTags extends string> {
  tags?: readonly TTags[];
}

type ExtractResources<P> = P extends AbilityPolicy<infer R, any, any> ? R : never;

type ExtractEnvironment<P> = P extends AbilityPolicy<any, infer E, any> ? E : never;

type ExtractTags<P> = P extends AbilityPolicy<any, any, infer T> ? T : never;

type ExtractResourceByPermission<P, Perm extends string> =
  P extends AbilityPolicy<infer R, any, any> ? (Perm extends keyof R ? R[Perm] : never) : never;

export class AbilityResolver<
  P extends AbilityPolicy<any, any, any>, // Политика
  S extends AbilityStrategy<
    P extends AbilityPolicy<infer R, infer E, any> ? R : never,
    P extends AbilityPolicy<any, infer E, any> ? E : never
  >,
  TTags extends string = P extends AbilityPolicy<any, any, infer T> ? T : never,
> {
  private readonly policies: readonly P[];
  private readonly StrategyClass: new (policies: readonly P[]) => S;

  public constructor(
    /**
     * `Important!` The incorrect Resources type was intentionally passed to AbilityPolicy so that TypeScript could suggest the name of the permission and the structure of its resource in the parse method.
     */
    policyOrListOfPolicies: readonly P[] | P,
    strategy: new (policies: readonly P[]) => S,
    options: AbilityResolverOptions<TTags> = {},
  ) {
    const policies = this.toArray(policyOrListOfPolicies);

    this.policies = options.tags
      ? policies.filter(p => p.tags.some(tag => options.tags!.includes(tag as TTags)))
      : policies;

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
    environment?: ExtractEnvironment<P>,
  ): AbilityResult<ExtractResourceByPermission<P, Permission>, ExtractEnvironment<P>> {
    const filteredPolicies = this.policies.filter(policy =>
      AbilityResolver.isInPermissionContain(
        policy.permission,
        String(permission).replace(/^permission\./, ''),
      ),
    );

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
    environment?: ExtractEnvironment<P>,
  ): void | never {
    const result = this.resolve(permission, resource, environment);

    if (result.isDenied()) {
      throw new AbilityError(`Permission denied`);
    }
  }

  /**
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
}

export default AbilityResolver;
