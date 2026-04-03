import AbilityPolicy from '~/core/AbilityPolicy';
import { AbilityError } from '~/core/AbilityError';
import { AbilityResult } from '~/core/AbilityResult';
import AbilityMatch from '~/core/AbilityMatch';
import { ResourcesMap } from '~/core/AbilityTypeGenerator';
import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import DenyOverridesStrategy from '~/strategy/DenyOverridesStrategy';


export class AbilityResolver<Resources extends ResourcesMap, Environment = unknown> {
  private readonly policies: readonly AbilityPolicy[];
  private readonly StrategyClass: new (
    policies: readonly AbilityPolicy<Resources, Environment>[],
  ) => AbilityStrategy<Resources, Environment>;

  public constructor(
    /**
     * `Important!` The incorrect Resources type was intentionally passed to AbilityPolicy so that TypeScript could suggest the name of the permission and the structure of its resource in the parse method.
     */
    policyOrListOfPolicies: readonly AbilityPolicy<Resources>[] | AbilityPolicy<Resources>,
    strategy?: new (
      policies: readonly AbilityPolicy<Resources, Environment>[],
    ) => AbilityStrategy<Resources, Environment>,
  ) {
    this.policies = Array.isArray(policyOrListOfPolicies)
      ? policyOrListOfPolicies
      : [policyOrListOfPolicies];

    this.StrategyClass = strategy ?? DenyOverridesStrategy;
  }

  /**
   * Resolve policy for the resource and permission key
   *
   * @param permission - Permission key
   * @param resource - Resource
   * @param environment
   */
  public resolve<Permission extends keyof Resources>(
    permission: Permission,
    resource: Resources[Permission],
    environment?: Environment,
  ): AbilityResult<Resources[Permission]> {
    // 1. filter policies by permission key (с учётом wildcard)
    const filteredPolicies = this.policies.filter(policy =>
      AbilityResolver.isInPermissionContain(
        policy.permission,
        String(permission).replace(/^permission\./, ''),
      ),
    );

    // 2. check the policies
    for (const policy of filteredPolicies) {
      const policyMatchState = policy.check(resource, environment);

      if (policyMatchState === AbilityMatch.pending) {
        throw new AbilityError(
          `The policy "${policy.name}" is still in a pending state. Make sure to call "check" to evaluate the policy before resolving permissions.`,
        );
      }
    }

    // 3. Use strategy
    const strategy = new this.StrategyClass(filteredPolicies) as AbilityStrategy<Resources[Permission], Environment>;
    const effect = strategy.evaluate();

    return new AbilityResult<Resources[Permission], Environment>(effect, strategy);
  }

  public enforce<Permission extends keyof Resources>(
    permission: Permission,
    resource: Resources[Permission],
    environment?: Environment,
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
}

export default AbilityResolver;
