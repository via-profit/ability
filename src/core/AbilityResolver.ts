import AbilityPolicy from '~/core/AbilityPolicy';
import { AbilityError } from '~/core/AbilityError';
import { AbilityResult } from '~/core/AbilityResult';
import AbilityMatch from '~/core/AbilityMatch';
import { ResourcesMap } from '~/core/AbilityTypeGenerator';


export class AbilityResolver<Resources extends ResourcesMap, Environment = unknown> {
  private policies: readonly AbilityPolicy[];

  public constructor(
    /**
     * `Important!` The incorrect Resources type was intentionally passed to AbilityPolicy so that TypeScript could suggest the name of the permission and the structure of its resource in the parse method.
     */
    policyOrListOfPolicies: readonly AbilityPolicy<Resources>[] | AbilityPolicy<Resources>,
  ) {
    this.policies = Array.isArray(policyOrListOfPolicies)
      ? policyOrListOfPolicies
      : [policyOrListOfPolicies];
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
    const filteredPolicies = this.policies.filter(policy =>
      AbilityResolver.isInPermissionContain(
        policy.permission,
        String(permission).replace(/^permission\./, ''),
      ),
    );

    for (const policy of filteredPolicies) {
      const policyMatchState = policy.check(resource, environment);

      if (policyMatchState === AbilityMatch.pending) {
        throw new AbilityError(
          `The policy "${policy.name}" is still in a pending state. Make sure to call "check" to evaluate the policy before resolving permissions.`,
        );
      }
    }

    return new AbilityResult(filteredPolicies);
  }

  public enforce<Permission extends keyof Resources>(
    permission: Permission,
    resource: Resources[Permission],
    environment?: Environment,
  ): void | never {
    const result = this.resolve(permission, resource, environment);

    if (result.isDenied()) {
      const lastPolicy = result.getLastMatchedPolicy();

      if (lastPolicy) {
        throw new AbilityError(`Permission denied by policy "${lastPolicy.name.toString()}"`);
      }

      // No policy matched → implicit deny
      throw new AbilityError(`Permission denied: no matching policy found (implicit deny)`);
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
