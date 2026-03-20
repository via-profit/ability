import AbilityPolicy from './AbilityPolicy';
import { AbilityError } from './AbilityError';
import { AbilityResult } from './AbilityResult';
import AbilityMatch from './AbilityMatch';
import { ResourcesMap } from './AbilityParser';



export class AbilityResolver<Resources extends ResourcesMap> {
  policies: readonly AbilityPolicy[];

  public constructor(
    /**
     * `Important!` The incorrect Resources type was intentionally passed to AbilityPolicy so that TypeScript could suggest the name of the action and the structure of its resource in the parse method.
     */
    policyOrListOfPolicies: readonly AbilityPolicy<Resources>[] | AbilityPolicy<Resources>,
  ) {
    this.policies = Array.isArray(policyOrListOfPolicies)
      ? policyOrListOfPolicies
      : [policyOrListOfPolicies];
  }

  /**
   * Resolve policy for the resource and action
   *
   * @param action - Action
   * @param resource - Resource
   */
  public async resolve<Action extends keyof Resources>(
    action: Action,
    resource: Resources[Action],
  ): Promise<AbilityResult<Resources[Action]>> {
    const filteredPolicies = this.policies.filter(policy =>
      AbilityResolver.isInActionContain(policy.action, String(action)),
    );

    // check the policies
    filteredPolicies.forEach(policy => {
      policy.check(resource);
      if (policy.matchState === AbilityMatch.pending) {
        throw new AbilityError(
          `The policy "${policy.name}" is still in a pending state. Make sure to call "check" to evaluate the policy before resolving permissions.`,
        );
      }
    });

    return new AbilityResult(filteredPolicies);
  }

  public async enforce<Action extends keyof Resources>(
    action: Action,
    resource: Resources[Action],
  ): Promise<void | never> {
    const result = await this.resolve(action, resource);

    if (result.isDenied()) {
      const policyName = result.getLastMatchedPolicy()?.name?.toString() || 'unknown';
      throw new AbilityError(`Permission denied by policy "${policyName}"`);
    }
  }

  /**
   * Check if the action is contained in another action
   * @param actionA - The first action to check
   * @param actionB - The second action to check
   */
  public static isInActionContain(actionA: string, actionB: string) {
    const A = actionA.split('.');
    const B = actionB.split('.');

    const [longer, shorter] = A.length >= B.length ? [A, B] : [B, A];

    return shorter.every((chunk, i) => {
      return chunk === '*' || longer[i] === '*' || chunk === longer[i];
    });
  }
}

export default AbilityResolver;
