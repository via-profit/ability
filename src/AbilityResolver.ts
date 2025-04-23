import AbilityPolicy from './AbilityPolicy';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityMatch from './AbilityMatch';

class AbilityResolver {
  public policies: readonly AbilityPolicy[];

  public constructor(policies: readonly AbilityPolicy[]) {
    this.policies = policies;
  }

  /**
   * Resolve policy for the resource and action
   *
   * @param policyOrPolicies - Policy or policies
   * @param resource - Resource
   * @param action - Action
   */
  public static resolve(
    policyOrPolicies: AbilityPolicy | readonly AbilityPolicy[],
    resource: unknown,
    action: string,
  ): AbilityResolver {
    const policies =
      policyOrPolicies instanceof AbilityPolicy ? [policyOrPolicies] : policyOrPolicies;

    const filteredPolicies = policies.filter(policy =>
      AbilityPolicy.isInActionContain(policy.action, action),
    );

    filteredPolicies.map(policy => policy.check(resource));

    return new AbilityResolver(filteredPolicies);
  }

  /**
   * Get the last effect of the policy
   *
   * @returns {AbilityPolicyEffect | null}
   */
  public getEffect(): AbilityPolicyEffect | null {
    const effects = this.policies.reduce<AbilityPolicyEffect[]>((collect, policy, _index) => {
      if (policy.matchState.isEqual(AbilityMatch.MATCH)) {
        return collect.concat(policy.effect);
      }
      return collect;
    }, []);

    if (effects.length) {
      return effects[effects.length - 1];
    }

    return null;
  }


  public isPermit() {
    const effect = this.getEffect();

    return effect !== null && effect.isEqual(AbilityPolicyEffect.PERMIT);
  }

  public isDeny() {
    const effect = this.getEffect();

    return effect !== null && effect.isEqual(AbilityPolicyEffect.DENY);
  }

  public getPolicy(): AbilityPolicy | null {
    return this.policies.length ? this.policies[this.policies.length - 1] : null;
  }
}

export default AbilityResolver;
