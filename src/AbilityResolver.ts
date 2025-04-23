import AbilityPolicy from './AbilityPolicy';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityMatch from '~/AbilityMatch';

class AbilityResolver {

  public static resolve(
    policyOrPolicies: AbilityPolicy | readonly AbilityPolicy[],
    resource: unknown,
    action: string,
  ): AbilityPolicyEffect | null {
    const policies =
      policyOrPolicies instanceof AbilityPolicy ? [policyOrPolicies] : policyOrPolicies;

    const effects = policies
      .filter(policy => AbilityPolicy.isInActionContain(policy.action, action))
      .reduce<AbilityPolicyEffect[]>((collect, policy, _index) => {
        const matchState = policy.check(resource);

        if (matchState.isEqual(AbilityMatch.MATCH)) {
          return collect.concat(policy.effect);
        }

        return collect;
      }, []);

    if (effects.length) {
      return effects[effects.length - 1];
    }

    return null;
  }

  public static isPermit(
    policyOrPolicies: AbilityPolicy | readonly AbilityPolicy[],
    resource: unknown,
    action: string,
  ): boolean {
    const effect = AbilityResolver.resolve(policyOrPolicies, resource, action);

    return effect !== null && effect.isEqual(AbilityPolicyEffect.PERMIT);
  }

  public static isDeny(
    policyOrPolicies: AbilityPolicy | readonly AbilityPolicy[],
    resource: unknown,
    action: string,
  ): boolean {
    const effect = AbilityResolver.resolve(policyOrPolicies, resource, action);

    return effect !== null && effect.isEqual(AbilityPolicyEffect.DENY);
  }
}

export default AbilityResolver;
