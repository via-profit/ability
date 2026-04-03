import { AbilityExplain, AbilityExplainPolicy } from '~/core/AbilityExplain';
import AbilityMatch from '~/core/AbilityMatch';
import { ResourceObject } from '~/core/AbilityTypeGenerator';
import AbilityPolicy from '~/core/AbilityPolicy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';

export class AbilityResult<Resource extends ResourceObject = Record<string, unknown>> {
  /**
   * Already checked policies (after call the policy.check())
   */
  readonly policies: readonly AbilityPolicy<Resource>[];

  public constructor(policies: readonly AbilityPolicy<Resource>[]) {
    this.policies = policies;
  }

  /**
   * Returns a list of explanations for each policy involved in the ability evaluation.
   * Each item describes how a specific policy contributed to the final permission result.
   *
   * Useful for debugging, logging, or building UI tools that visualize permission logic.
   */
  public explain(): readonly AbilityExplain[] {
    return this.policies.map(policy => {
      return new AbilityExplainPolicy(policy);
    });
  }

  public getLastMatchedPolicy(): AbilityPolicy<Resource> | null {
    for (let i = this.policies.length - 1; i >= 0; i--) {
      if (this.policies[i].matchState.isEqual(AbilityMatch.match)) {
        return this.policies[i];
      }
    }
    return null;
  }

  public isAllowed() {
    const effect = this.getLastEffectOfMatchedPolicy();
    return effect?.isEqual(AbilityPolicyEffect.permit) ?? false;
  }

  public isDenied() {
    const effect = this.getLastEffectOfMatchedPolicy();

    return effect?.isEqual(AbilityPolicyEffect.deny) ?? true;
  }

  /**
   * Get the last effect of the policy
   *
   * @returns {AbilityPolicyEffect | null}
   */
  public getLastEffectOfMatchedPolicy(): AbilityPolicyEffect | null {
    for (let i = this.policies.length - 1; i >= 0; i--) {
      const p = this.policies[i];
      if (p.matchState.isEqual(AbilityMatch.match)) {
        return p.effect;
      }
    }
    return null;
  }
}
