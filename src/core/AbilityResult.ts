import { AbilityExplainPolicy } from './AbilityExplain';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import { AbilityPolicyEffectType } from './AbilityPolicyEffect';
import { AbilityStrategy } from '../strategy/AbilityStrategy';

export class AbilityResult<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
> {
  protected readonly effect: AbilityPolicyEffectType;
  protected readonly strategy: AbilityStrategy<R, E>;

  public constructor(effect: AbilityPolicyEffectType, strategy: AbilityStrategy<R, E>) {
    this.effect = effect;
    this.strategy = strategy;
  }

  /**
   * Returns a list of explanations for each policy involved in the ability evaluation.
   * Each item describes how a specific policy contributed to the final permission result.
   *
   * Useful for debugging, logging, or building UI tools that visualize permission logic.
   */
  public explain(): string {
    return this.strategy.policies
      .map(policy => {
        return new AbilityExplainPolicy(policy).toString();
      })
      .join('\n');
  }

  public decisivePolicy() {
    return this.strategy.decisivePolicy();
  }

  public explainDecisive(): string | null {
    const policy = this.decisivePolicy();

    if (!policy) {
      return null;
    }

    return new AbilityExplainPolicy(policy).toString();
  }

  public isAllowed = () => {
    return this.strategy.isAllowed();
  };

  public isDenied = () => {
    return this.strategy.isDenied();
  };
}
