import { AbilityExplain, AbilityExplainPolicy } from './AbilityExplain';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import {AbilityPolicyEffect, AbilityPolicyEffectType} from './AbilityPolicyEffect';
import { AbilityStrategy } from '../strategy/AbilityStrategy';

export class AbilityResult<R extends ResourceObject = Record<string, unknown>, E extends EnvironmentObject = Record<string, unknown>> {
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
  public explain(): readonly AbilityExplain[] {
    return this.strategy.policies.map(policy => {
      return new AbilityExplainPolicy(policy);
    });
  }

  public isAllowed() {
    return this.strategy.isAllowed();
  }

  public isDenied() {
    return this.strategy.isDenied();
  }
}
