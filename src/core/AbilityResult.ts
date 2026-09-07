import { AbilityExplainJSON, AbilityExplainPolicy } from './AbilityExplain';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import { AbilityPolicyEffectType } from './AbilityPolicyEffect';
import { AbilityStrategy } from '../strategy/AbilityStrategy';
import { ExtractResources } from '~/core/AbilityResolver';

export type AbilityResultExplainJSON = {
  readonly permission: string;
  readonly effect: AbilityPolicyEffectType;
  readonly policies: AbilityExplainJSON[];
};

export class AbilityResult<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
> {
  protected readonly permission: string;
  protected readonly effect: AbilityPolicyEffectType;
  public readonly strategy: AbilityStrategy<R, E>;

  public constructor(
    permission: string,
    effect: AbilityPolicyEffectType,
    strategy: AbilityStrategy<R, E>,
  ) {
    this.permission = permission;
    this.effect = effect;
    this.strategy = strategy;
  }

  /**
   * Returns a list of explanations for each policy involved in the ability evaluation.
   * Each item describes how a specific policy contributed to the final permission result.
   *
   * Useful for debugging, logging, or building UI tools that visualize permission logic.
   */
  public explainToString(): string {
    const resMarker = this.strategy.isDenied()
      ? `== ${this.permission} DENIED==`
      : `== ${this.permission} ALLOWED ==`;

    const policiesExplain = this.strategy.policies
      .map(policy => {
        return new AbilityExplainPolicy(policy).toString();
      })
      .join('\n');

    return `${resMarker}\n${policiesExplain}\n`;
  }

  public explainToJSON(): AbilityResultExplainJSON {
    return {
      permission: this.permission,
      effect: this.effect,
      policies: this.strategy.policies.map(policy => new AbilityExplainPolicy(policy).toJSON()),
    };
  }

  public explain(): string {
    return this.explainToString();
  }

  public decisive() {
    return this.strategy.decisivePolicy();
  }

  public explainDecisive(): string | null {
    const policy = this.decisive();

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
