import { AbilityExplainJSON, AbilityExplainPolicy } from './AbilityExplain';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import { AbilityPolicyEffect, AbilityPolicyEffectType } from './AbilityPolicyEffect';
import { AbilityStrategy } from '../strategy/AbilityStrategy';
import { AbilityPolicySnapshot } from './AbilityPolicy';

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

  /**
   * Snapshot of the evaluation state of the policies at the moment of the check.
   * The same policy instances are re-checked on every resolve, so the explanation
   * must be built from the snapshot and not from the current state of the policies.
   */
  private readonly snapshots: readonly AbilityPolicySnapshot[];

  public constructor(
    permission: string,
    effect: AbilityPolicyEffectType,
    strategy: AbilityStrategy<R, E>,
  ) {
    this.permission = permission;
    this.effect = effect;
    this.strategy = strategy;
    this.snapshots = strategy.policies.map(policy => policy.snapshot());
  }

  private explainPolicies(): AbilityExplainPolicy[] {
    return this.strategy.policies.map(
      (policy, idx) => new AbilityExplainPolicy(policy as never, this.snapshots[idx]),
    );
  }

  /**
   * Returns a list of explanations for each policy involved in the ability evaluation.
   * Each item describes how a specific policy contributed to the final permission result.
   *
   * Useful for debugging, logging, or building UI tools that visualize permission logic.
   */
  public explainToString(): string {
    const resMarker = this.isDenied()
      ? `== ${this.permission} DENIED==`
      : `== ${this.permission} ALLOWED ==`;

    const policiesExplain = this.explainPolicies()
      .map(explain => explain.toString())
      .join('\n');

    return `${resMarker}\n${policiesExplain}\n`;
  }

  public explainToJSON(): AbilityResultExplainJSON {
    return {
      permission: this.permission,
      effect: this.effect,
      policies: this.explainPolicies().map(explain => explain.toJSON()),
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

    const idx = this.strategy.policies.indexOf(policy);

    return new AbilityExplainPolicy(policy as never, this.snapshots[idx]).toString();
  }

  public isAllowed = () => {
    return this.effect === AbilityPolicyEffect.permit;
  };

  public isDenied = () => {
    return this.effect === AbilityPolicyEffect.deny;
  };
}
