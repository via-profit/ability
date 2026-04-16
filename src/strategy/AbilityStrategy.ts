import { AbilityMatch } from '../core/AbilityMatch';
import AbilityPolicy from '../core/AbilityPolicy';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import { AbilityPolicyEffect, AbilityPolicyEffectType } from '../core/AbilityPolicyEffect';

export abstract class AbilityStrategy<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment extends EnvironmentObject = Record<string, unknown>,
> {
  private readonly matched: readonly AbilityPolicy<Resource, Environment>[];

  constructor(public readonly policies: readonly AbilityPolicy<Resource, Environment>[]) {
    this.matched = policies.filter(p => p.matchState === AbilityMatch.match);
  }

  /**
   * Executes the strategy’s decision logic and returns the final policy effect
   * (permit or deny).
   *
   * Each concrete strategy must implement its own evaluation rules:
   * - how matched policies are interpreted,
   * - how priority, order, or overrides are applied,
   * - and how the final effect is determined.
   *
   * The implementation must also record the policy that actually determined
   * the outcome (if any), so that decisivePolicy() can later expose it.
   */
  abstract evaluate(): AbilityPolicyEffectType;

  /**
   * Returns the policy that directly determined the final decision of the strategy.
   *
   * This is:
   * - the specific policy chosen by the strategy’s evaluation rules, or
   * - null if the decision was made by default (e.g., no matched policies,
   *   or the strategy cannot identify a single decisive policy).
   *
   * This method performs no computation; it simply exposes the result stored
   * during evaluate(), enabling explainability and debugging tools to show
   * why a decision was made.
   */
  abstract decisivePolicy(): AbilityPolicy<Resource, Environment> | null;

  public matchedPolicies() {
    return this.matched;
  }

  public hasMatched(): boolean {
    return this.matched.length > 0;
  }

  protected firstMatched(): AbilityPolicy<Resource, Environment> | null {
    return this.matchedPolicies()[0] ?? null;
  }

  protected lastMatched(): AbilityPolicy<Resource, Environment> | null {
    const list = this.matchedPolicies();
    return list.length > 0 ? list[list.length - 1] : null;
  }

  protected firstDenied(): AbilityPolicy<Resource, Environment> | null {
    return this.getDenyPolicies()[0] ?? null;
  }

  protected firstPermitted(): AbilityPolicy<Resource, Environment> | null {
    return this.getPermitPolicies()[0] ?? null;
  }

  protected getPermitPolicies() {
    return this.matched.filter(p => p.effect === AbilityPolicyEffect.permit);
  }

  protected getDenyPolicies() {
    return this.matched.filter(p => p.effect === AbilityPolicyEffect.deny);
  }

  protected hasPermit(): boolean {
    return this.getPermitPolicies().length > 0;
  }

  protected hasDeny(): boolean {
    return this.getDenyPolicies().length > 0;
  }

  public isAllowed(): boolean {
    return this.evaluate() === AbilityPolicyEffect.permit;
  }

  public isDenied(): boolean {
    return this.evaluate() === AbilityPolicyEffect.deny;
  }
}
