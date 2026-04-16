import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * PermitOverridesStrategy
 *
 * This strategy gives priority to "permit".
 * If any applicable policy permits, the final result is "permit".
 * Deny is returned only if no policy permits.
 *
 * Use this strategy when:
 * - You want permissive behavior.
 * - A single positive rule should override denials.
 *
 * Example:
 *   Policies:
 *     P1 → deny
 *     P2 → permit
 *     P3 → deny
 *   Result: permit (permit overrides deny)
 */
export class PermitOverridesStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    // 1. Если есть permit — он выигрывает
    const permit = this.matchedPolicies().find(p => p.effect === AbilityPolicyEffect.permit);

    if (permit) {
      this._decisive = permit;
      return AbilityPolicyEffect.permit;
    }

    // 2. Если permit нет — ищем deny
    const deny = this.matchedPolicies().find(p => p.effect === AbilityPolicyEffect.deny);

    if (deny) {
      this._decisive = deny;
      return AbilityPolicyEffect.deny;
    }

    // 3. Нет ни permit, ни deny → deny по умолчанию
    this._decisive = null;
    return AbilityPolicyEffect.deny;
  }

  decisivePolicy() {
    return this._decisive;
  }
}


export  default PermitOverridesStrategy;
