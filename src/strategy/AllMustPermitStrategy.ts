import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * AllMustPermitStrategy
 *
 * This strategy requires *every applicable policy* to return "permit".
 * If at least one policy returns "deny" or "not applicable", the final result is "deny".
 *
 * Use this strategy when:
 * - You want strict, conservative access control.
 * - All rules must explicitly allow the action.
 *
 * Example:
 *   Policies:
 *     P1 → permit
 *     P2 → permit
 *     P3 → deny
 *   Result: deny (because not all policies permitted)
 */
export class AllMustPermitStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    // 1. Нет совпавших политик → deny, но решающей политики нет
    if (!this.hasMatched()) {
      this._decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // 2. Если есть deny — она решающая
    const deny = this.firstDenied();
    if (deny) {
      this._decisive = deny;
      return AbilityPolicyEffect.deny;
    }

    this._decisive = this.firstPermitted();

    return AbilityPolicyEffect.permit;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default AllMustPermitStrategy;
