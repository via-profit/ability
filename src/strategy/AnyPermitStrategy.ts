import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * AnyPermitStrategy
 *
 * This strategy returns "permit" as soon as *any* applicable policy permits the action.
 * If no policy permits, the result is "deny".
 *
 * Use this strategy when:
 * - You want optimistic access control.
 * - A single positive rule should be enough to grant access.
 *
 * Example:
 *   Policies:
 *     P1 → deny
 *     P2 → permit
 *     P3 → deny
 *   Result: permit (because at least one policy permitted)
 */
export class AnyPermitStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    // 1. Если есть permit — он решающий
    const permit = this.firstPermitted();
    if (permit) {
      this._decisive = permit;
      return AbilityPolicyEffect.permit;
    }

    // 2. Нет permit → deny по умолчанию
    this._decisive = null;
    return AbilityPolicyEffect.deny;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default AnyPermitStrategy;

