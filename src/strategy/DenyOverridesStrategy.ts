import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * DenyOverridesStrategy
 *
 * This strategy gives absolute priority to "deny".
 * If any applicable policy returns "deny", the final result is "deny".
 * Otherwise, if at least one policy permits, the result is "permit".
 *
 * Use this strategy when:
 * - Security is critical.
 * - A single denial must block access.
 *
 * Example:
 *   Policies:
 *     P1 → permit
 *     P2 → deny
 *     P3 → permit
 *   Result: deny (because deny overrides everything)
 */
export class DenyOverridesStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    // 1. Если есть deny — он решающий
    const deny = this.firstDenied();
    if (deny) {
      this._decisive = deny;
      return AbilityPolicyEffect.deny;
    }

    // 2. Если есть permit — он решающий
    const permit = this.firstPermitted();
    if (permit) {
      this._decisive = permit;
      return AbilityPolicyEffect.permit;
    }

    // 3. Нет ни permit, ни deny → deny по умолчанию
    this._decisive = null;
    return AbilityPolicyEffect.deny;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default DenyOverridesStrategy;

