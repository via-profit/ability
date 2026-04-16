import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * OnlyOneApplicableStrategy
 *
 * This strategy requires that *exactly one* policy is applicable.
 * If zero or more than one policy applies, the result is "deny".
 *
 * Use this strategy when:
 * - Policies must be mutually exclusive.
 * - You want to detect ambiguous or conflicting rules.
 *
 * Example:
 *   Policies:
 *     P1 → applicable
 *     P2 → applicable
 *   Result: deny (more than one applicable policy)
 */
export class OnlyOneApplicableStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    const matched = this.matchedPolicies();

    // 1. Ровно одна совпавшая политика → она решающая
    if (matched.length === 1) {
      this._decisive = matched[0];
      return matched[0].effect;
    }

    // 2. Иначе deny, решающей политики нет
    this._decisive = null;
    return AbilityPolicyEffect.deny;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default OnlyOneApplicableStrategy;

