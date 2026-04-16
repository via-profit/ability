import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * FirstMatchStrategy
 *
 * This strategy evaluates policies in order and returns the result of the *first applicable* policy.
 * Remaining policies are ignored.
 *
 * Use this strategy when:
 * - Policy order matters.
 * - You want predictable, sequential rule evaluation.
 *
 * Example:
 *   Policies:
 *     P1 → not applicable
 *     P2 → permit
 *     P3 → deny
 *   Result: permit (P2 is the first applicable)
 */
export class FirstMatchStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    const first = this.firstMatched();

    // Если нет совпавших политик → deny по умолчанию
    if (!first) {
      this._decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // Первая совпавшая политика — решающая
    this._decisive = first;
    return first.effect;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default FirstMatchStrategy;

