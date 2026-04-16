import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * SequentialLastMatchStrategy
 *
 * This strategy evaluates all applicable policies in order and returns the result of the *last* applicable one.
 *
 * Use this strategy when:
 * - Later policies should override earlier ones.
 * - You want a "last rule wins" behavior.
 *
 * Example:
 *   Policies:
 *     P1 → permit
 *     P2 → deny
 *     P3 → permit
 *   Result: permit (P3 is the last applicable)
 */
export class SequentialLastMatchStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    const last = this.lastMatched();

    // Нет совпавших политик → deny по умолчанию
    if (!last) {
      this._decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // Последняя совпавшая политика — решающая
    this._decisive = last;
    return last.effect;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default SequentialLastMatchStrategy;

