import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import AbilityPolicy from '../core/AbilityPolicy';

/**
 * PriorityStrategy
 *
 * This strategy evaluates policies based on their numeric priority.
 * The policy with the highest priority (lowest number or highest number depending on implementation)
 * determines the final result.
 *
 * Use this strategy when:
 * - Policies have explicit priority levels.
 * - You want deterministic resolution based on ranking.
 *
 * Example:
 *   Policies:
 *     P1 (priority 10) → deny
 *     P2 (priority 1)  → permit
 *   Result: permit (P2 has higher priority)
 */
export class PriorityStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  private _decisive: AbilityPolicy<R, E> | null = null;

  evaluate() {
    const matched = this.matchedPolicies();

    // 1. Нет совпавших политик → deny, решающей политики нет
    if (matched.length === 0) {
      this._decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // 2. Сортируем по приоритету (больший приоритет — выше)
    const sorted = [...matched].sort((a, b) => b.priority - a.priority);

    // 3. Самая приоритетная политика — решающая
    const top = sorted[0];
    this._decisive = top;

    return top.effect;
  }

  decisivePolicy() {
    return this._decisive;
  }
}

export default PriorityStrategy;

