import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

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
export class PriorityStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<R, E> {
  evaluate() {
    const matched = this.matchedPolicies();
    if (matched.length === 0) {
      return AbilityPolicyEffect.deny;
    }

    const sorted = [...matched].sort((a, b) => b.priority - a.priority);
    return sorted[0].effect;
  }
}

export default PriorityStrategy;
