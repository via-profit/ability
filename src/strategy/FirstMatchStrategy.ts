import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

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
export class FirstMatchStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<R, E> {
  evaluate() {
    const first = this.firstMatched();

    return first?.effect ?? AbilityPolicyEffect.deny;
  }
}

export default FirstMatchStrategy;
