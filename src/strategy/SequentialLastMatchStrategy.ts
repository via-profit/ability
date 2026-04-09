import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

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
  evaluate() {
    const last = this.lastMatched();

    return last?.effect ?? AbilityPolicyEffect.deny;
  }
}

export default SequentialLastMatchStrategy;
