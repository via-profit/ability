import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

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
export class OnlyOneApplicableStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    const matched = this.matchedPolicies();
    if (matched.length === 1) {
      return matched[0].effect;
    }

    return AbilityPolicyEffect.deny;
  }
}

export default OnlyOneApplicableStrategy;
