import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

/**
 * AllMustPermitStrategy
 *
 * This strategy requires *every applicable policy* to return "permit".
 * If at least one policy returns "deny" or "not applicable", the final result is "deny".
 *
 * Use this strategy when:
 * - You want strict, conservative access control.
 * - All rules must explicitly allow the action.
 *
 * Example:
 *   Policies:
 *     P1 → permit
 *     P2 → permit
 *     P3 → deny
 *   Result: deny (because not all policies permitted)
 */
export class AllMustPermitStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    const matched = this.matchedPolicies();
    if (matched.length === 0) {
      return AbilityPolicyEffect.deny;
    }

    const allPermit = matched.every(p => p.effect === AbilityPolicyEffect.permit);

    return allPermit ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny;
  }
}

export default AllMustPermitStrategy;
