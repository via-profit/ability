import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

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
export class DenyOverridesStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    if (this.hasDeny()) {
      return AbilityPolicyEffect.deny;
    }
    if (this.hasPermit()) {
      return AbilityPolicyEffect.permit;
    }
    return AbilityPolicyEffect.deny;
  }
}

export default DenyOverridesStrategy;
