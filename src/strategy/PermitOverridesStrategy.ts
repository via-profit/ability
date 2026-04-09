import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

/**
 * PermitOverridesStrategy
 *
 * This strategy gives priority to "permit".
 * If any applicable policy permits, the final result is "permit".
 * Deny is returned only if no policy permits.
 *
 * Use this strategy when:
 * - You want permissive behavior.
 * - A single positive rule should override denials.
 *
 * Example:
 *   Policies:
 *     P1 → deny
 *     P2 → permit
 *     P3 → deny
 *   Result: permit (permit overrides deny)
 */
export class PermitOverridesStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    if (this.hasPermit()) {
      return AbilityPolicyEffect.permit;
    }
    if (this.hasDeny()) {
      return AbilityPolicyEffect.deny;
    }

    return AbilityPolicyEffect.deny;
  }
}

export default PermitOverridesStrategy;
