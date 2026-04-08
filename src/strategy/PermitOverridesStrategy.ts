import { AbilityStrategy } from './AbilityStrategy';
import { AbilityPolicyEffect  } from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

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
