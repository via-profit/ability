import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export class PermitOverridesStrategy<R extends ResourceObject, E> extends AbilityStrategy<
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
