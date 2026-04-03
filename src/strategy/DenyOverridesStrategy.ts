import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export class DenyOverridesStrategy<R extends ResourceObject, E> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    if (this.hasDeny()) return AbilityPolicyEffect.deny;
    if (this.hasPermit()) return AbilityPolicyEffect.permit;
    return AbilityPolicyEffect.deny;
  }
}

export default DenyOverridesStrategy;
