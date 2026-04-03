import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export class AnyPermitStrategy<R extends ResourceObject, E> extends AbilityStrategy<R, E> {
  evaluate() {
    return this.hasPermit() ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny;
  }
}

export default AnyPermitStrategy;
