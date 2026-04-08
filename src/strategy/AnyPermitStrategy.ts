import { AbilityStrategy } from './AbilityStrategy';
import AbilityPolicyEffect from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

export class AnyPermitStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<R, E> {
  evaluate() {
    return this.hasPermit() ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny;
  }
}

export default AnyPermitStrategy;
