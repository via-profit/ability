import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export class FirstMatchStrategy<R extends ResourceObject, E> extends AbilityStrategy<R, E> {
  evaluate() {
    const first = this.firstMatched();

    return first?.effect ?? AbilityPolicyEffect.deny;
  }
}

export default FirstMatchStrategy;
