import { AbilityStrategy } from './AbilityStrategy';
import AbilityPolicyEffect from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

export class SequentialLastMatchStrategy<
  R extends ResourceObject,
  E extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<R, E> {
  evaluate() {
    const last = this.lastMatched();

    return last?.effect ?? AbilityPolicyEffect.deny;
  }
}

export default SequentialLastMatchStrategy;
