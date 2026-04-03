import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export class OnlyOneApplicableStrategy<R extends ResourceObject, E> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    const matched = this.matchedPolicies();
    if (matched.length === 1) {
      return matched[0].effect;
    }

    return AbilityPolicyEffect.deny;
  }
}

export default OnlyOneApplicableStrategy;
