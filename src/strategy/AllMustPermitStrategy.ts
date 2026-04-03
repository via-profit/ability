import { AbilityStrategy } from '~/strategy/AbilityStrategy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export class AllMustPermitStrategy<R extends ResourceObject, E> extends AbilityStrategy<
  R,
  E
> {
  evaluate() {
    const matched = this.matchedPolicies();
    if (matched.length === 0) {
      return AbilityPolicyEffect.deny;
    }

    const allPermit = matched.every(p => p.effect.isEqual(AbilityPolicyEffect.permit));

    return allPermit ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny;
  }
}

export default AllMustPermitStrategy;
