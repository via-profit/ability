import { AbilityStrategy } from './AbilityStrategy';
import AbilityPolicyEffect from '../core/AbilityPolicyEffect';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';

export class PriorityStrategy<R extends ResourceObject, E extends EnvironmentObject = Record<string, unknown>> extends AbilityStrategy<R, E> {
  evaluate() {
    const matched = this.matchedPolicies();
    if (matched.length === 0) {
      return AbilityPolicyEffect.deny;
    }

    const sorted = [...matched].sort((a, b) => b.priority - a.priority);
    return sorted[0].effect;
  }
}

export default PriorityStrategy;
