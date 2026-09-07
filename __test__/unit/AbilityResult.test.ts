import { AbilityMatch } from '../../src/core/AbilityMatch';
import AbilityPolicy from '../../src/core/AbilityPolicy';
import { AbilityPolicyEffect } from '../../src/core/AbilityPolicyEffect';
import { AbilityResult } from '../../src/core/AbilityResult';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

describe('AbilityResult', () => {
  it('should delegate explain to explainToString', () => {
    const policy = new AbilityPolicy({
      id: 'policy-id',
      name: 'Policy',
      permission: 'orders.read',
      effect: AbilityPolicyEffect.permit,
    });
    policy.matchState = AbilityMatch.match;

    const strategy = new DenyOverridesStrategy([policy]);
    const result = new AbilityResult('orders.read', strategy.evaluate(), strategy);

    expect(result.explain()).toBe(result.explainToString());
  });

  it('should return the result and policy explanations as JSON', () => {
    const policy = new AbilityPolicy({
      id: 'policy-id',
      name: 'Policy',
      permission: 'orders.read',
      effect: AbilityPolicyEffect.permit,
    });
    policy.matchState = AbilityMatch.match;

    const strategy = new DenyOverridesStrategy([policy]);
    const result = new AbilityResult('orders.read', strategy.evaluate(), strategy);

    expect(result.explainToJSON()).toEqual({
      permission: 'orders.read',
      effect: AbilityPolicyEffect.permit,
      policies: [
        {
          type: 'policy',
          name: '<permit> Policy',
          match: AbilityMatch.match,
          children: [],
        },
      ],
    });
  });
});
