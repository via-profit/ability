import { AbilityResolver } from '../../src/core/AbilityResolver';
import { AbilityResult } from '../../src/core/AbilityResult';
import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

const policies = new AbilityDSLParser(`
  @name blocked user
  deny permission.orders.read if all:
    user.blocked equals true

  @name active user
  permit permission.orders.read if all:
    user.active equals true
`).parse();

describe('AbilityResult explanations through AbilityResolver.enforce', () => {
  const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

  it('should expose the string explanation through onDeny', () => {
    let receivedResult: AbilityResult | undefined;

    expect(() =>
      resolver.enforce(
        'orders.read',
        { user: { blocked: true, active: false } },
        undefined,
        {
          onDeny: result => {
            receivedResult = result;

            expect(result.explain()).toBe(result.explainToString());
            expect(result.explainToString()).toContain('== orders.read DENIED==');
            expect(result.explainToString()).toContain('blocked user');
          },
        },
      ),
    ).toThrow('Permission denied');

    expect(receivedResult).toBeDefined();
  });

  it('should expose the JSON explanation through onDeny', () => {
    let receivedResult: AbilityResult | undefined;

    expect(() =>
      resolver.enforce(
        'orders.read',
        { user: { blocked: true, active: false } },
        undefined,
        {
          onDeny: result => {
            receivedResult = result;

            const explanation = result.explainToJSON();

            expect(explanation.permission).toBe('orders.read');
            expect(explanation.effect).toBe('deny');
            expect(explanation.policies).toHaveLength(2);
            expect(explanation.policies.map(policy => policy.name)).toEqual([
              '<deny> blocked user',
              '<permit> active user',
            ]);
            expect(JSON.stringify(explanation)).toBe(JSON.stringify(result.explainToJSON()));
          },
        },
      ),
    ).toThrow('Permission denied');

    expect(receivedResult).toBeDefined();
  });
});
