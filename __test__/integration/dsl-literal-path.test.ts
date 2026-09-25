import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import { AbilityJSONParser } from '../../src/parsers/json/AbilityJSONParser';
import { AbilityDSLSyntaxError } from '../../src';
import AbilityResolver from '../../src/core/AbilityResolver';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';
import { AbilityCondition } from '../../src/core/AbilityCondition';

const resolve = (
  dsl: string,
  resource: Record<string, unknown> | null,
  environment?: Record<string, unknown>,
) => {
  const policies = new AbilityDSLParser(dsl).parse();
  const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

  return resolver.resolve('test', resource as never, environment as never);
};

describe('DSL literals and paths', () => {
  describe('Quoted string is always a literal', () => {
    it('should compare a quoted string with dots as a literal', () => {
      const dsl = `
      permit permission.test if all:
        domain.version is equals '1.2.3'
      `;

      expect(resolve(dsl, { domain: { version: '1.2.3' } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { domain: { version: '1.2.4' } }).isAllowed()).toBeFalsy();
    });

    it('should compare an email as a literal', () => {
      const dsl = `
      permit permission.test if all:
        user.email equals "john@example.com"
      `;

      expect(resolve(dsl, { user: { email: 'john@example.com' } }).isAllowed()).toBeTruthy();
    });

    it('should mark quoted value as value and unquoted as path', () => {
      const [policy] = new AbilityDSLParser(`
      permit permission.test if all:
        user.a equals 'user.b'
        user.a equals user.b
      `).parse();

      const [literal, path] = policy.ruleSet[0].rules;

      expect(literal.resource).toBe('user.b');
      expect(literal.resourceType).toBe('value');
      expect(path.resource).toBe('user.b');
      expect(path.resourceType).toBe('path');
    });

    it('should resolve an unquoted path to the resource value', () => {
      const dsl = `
      permit permission.test if all:
        document.author equals user.id
      `;

      expect(resolve(dsl, { document: { author: 1 }, user: { id: 1 } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { document: { author: 1 }, user: { id: 2 } }).isAllowed()).toBeFalsy();
    });

    it('should resolve an unquoted env path to the environment value', () => {
      const dsl = `
      permit permission.test if all:
        user.ip equals env.request.ip
      `;

      expect(
        resolve(dsl, { user: { ip: '127.0.0.1' } }, { request: { ip: '127.0.0.1' } }).isAllowed(),
      ).toBeTruthy();
    });
  });

  describe('Subject is always a path', () => {
    it('should resolve a subject without dots as a top-level path', () => {
      const dsl = `
      permit permission.test if all:
        role equals 'admin'
      `;

      expect(resolve(dsl, { role: 'admin' }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { role: 'user' }).isAllowed()).toBeFalsy();
    });

    it('should not take `env.*` values from the resource', () => {
      const dsl = `
      permit permission.test if all:
        env.hour less than 16
      `;

      expect(resolve(dsl, { env: { hour: 10 } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, {}, { hour: 10 }).isAllowed()).toBeTruthy();
    });
  });

  describe('Syntax errors', () => {
    it('should throw if the value is an unquoted word without dots', () => {
      expect(() =>
        new AbilityDSLParser(`
        permit permission.test if all:
          user.role equals admin
        `).parse(),
      ).toThrow(/wrap it in quotes/);
    });

    it('should throw if a path is used inside the array', () => {
      expect(() =>
        new AbilityDSLParser(`
        permit permission.test if all:
          user.role in ['admin', user.role]
        `).parse(),
      ).toThrow(AbilityDSLSyntaxError);
    });

    it('should throw on nested arrays', () => {
      expect(() =>
        new AbilityDSLParser(`
        permit permission.test if all:
          user.role in ['admin', ['user']]
        `).parse(),
      ).toThrow(/Nested arrays/);
    });
  });

  describe('Arrays', () => {
    it('should parse the array with mixed literals including null', () => {
      const [policy] = new AbilityDSLParser(`
      permit permission.test if all:
        user.value in ['foo', false, null, 1, -2, 3.5, '9.9.9']
      `).parse();

      expect(policy.ruleSet[0].rules[0].resource).toEqual([
        'foo',
        false,
        null,
        1,
        -2,
        3.5,
        '9.9.9',
      ]);
    });

    it('should match null inside the array', () => {
      const dsl = `
      permit permission.test if all:
        user.value in ['foo', null]
      `;

      expect(resolve(dsl, { user: { value: null } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { user: { value: 'bar' } }).isAllowed()).toBeFalsy();
    });
  });

  describe('JSON', () => {
    it('should keep the literal with dots after JSON round trip', () => {
      const dsl = `
      permit permission.test if all:
        domain.version is equals '1.2.3'
      `;
      const json = AbilityJSONParser.toJSON(new AbilityDSLParser(dsl).parse());
      const policies = AbilityJSONParser.parse(JSON.parse(JSON.stringify(json)));
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

      expect(json[0].ruleSet[0].rules[0].resourceType).toBe('value');
      expect(resolver.resolve('test', { domain: { version: '1.2.3' } }).isAllowed()).toBeTruthy();
    });

    it('should treat a string with dots as a path in legacy JSON without resourceType', () => {
      const policies = AbilityJSONParser.parse([
        {
          id: 'p1',
          name: 'legacy',
          permission: 'test',
          effect: 'permit' as never,
          compareMethod: 'and' as never,
          priority: -1,
          ruleSet: [
            {
              compareMethod: 'and' as never,
              rules: [
                {
                  subject: 'document.author',
                  resource: 'user.id',
                  condition: AbilityCondition.equals,
                },
              ],
            },
          ],
        },
      ]);

      expect(policies[0].ruleSet[0].rules[0].resourceType).toBe('path');

      const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
      expect(
        resolver.resolve('test', { document: { author: 7 }, user: { id: 7 } }).isAllowed(),
      ).toBeTruthy();
    });
  });
});

describe('DSL numbers', () => {
  it('should parse decimal numbers', () => {
    const [policy] = new AbilityDSLParser(`
    permit permission.test if all:
      order.total equals 10.5
      order.discount < 0.25
    `).parse();

    expect(policy.ruleSet[0].rules[0].resource).toBe(10.5);
    expect(policy.ruleSet[0].rules[1].resource).toBe(0.25);
  });

  it('should parse negative numbers', () => {
    const [policy] = new AbilityDSLParser(`
    permit permission.test if all:
      account.balance >= -100
      account.delta equals -0.5
    `).parse();

    expect(policy.ruleSet[0].rules[0].resource).toBe(-100);
    expect(policy.ruleSet[0].rules[1].resource).toBe(-0.5);
  });

  it('should compare decimal and negative numbers', () => {
    const dsl = `
    permit permission.test if all:
      account.balance greater than or equal -100
      account.rate less than 1.5
    `;

    expect(resolve(dsl, { account: { balance: -99.9, rate: 1.49 } }).isAllowed()).toBeTruthy();
    expect(resolve(dsl, { account: { balance: -100.1, rate: 1 } }).isAllowed()).toBeFalsy();
    expect(resolve(dsl, { account: { balance: 0, rate: 1.5 } }).isAllowed()).toBeFalsy();
  });

  it('should parse negative number right after the symbol operator', () => {
    const [policy] = new AbilityDSLParser(`
    permit permission.test if all:
      account.balance>-1
    `).parse();

    expect(policy.ruleSet[0].rules[0].condition).toBe(AbilityCondition.greater_than);
    expect(policy.ruleSet[0].rules[0].resource).toBe(-1);
  });

  it('should throw on a minus sign without a number', () => {
    expect(() =>
      new AbilityDSLParser(`
      permit permission.test if all:
        account.balance > -
      `).parse(),
    ).toThrow(AbilityDSLSyntaxError);
  });

  it('should throw on a number followed by letters', () => {
    expect(() =>
      new AbilityDSLParser(`
      permit permission.test if all:
        account.balance > 10abc
      `).parse(),
    ).toThrow(/Invalid number/);
  });

  it('should throw on a number with a trailing dot', () => {
    expect(() =>
      new AbilityDSLParser(`
      permit permission.test if all:
        account.balance > 10.
      `).parse(),
    ).toThrow(AbilityDSLSyntaxError);
  });
});
