import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import { AbilityJSONParser } from '../../src/parsers/json/AbilityJSONParser';
import AbilityResolver from '../../src/core/AbilityResolver';
import AbilityRule from '../../src/core/AbilityRule';
import AbilityRuleSet from '../../src/core/AbilityRuleSet';
import AbilityPolicy from '../../src/core/AbilityPolicy';
import { AbilityCompare } from '../../src/core/AbilityCompare';
import { AbilityMatch } from '../../src/core/AbilityMatch';
import { AbilityPolicyEffect } from '../../src/core/AbilityPolicyEffect';
import { AbilityTypeGenerator } from '../../src/core/AbilityTypeGenerator';
import { AbilityError } from '../../src/core/AbilityError';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';
import PermitOverridesStrategy from '../../src/strategy/PermitOverridesStrategy';

const parse = (dsl: string) => new AbilityDSLParser(dsl).parse();

describe('Core fixes', () => {
  describe('Permission wildcards', () => {
    it.each([
      ['*.create', 'order.create', true],
      ['*.create', 'user.create', true],
      ['*.create', 'order.update', false],
      ['*.create', 'order.item.create', false],
      ['order.*', 'order.create', true],
      ['order.*', 'order.item.update', true],
      ['order.*', 'user.create', false],
      ['order.*.read', 'order.item.read', true],
      ['order.*.read', 'order.item.update', false],
      ['*', 'anything.at.all', true],
      ['order.update', 'order.update', true],
      ['order.update', 'order.update.extra', false],
      ['order.update.extra', 'order.update', false],
    ])('policy `%s` vs key `%s` → %s', (policyKey, key, expected) => {
      expect(AbilityResolver.matchPermissions(policyKey.split('.'), key.split('.'))).toBe(
        expected,
      );
    });

    it('should not apply `*.create` policy to `order.update`', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.*.create if all:
          always
        `),
        DenyOverridesStrategy,
      );

      expect(resolver.resolve('order.create', {}).isAllowed()).toBeTruthy();
      expect(resolver.resolve('order.update', {}).isAllowed()).toBeFalsy();
    });
  });

  describe('Tags', () => {
    const policies = parse(`
    @tags admin
    permit permission.a if all:
      user.role equals 'admin'

    @tags user
    permit permission.a if all:
      user.role equals 'user'

    permit permission.a if all:
      user.role equals 'guest'
    `);

    it('should always use policies without tags', () => {
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy, { tags: ['admin'] });

      expect(resolver.resolve('a', { user: { role: 'admin' } }).isAllowed()).toBeTruthy();
      expect(resolver.resolve('a', { user: { role: 'guest' } }).isAllowed()).toBeTruthy();
      expect(resolver.resolve('a', { user: { role: 'user' } }).isAllowed()).toBeFalsy();
    });
  });

  describe('Callbacks', () => {
    const policies = parse(`
    permit permission.allowed if all:
      always

    deny permission.denied if all:
      always
    `);

    it('should not call the resolver callbacks in resolve()', () => {
      const onDeny = jest.fn(() => {
        throw new Error('custom');
      });
      const onAllow = jest.fn();
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy, { onDeny, onAllow });

      expect(resolver.resolve('denied', {}).isDenied()).toBeTruthy();
      expect(resolver.resolve('allowed', {}).isAllowed()).toBeTruthy();
      expect(onDeny).not.toHaveBeenCalled();
      expect(onAllow).not.toHaveBeenCalled();
    });

    it('should call the resolver onDeny in enforce() and throw its error', () => {
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy, {
        onDeny: () => {
          throw new AbilityError('custom denied');
        },
      });

      expect(() => resolver.enforce('denied', {})).toThrow('custom denied');
    });

    it('should call the local onDeny before the resolver onDeny', () => {
      const calls: string[] = [];
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy, {
        onDeny: () => {
          calls.push('global');
        },
      });

      expect(() =>
        resolver.enforce('denied', {}, undefined, {
          onDeny: () => {
            calls.push('local');
          },
        }),
      ).toThrow(AbilityError);
      expect(calls).toEqual(['local', 'global']);
    });

    it('should call onAllow in enforce()', () => {
      const globalOnAllow = jest.fn();
      const localOnAllow = jest.fn();
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy, {
        onAllow: globalOnAllow,
      });

      resolver.enforce('allowed', {}, undefined, { onAllow: localOnAllow });

      expect(localOnAllow).toHaveBeenCalledTimes(1);
      expect(globalOnAllow).toHaveBeenCalledTimes(1);
      expect(localOnAllow.mock.calls[0][0].isAllowed()).toBeTruthy();
    });
  });

  describe('Explain', () => {
    it('should keep the explanation of the result after the next resolve', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.a if all:
          user.x equals 1
        `),
        DenyOverridesStrategy,
      );

      const first = resolver.resolve('a', { user: { x: 1 } });
      const before = first.explainToString();
      resolver.resolve('a', { user: { x: 2 } });

      expect(first.explainToString()).toBe(before);
      expect(first.explainToJSON().policies[0].match).toBe(AbilityMatch.match);
    });

    it('should mark rules that were not evaluated as skipped', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.a if all:
          user.x equals 1
          user.y equals 1
        `),
        DenyOverridesStrategy,
      );

      resolver.resolve('a', { user: { x: 1, y: 1 } });
      const result = resolver.resolve('a', { user: { x: 2, y: 1 } });
      const [x, y] = result.explainToJSON().policies[0].children[0].children;

      expect(x.match).toBe(AbilityMatch.mismatch);
      expect(y.match).toBe(AbilityMatch.pending);
      expect(result.explainToString()).toContain('SKIPPED');
    });

    it('should mark the policy cancelled by except block', () => {
      const resolver = new AbilityResolver(
        parse(`
        deny permission.a if all:
          all of:
            order.status equals 'done'
          except any of:
            user.role equals 'admin'
        `),
        DenyOverridesStrategy,
      );

      const result = resolver.resolve('a', { order: { status: 'done' }, user: { role: 'admin' } });
      const text = result.explainToString();

      expect(result.explainToJSON().policies[0].match).toBe(AbilityMatch.exceptMismatch);
      expect(text).toContain('[EXCEPT ✗]');
      expect(text).not.toContain('DISABLED');
    });

    it('should mark disabled policy as disabled', () => {
      const resolver = new AbilityResolver(
        parse(`
        @disabled
        permit permission.a if all:
          always
        `),
        DenyOverridesStrategy,
      );

      expect(resolver.resolve('a', {}).explainToJSON().policies[0].match).toBe(
        AbilityMatch.disabled,
      );
    });

    it('should show a literal in quotes and a path without quotes', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.a if all:
          user.a equals 'user.b'
          user.a equals user.b
        `),
        DenyOverridesStrategy,
      );

      const text = resolver.resolve('a', { user: { a: 'user.b' } }).explainToString();

      expect(text).toContain('(user.a = "user.b")');
      expect(text).toContain('(user.a = user.b)');
    });

    it('should explain the decisive policy with the snapshot', () => {
      const resolver = new AbilityResolver(
        parse(`
        @name decisive
        deny permission.a if all:
          user.x equals 1
        `),
        DenyOverridesStrategy,
      );

      const result = resolver.resolve('a', { user: { x: 1 } });
      resolver.resolve('a', { user: { x: 2 } });

      expect(result.explainDecisive()).toContain('[MATCH ✓]');
    });
  });

  describe('Empty groups and policies', () => {
    it('should not match the policy if all the rules are disabled', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.a if all:
          @disabled
          user.x equals 1
        `),
        DenyOverridesStrategy,
      );

      expect(resolver.resolve('a', { user: {} }).isAllowed()).toBeFalsy();
    });

    it('should ignore the group with disabled rules only', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.a if all:
          all of:
            @disabled
            user.x equals 1
          all of:
            user.y equals 1
        `),
        DenyOverridesStrategy,
      );

      expect(resolver.resolve('a', { user: { y: 1 } }).isAllowed()).toBeTruthy();
      expect(resolver.resolve('a', { user: { y: 2 } }).isAllowed()).toBeFalsy();
    });

    it('should not match the policy without rule sets', () => {
      const policy = new AbilityPolicy({
        id: null,
        name: null,
        permission: 'a',
        effect: AbilityPolicyEffect.permit,
      });

      expect(policy.check({})).toBe(AbilityMatch.mismatch);
    });

    it('should not match the policy with except rule sets only', () => {
      const policy = new AbilityPolicy({
        id: null,
        name: null,
        permission: 'a',
        effect: AbilityPolicyEffect.permit,
      }).addRuleSet(
        new AbilityRuleSet({ compareMethod: AbilityCompare.or, isExcept: true }).addRule(
          AbilityRule.equals('user.x', 1),
        ),
      );

      expect(policy.check({ user: { x: 2 } })).toBe(AbilityMatch.mismatch);
    });
  });

  describe('Generated IDs', () => {
    it('should generate different IDs for policies with different rules', () => {
      const [a, b] = parse(`
      permit permission.a if all:
        user.x equals 1

      permit permission.a if all:
        user.y equals 2
      `);

      expect(a.id).not.toBe(b.id);
      expect(a.ruleSet[0].id).not.toBe(b.ruleSet[0].id);
    });

    it('should generate the same ID for the same content', () => {
      const dsl = `
      permit permission.a if all:
        user.x equals 1
      `;

      expect(parse(dsl)[0].id).toBe(parse(dsl)[0].id);
    });

    it('should distinguish a literal and a path in the rule ID', () => {
      const [policy] = parse(`
      permit permission.a if all:
        user.a equals 'user.b'
        user.a equals user.b
      `);

      const [literal, path] = policy.ruleSet[0].rules;
      expect(literal.id).not.toBe(path.id);
    });

    it('should use the name as ID fallback', () => {
      const rule = AbilityRule.equals('user.x', 1);
      expect(rule.name).toBe(rule.id);
      rule.name = 'custom';
      expect(rule.name).toBe('custom');
    });
  });

  describe('copyWith', () => {
    it('should keep disabled flag and resource type of the rule', () => {
      const rule = new AbilityRule({
        subject: 'user.a',
        resource: 'user.b',
        resourceType: 'value',
        condition: AbilityRule.equals('a', 1).condition,
        disabled: true,
      });

      const copy = rule.copyWith({ name: 'copy' });

      expect(copy.disabled).toBe(true);
      expect(copy.resourceType).toBe('value');
      expect(copy.name).toBe('copy');
      expect(copy.id).toBe(rule.id);
    });

    it('should keep except and disabled flags of the rule set', () => {
      const ruleSet = new AbilityRuleSet({
        compareMethod: AbilityCompare.or,
        isExcept: true,
        disabled: true,
      }).addRule(AbilityRule.equals('user.x', 1));

      const copy = ruleSet.copyWith({ name: 'copy' });

      expect(copy.isExcept).toBe(true);
      expect(copy.disabled).toBe(true);
      expect(copy.rules).toHaveLength(1);
    });

    it('should keep tags and disabled flag of the policy', () => {
      const [policy] = parse(`
      @tags admin, user
      @disabled
      permit permission.a if all:
        always
      `);

      const copy = policy.copyWith({ name: 'copy' });

      expect(copy.tags).toEqual(['admin', 'user']);
      expect(copy.disabled).toBe(true);
      expect(copy.name).toBe('copy');
      expect(copy.id).toBe(policy.id);
    });
  });

  describe('JSON', () => {
    const dsl = `
    @description "Policy description"
    deny permission.a if all:
      @name "main"
      @description "Group description"
      all of:
        @description "Rule description"
        order.status equals 'done'
      except any of:
        user.role equals 'admin'
    `;

    it('should keep the except block after JSON round trip', () => {
      const json = AbilityJSONParser.toJSON(parse(dsl));
      const policies = AbilityJSONParser.parse(JSON.parse(JSON.stringify(json)));
      const resolver = new AbilityResolver(policies, PermitOverridesStrategy);
      const resource = { order: { status: 'done' }, user: { role: 'admin' } };

      expect(json[0].ruleSet[1].isExcept).toBe(true);
      expect(policies[0].ruleSet[1].isExcept).toBe(true);
      expect(resolver.resolve('a', resource).decisive()).toBeNull();
      expect(
        resolver.resolve('a', { ...resource, user: { role: 'user' } }).decisive()?.effect,
      ).toBe(AbilityPolicyEffect.deny);
    });

    it('should keep descriptions after JSON round trip', () => {
      const policies = AbilityJSONParser.parse(AbilityJSONParser.toJSON(parse(dsl)));

      expect(policies[0].description).toBe('Policy description');
      expect(policies[0].ruleSet[0].description).toBe('Group description');
      expect(policies[0].ruleSet[0].rules[0].description).toBe('Rule description');
    });
  });

  describe('Null and undefined', () => {
    const dsl = `
    permit permission.a if all:
      user.token is not null
    `;

    it('should treat `is not null` strictly: undefined is not null', () => {
      const resolver = new AbilityResolver(parse(dsl), DenyOverridesStrategy);

      expect(resolver.resolve('a', { user: { token: 'x' } }).isAllowed()).toBeTruthy();
      expect(resolver.resolve('a', { user: { token: null } }).isAllowed()).toBeFalsy();
      expect(resolver.resolve('a', { user: {} }).isAllowed()).toBeTruthy();
    });

    it('should check the presence of the value with `is defined` and `is not null` together', () => {
      const resolver = new AbilityResolver(
        parse(`
        permit permission.a if all:
          user.token is defined
          user.token is not null
        `),
        DenyOverridesStrategy,
      );

      expect(resolver.resolve('a', { user: { token: 'x' } }).isAllowed()).toBeTruthy();
      expect(resolver.resolve('a', { user: { token: null } }).isAllowed()).toBeFalsy();
      expect(resolver.resolve('a', { user: {} }).isAllowed()).toBeFalsy();
    });
  });

  describe('Types generation', () => {
    it('should merge the fields of wildcard policies into concrete permissions', () => {
      const typeDefs = new AbilityTypeGenerator(
        parse(`
        permit permission.order.* if all:
          user.role equals 'manager'

        permit permission.order.update if all:
          order.id equals 1

        permit permission.*.create if all:
          user.active is true

        permit permission.order.create if all:
          order.total > 0
        `),
      ).generateTypeDefs();

      // keys are sorted: *.create, order.create, order.update
      const create = typeDefs.slice(
        typeDefs.indexOf("['order.create']"),
        typeDefs.indexOf("['order.update']"),
      );
      const update = typeDefs.slice(
        typeDefs.indexOf("['order.update']"),
        typeDefs.indexOf('export type PolicyTags'),
      );

      expect(update).toContain('readonly role?: string');
      expect(update).not.toContain('active');
      expect(create).toContain('readonly role?: string');
      expect(create).toContain('readonly active?: boolean');
      expect(create).toContain('readonly total?: number');
    });

    it('should type the path on the right side as unknown', () => {
      const typeDefs = new AbilityTypeGenerator(
        parse(`
        permit permission.a if all:
          document.author equals user.id
          document.version equals '1.2.3'
        `),
      ).generateTypeDefs();

      expect(typeDefs).toContain('readonly version?: string | null | undefined');
      expect(typeDefs).toContain('readonly author?: unknown');
      expect(typeDefs).toContain('readonly id?: unknown');
    });
  });
});
