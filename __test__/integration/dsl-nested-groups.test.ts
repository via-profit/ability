import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import { AbilityDSLSyntaxError } from '../../src/parsers/dsl/AbilityDSLSyntaxError';
import { AbilityJSONParser } from '../../src/parsers/json/AbilityJSONParser';
import AbilityResolver from '../../src/core/AbilityResolver';
import AbilityRule from '../../src/core/AbilityRule';
import AbilityRuleSet from '../../src/core/AbilityRuleSet';
import AbilityPolicy from '../../src/core/AbilityPolicy';
import { AbilityCompare } from '../../src/core/AbilityCompare';
import { AbilityMatch } from '../../src/core/AbilityMatch';
import { AbilityPolicyEffect } from '../../src/core/AbilityPolicyEffect';
import { AbilityTypeGenerator } from '../../src/core/AbilityTypeGenerator';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

const parse = (dsl: string) => new AbilityDSLParser(dsl).parse();

type IssueResource = {
  user: { roles: string[] };
  project: { isMember: boolean };
  issue: { isClosed: boolean; isAssignee: boolean; isCreator: boolean };
};

const resource = (
  roles: string[],
  issue: Partial<IssueResource['issue']> = {},
  isMember = true,
): IssueResource => ({
  user: { roles },
  project: { isMember },
  issue: { isClosed: false, isAssignee: false, isCreator: false, ...issue },
});

// The shape of the rule found in the CRM: `any of` nested in `all of`
const ISSUE_UPDATE_DSL = `
permit permission.issue.update if any:
  user.roles contains 'administrator'
  user.roles contains 'manager'
  @name "Assignee or author of an open issue of his project"
  all of:
    user.roles contains 'programmer'
    project.isMember is equals true
    issue.isClosed is equals false
    any of:
      issue.isAssignee is equals true
      issue.isCreator is equals true
`;

const isAllowed = (dsl: string, permission: string, res: object) =>
  new AbilityResolver(parse(dsl), DenyOverridesStrategy).resolve(permission, res).isAllowed();

describe('Nested groups', () => {
  describe('DSL parsing', () => {
    it('should put an indented group into the enclosing group', () => {
      const [policy] = parse(ISSUE_UPDATE_DSL);

      // implicit group (administrator / manager) + `all of`
      expect(policy.ruleSet).toHaveLength(2);

      const allOf = policy.ruleSet[1];
      expect(allOf.compareMethod).toBe(AbilityCompare.and);
      expect(allOf.rules).toHaveLength(3);
      expect(allOf.ruleSets).toHaveLength(1);

      const anyOf = allOf.ruleSets[0];
      expect(anyOf.compareMethod).toBe(AbilityCompare.or);
      expect(anyOf.rules.map(r => r.subject)).toEqual(['issue.isAssignee', 'issue.isCreator']);
    });

    it('should keep a group on the same indentation as a sibling', () => {
      const [policy] = parse(`
      permit permission.a if all:
        all of:
          user.a is equals 1
        any of:
          user.b is equals 1
          user.c is equals 1
      `);

      expect(policy.ruleSet).toHaveLength(2);
      expect(policy.ruleSet[0].ruleSets).toHaveLength(0);
      expect(policy.ruleSet[1].ruleSets).toHaveLength(0);
    });

    it('should return rules after a nested group to the parent group', () => {
      const [policy] = parse(`
      permit permission.a if any:
        all of:
          user.a is equals 1
          any of:
            user.b is equals 1
            user.c is equals 1
          user.d is equals 1
        all of:
          user.e is equals 1
      `);

      expect(policy.ruleSet).toHaveLength(2);
      expect(policy.ruleSet[0].rules.map(r => r.subject)).toEqual(['user.a', 'user.d']);
      expect(policy.ruleSet[0].ruleSets[0].rules.map(r => r.subject)).toEqual(['user.b', 'user.c']);
      expect(policy.ruleSet[1].rules.map(r => r.subject)).toEqual(['user.e']);
    });

    it('should apply annotations to nested groups and their rules', () => {
      const [policy] = parse(`
      permit permission.a if all:
        @name "outer"
        all of:
          user.a is equals 1
          @name "inner"
          @id inner-id
          any of:
            @name "rule-b"
            user.b is equals 1
            user.c is equals 1
        @name "next"
        all of:
          user.d is equals 1
      `);

      const inner = policy.ruleSet[0].ruleSets[0];
      expect(policy.ruleSet[0].name).toBe('outer');
      expect(inner.name).toBe('inner');
      expect(inner.id).toBe('inner-id');
      expect(inner.rules[0].name).toBe('rule-b');
      expect(policy.ruleSet[1].name).toBe('next');
    });

    it('should support nested groups in the except block', () => {
      const [policy] = parse(`
      deny permission.a if all:
        user.a is equals 1
        except any of:
          user.admin is equals true
          all of:
            user.owner is equals true
            user.active is equals true
      `);

      const except = policy.ruleSet[1];
      expect(except.isExcept).toBe(true);
      expect(except.ruleSets).toHaveLength(1);
      expect(except.ruleSets[0].isExcept).toBe(false);
    });

    it('should reject `except` nested in a group', () => {
      expect(() =>
        parse(`
        permit permission.a if all:
          all of:
            user.a is equals 1
            except all of:
              user.b is equals 1
        `),
      ).toThrow(AbilityDSLSyntaxError);
    });

    it('should reject an empty nested group', () => {
      expect(() =>
        parse(`
        permit permission.a if all:
          all of:
            user.a is equals 1
            any of:
          user.b is equals 1
        `),
      ).toThrow(AbilityDSLSyntaxError);
    });

    it('should accept a group which contains only nested groups', () => {
      const [policy] = parse(`
      permit permission.a if all:
        all of:
          any of:
            user.a is equals 1
          any of:
            user.b is equals 1
      `);

      expect(policy.ruleSet).toHaveLength(1);
      expect(policy.ruleSet[0].rules).toHaveLength(0);
      expect(policy.ruleSet[0].ruleSets).toHaveLength(2);
    });

    it('should give different ids to groups with different nested groups', () => {
      const [a, b] = parse(`
      permit permission.a if all:
        all of:
          user.a is equals 1
          any of:
            user.b is equals 1

      permit permission.a if all:
        all of:
          user.a is equals 1
          any of:
            user.c is equals 1
      `);

      expect(a.id).not.toBe(b.id);
      expect(a.ruleSet[0].id).not.toBe(b.ruleSet[0].id);
    });
  });

  describe('Evaluation: `any of` inside `all of` (the CRM case)', () => {
    it.each([
      ['administrator', ['administrator'], {}, true],
      ['manager', ['manager'], {}, true],
      ['programmer, assignee', ['programmer'], { isAssignee: true }, true],
      ['programmer, author', ['programmer'], { isCreator: true }, true],
      ['programmer, foreign issue', ['programmer'], {}, false],
      [
        'programmer, assignee of a closed issue',
        ['programmer'],
        { isAssignee: true, isClosed: true },
        false,
      ],
    ])('%s → %s', (_title, roles, issue, expected) => {
      expect(isAllowed(ISSUE_UPDATE_DSL, 'issue.update', resource(roles, issue))).toBe(expected);
    });

    it('programmer, assignee in a foreign project → false', () => {
      expect(
        isAllowed(
          ISSUE_UPDATE_DSL,
          'issue.update',
          resource(['programmer'], { isAssignee: true }, false),
        ),
      ).toBe(false);
    });

    it('should work with `if all:` policy', () => {
      const dsl = `
      permit permission.a if all:
        all of:
          user.role is equals 'programmer'
          any of:
            user.isAssignee is equals true
            user.isCreator is equals true
      `;

      expect(isAllowed(dsl, 'a', { user: { role: 'programmer', isAssignee: true } })).toBe(true);
      expect(isAllowed(dsl, 'a', { user: { role: 'programmer' } })).toBe(false);
      expect(isAllowed(dsl, 'a', { user: { role: 'guest', isCreator: true } })).toBe(false);
    });
  });

  describe('Evaluation: `all of` inside `any of`', () => {
    const dsl = `
    permit permission.a if all:
      any of:
        user.role is equals 'admin'
        all of:
          user.role is equals 'programmer'
          user.isOwner is equals true
    `;

    it.each([
      [{ role: 'admin' }, true],
      [{ role: 'programmer', isOwner: true }, true],
      [{ role: 'programmer' }, false],
      [{ role: 'guest', isOwner: true }, false],
    ])('%j → %s', (user, expected) => {
      expect(isAllowed(dsl, 'a', { user })).toBe(expected);
    });
  });

  describe('Evaluation: deeper nesting', () => {
    // a AND (b OR (c AND (d OR e)))
    const dsl = `
    permit permission.a if any:
      all of:
        user.a is equals true
        any of:
          user.b is equals true
          all of:
            user.c is equals true
            any of:
              user.d is equals true
              user.e is equals true
    `;

    const expected = (u: Record<string, boolean>) => u.a && (u.b || (u.c && (u.d || u.e)));
    const keys = ['a', 'b', 'c', 'd', 'e'];
    const combinations = Array.from({ length: 32 }, (_, n) =>
      Object.fromEntries(keys.map((k, i) => [k, Boolean(n & (1 << i))])),
    );

    it.each(combinations)('%j', user => {
      expect(isAllowed(dsl, 'a', { user })).toBe(Boolean(expected(user)));
    });

    it('should survive the JSON round-trip', () => {
      const policies = AbilityJSONParser.parse(
        JSON.parse(JSON.stringify(AbilityJSONParser.toJSON(parse(dsl)))),
      );
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

      combinations.forEach(user => {
        expect(resolver.resolve('a', { user }).isAllowed()).toBe(Boolean(expected(user)));
      });
    });
  });

  describe('Evaluation: deny effect', () => {
    const dsl = `
    permit permission.order.update if all:
      always

    deny permission.order.update if all:
      all of:
        order.status is equals 'completed'
        any of:
          user.role is equals 'manager'
          user.role is equals 'guest'
    `;

    it.each([
      ['completed', 'manager', false],
      ['completed', 'guest', false],
      ['completed', 'admin', true],
      ['new', 'manager', true],
    ])('status %s, role %s → %s', (status, role, expected) => {
      expect(isAllowed(dsl, 'order.update', { order: { status }, user: { role } })).toBe(expected);
    });
  });

  describe('Evaluation: except with a nested group', () => {
    const dsl = `
    permit permission.order.update if all:
      always

    deny permission.order.update if all:
      order.status is equals 'completed'
      except any of:
        user.role is equals 'admin'
        all of:
          user.role is equals 'manager'
          user.isSenior is equals true
    `;

    it.each([
      [{ role: 'admin' }, true],
      [{ role: 'manager', isSenior: true }, true],
      [{ role: 'manager' }, false],
      [{ role: 'guest', isSenior: true }, false],
    ])('%j → %s', (user, expected) => {
      expect(isAllowed(dsl, 'order.update', { order: { status: 'completed' }, user })).toBe(
        expected,
      );
    });
  });

  describe('Disabled nested groups', () => {
    it('should skip a disabled nested group', () => {
      const dsl = `
      permit permission.a if all:
        all of:
          user.a is equals true
          @disabled
          any of:
            user.b is equals true
      `;

      expect(isAllowed(dsl, 'a', { user: { a: true } })).toBe(true);
      expect(isAllowed(dsl, 'a', { user: { a: false } })).toBe(false);
    });

    it('should skip a group whose rules and nested groups are all disabled', () => {
      const dsl = `
      permit permission.a if any:
        all of:
          @disabled
          user.a is equals true
          @disabled
          any of:
            user.b is equals true
        all of:
          user.c is equals true
      `;

      expect(isAllowed(dsl, 'a', { user: { c: true } })).toBe(true);
      expect(isAllowed(dsl, 'a', { user: { a: true, b: true } })).toBe(false);
    });
  });

  describe('Programmatic API', () => {
    const build = () =>
      new AbilityPolicy({
        id: 'p',
        name: 'p',
        permission: 'issue.update',
        effect: AbilityPolicyEffect.permit,
        compareMethod: AbilityCompare.or,
      }).addRuleSet(
        AbilityRuleSet.and([
          AbilityRule.contains('user.roles', 'programmer'),
          AbilityRule.equals('issue.isClosed', false),
        ]).addRuleSet(
          AbilityRuleSet.or([
            AbilityRule.equals('issue.isAssignee', true),
            AbilityRule.equals('issue.isCreator', true),
          ]),
        ),
      );

    it('should evaluate nested rule sets', () => {
      const policy = build();

      expect(policy.check(resource(['programmer'], { isAssignee: true }))).toBe(AbilityMatch.match);
      expect(policy.check(resource(['programmer']))).toBe(AbilityMatch.mismatch);
    });

    it('should keep nested rule sets in copyWith', () => {
      const copy = build().ruleSet[0].copyWith({ name: 'copy' });

      expect(copy.ruleSets).toHaveLength(1);
      expect(copy.check(resource(['programmer']))).toBe(AbilityMatch.mismatch);
    });

    it('should extract rules of nested rule sets', () => {
      expect(
        build()
          .extractRules()
          .map(r => r.subject),
      ).toEqual(['user.roles', 'issue.isClosed', 'issue.isAssignee', 'issue.isCreator']);
    });
  });

  describe('JSON', () => {
    it('should serialize nested groups and parse them back', () => {
      const json = AbilityJSONParser.toJSON(parse(ISSUE_UPDATE_DSL));
      const allOf = json[0].ruleSet[1];

      expect(allOf.ruleSets).toHaveLength(1);
      expect(allOf.ruleSets?.[0].compareMethod).toBe(AbilityCompare.or);
      expect(allOf.ruleSets?.[0].rules).toHaveLength(2);

      const policies = AbilityJSONParser.parse(JSON.parse(JSON.stringify(json)));
      expect(AbilityJSONParser.toJSON(policies)).toEqual(json);
      expect(policies[0].id).toBe(parse(ISSUE_UPDATE_DSL)[0].id);

      const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
      expect(resolver.resolve('issue.update', resource(['programmer'])).isAllowed()).toBe(false);
      expect(
        resolver.resolve('issue.update', resource(['programmer'], { isCreator: true })).isAllowed(),
      ).toBe(true);
    });

    it('should not add `ruleSets` to flat groups', () => {
      const json = AbilityJSONParser.toJSON(
        parse(`
        permit permission.a if all:
          user.a is equals 1
        `),
      );

      expect(json[0].ruleSet[0]).not.toHaveProperty('ruleSets');
    });
  });

  describe('Explain', () => {
    it('should show nested groups in explain', () => {
      const resolver = new AbilityResolver(parse(ISSUE_UPDATE_DSL), DenyOverridesStrategy);
      const result = resolver.resolve(
        'issue.update',
        resource(['programmer'], { isCreator: true }),
      );
      const [policy] = result.explainToJSON().policies;

      const allOf = policy.children[1];
      const nested = allOf.children.find(c => c.type === 'ruleSet');

      expect(nested).toBeDefined();
      expect(nested?.match).toBe(AbilityMatch.match);
      expect(result.explainToString()).toContain('issue.isCreator');
    });

    it('should keep nested states in the result after the next resolve', () => {
      const resolver = new AbilityResolver(parse(ISSUE_UPDATE_DSL), DenyOverridesStrategy);
      const first = resolver.resolve('issue.update', resource(['programmer'], { isCreator: true }));
      const before = first.explainToString();

      resolver.resolve('issue.update', resource(['programmer']));

      expect(first.explainToString()).toBe(before);
    });
  });

  describe('Type generator', () => {
    it('should include fields of nested groups', () => {
      const types = new AbilityTypeGenerator(parse(ISSUE_UPDATE_DSL)).generateTypeDefs();

      expect(types).toContain('isAssignee');
      expect(types).toContain('isCreator');
    });
  });
});
