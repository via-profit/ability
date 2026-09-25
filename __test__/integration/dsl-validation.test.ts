import { AbilityDSLParser, AbilityDSLSyntaxError, AbilityDSLLexer } from '../../src';
import AbilityResolver from '../../src/core/AbilityResolver';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';
import { AbilityCondition } from '../../src/core/AbilityCondition';

const parse = (dsl: string) => new AbilityDSLParser(dsl).parse();

describe('DSL parse-time validation', () => {
  describe('Exports', () => {
    it('should export AbilityDSLSyntaxError from the package entry', () => {
      expect(AbilityDSLSyntaxError).toBeDefined();

      let error: unknown = null;
      try {
        parse(`permit permission.test if all:\n  user.a equals`);
      } catch (err) {
        error = err;
      }

      expect(error).toBeInstanceOf(AbilityDSLSyntaxError);
      expect((error as AbilityDSLSyntaxError).line).toBeGreaterThan(0);
    });

    it('should throw AbilityDSLSyntaxError from the lexer with the position', () => {
      let error: unknown = null;
      try {
        new AbilityDSLLexer(`permit permission.test if all:\n  user.a equals $`).tokenize();
      } catch (err) {
        error = err;
      }

      expect(error).toBeInstanceOf(AbilityDSLSyntaxError);
      expect((error as AbilityDSLSyntaxError).line).toBe(2);
      expect((error as AbilityDSLSyntaxError).column).toBe(17);
    });

    it('should throw AbilityDSLSyntaxError on unterminated string', () => {
      expect(() => parse(`permit permission.test if all:\n  user.a equals 'abc`)).toThrow(
        AbilityDSLSyntaxError,
      );
    });
  });

  describe('Annotations', () => {
    it('should throw on unknown annotation and suggest the similar one', () => {
      expect(() =>
        parse(`
        @diasbled true
        permit permission.test if all:
          always
        `),
      ).toThrow(/Unknown annotation `@diasbled`. Did you mean `@disabled`\?/);
    });

    it('should throw on duplicate annotation', () => {
      expect(() =>
        parse(`
        @tags a, b
        @tags c
        permit permission.test if all:
          always
        `),
      ).toThrow(/Duplicate annotation `@tags`/);
    });

    it.each(['@id', '@name', '@description', '@priority', '@tags'])(
      'should throw if %s has no value',
      annotation => {
        expect(() =>
          parse(`
          ${annotation}
          permit permission.test if all:
            always
          `),
        ).toThrow(/requires a value/);
      },
    );

    it('should throw on non-integer priority', () => {
      expect(() =>
        parse(`
        @priority high
        permit permission.test if all:
          always
        `),
      ).toThrow(/expects an integer/);

      expect(() =>
        parse(`
        @priority 1.5
        permit permission.test if all:
          always
        `),
      ).toThrow(/expects an integer/);
    });

    it('should keep priority 0 and negative priority', () => {
      const policies = parse(`
      @priority 0
      permit permission.a if all:
        always

      @priority -5
      permit permission.b if all:
        always
      `);

      expect(policies[0].priority).toBe(0);
      expect(policies[1].priority).toBe(-5);
    });

    it('should throw on invalid @disabled value', () => {
      expect(() =>
        parse(`
        @disabled yes
        permit permission.test if all:
          always
        `),
      ).toThrow(/expects `true` or `false`/);
    });

    it('should parse @disabled without value as true and `false` as false', () => {
      const policies = parse(`
      @disabled
      permit permission.a if all:
        always

      @disabled false
      permit permission.b if all:
        always
      `);

      expect(policies[0].disabled).toBe(true);
      expect(policies[1].disabled).toBe(false);
    });

    it('should throw on empty tag', () => {
      expect(() =>
        parse(`
        @tags admin,,user
        permit permission.test if all:
          always
        `),
      ).toThrow(/empty tag/);
    });

    it.each(['["admin"]', 'admin user', '[admin, user]'])('should throw on invalid tags `%s`', tags => {
      expect(() =>
        parse(`
        @tags ${tags}
        permit permission.test if all:
          always
        `),
      ).toThrow(/Invalid tag/);
    });

    it('should throw if the annotation is not attached to anything', () => {
      expect(() =>
        parse(`
        permit permission.test if all:
          always

        @name "orphan"
        `),
      ).toThrow(/is not attached/);
    });

    it('should throw if the annotation is not allowed on the target', () => {
      expect(() =>
        parse(`
        permit permission.test if all:
          @priority 1
          all of:
            always
        `),
      ).toThrow(/@priority is not allowed on ruleSet/);
    });

    it('should allow @description on rules', () => {
      const [policy] = parse(`
      permit permission.test if all:
        @description "Rule description"
        user.a equals 1
      `);

      expect(policy.ruleSet[0].rules[0].description).toBe('Rule description');
    });
  });

  describe('Duplicate IDs', () => {
    it('should throw on duplicate policy @id', () => {
      expect(() =>
        parse(`
        @id policy-1
        permit permission.a if all:
          always

        @id policy-1
        permit permission.b if all:
          always
        `),
      ).toThrow(/Duplicate @id `policy-1`/);
    });

    it('should throw on duplicate rule @id', () => {
      expect(() =>
        parse(`
        permit permission.a if all:
          @id rule-1
          user.a equals 1
          @id rule-1
          user.b equals 1
        `),
      ).toThrow(/Duplicate @id `rule-1`/);
    });

    it('should allow the same @id for different kinds of elements', () => {
      expect(() =>
        parse(`
        @id same
        permit permission.a if all:
          @id same
          all of:
            @id same
            user.a equals 1
        `),
      ).not.toThrow();
    });
  });

  describe('Empty policies and groups', () => {
    it('should throw on policy without rules', () => {
      expect(() =>
        parse(`
        permit permission.a if all:

        permit permission.b if all:
          always
        `),
      ).toThrow(/has no rules/);
    });

    it('should throw on policy without rules at the end of DSL', () => {
      expect(() => parse(`permit permission.a if all:`)).toThrow(/has no rules/);
    });

    it('should throw on policy with the except block only', () => {
      expect(() =>
        parse(`
        permit permission.a if all:
          except any of:
            user.a equals 1
        `),
      ).toThrow(/outside the `except` block/);
    });

    it('should throw on empty group', () => {
      expect(() =>
        parse(`
        permit permission.a if all:
          all of:
          any of:
            user.a equals 1
        `),
      ).toThrow(/The group has no rules/);
    });

    it('should throw on empty except group', () => {
      expect(() =>
        parse(`
        permit permission.a if all:
          user.a equals 1
          except any of:
        `),
      ).toThrow(/except group has no rules/);
    });

    it('should return an empty list for DSL with comments only', () => {
      expect(parse(`\n  # comment\n  # another comment\n`)).toEqual([]);
    });
  });

  describe('Aliases', () => {
    it('should throw on unknown alias', () => {
      expect(() =>
        parse(`
        permit permission.a if any:
          isAdmin
          user.a equals 1
        `),
      ).toThrow(/Unknown alias `isAdmin`/);
    });

    it('should throw if the alias is used before definition', () => {
      expect(() =>
        parse(`
        permit permission.a if any:
          isAdmin

        alias isAdmin:
          user.roles contains 'admin'
        `),
      ).toThrow(/Unknown alias `isAdmin`/);
    });

    it('should throw on duplicate alias', () => {
      expect(() =>
        parse(`
        alias isAdmin:
          user.roles contains 'admin'

        alias isAdmin:
          user.role equals 'admin'

        permit permission.a if all:
          isAdmin
        `),
      ).toThrow(/already defined/);
    });

    it('should throw if the alias contains more than one rule', () => {
      expect(() =>
        parse(`
        alias isAdmin:
          user.roles contains 'admin'
          user.active is true

        permit permission.a if all:
          isAdmin
        `),
      ).toThrow(/exactly one rule/);
    });

    it('should throw if the alias has no rule', () => {
      expect(() =>
        parse(`
        alias isAdmin:

        permit permission.a if all:
          always
        `),
      ).toThrow(/has no rule/);
    });

    it('should throw if the alias name contains dots', () => {
      expect(() =>
        parse(`
        alias user.isAdmin:
          user.roles contains 'admin'

        permit permission.a if all:
          always
        `),
      ).toThrow(/must not contain dots/);
    });

    it('should apply alias annotations and rule annotations at the usage place', () => {
      const [policy] = parse(`
      @name "Is admin"
      @description "User has the admin role"
      alias isAdmin:
        user.roles contains 'admin'

      permit permission.a if any:
        isAdmin

        @name "Admin at usage"
        isAdmin
      `);

      const [first, second] = policy.ruleSet[0].rules;
      expect(first.name).toBe('Is admin');
      expect(first.description).toBe('User has the admin role');
      expect(second.name).toBe('Admin at usage');
    });

    it('should not share the evaluation state of the alias between policies', () => {
      const policies = parse(`
      alias isAdmin:
        user.roles contains 'admin'

      permit permission.a if all:
        isAdmin

      permit permission.a if all:
        user.active is true
        isAdmin
      `);

      expect(policies[0].ruleSet[0].rules[0]).not.toBe(policies[1].ruleSet[0].rules[1]);

      const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
      const result = resolver.resolve('a', { user: { roles: ['admin'], active: false } });
      const [p1, p2] = result.explainToJSON().policies;

      // the first policy checked the alias rule, the second one stopped on the first rule
      expect(p1.children[0].children[0].match).toBe('match');
      expect(p2.children[0].children[1].match).toBe('pending');
    });
  });

  describe('Rules', () => {
    it('should allow `always` and `never` inside explicit groups and except blocks', () => {
      const [policy] = parse(`
      permit permission.a if all:
        all of:
          always
        except any of:
          never
      `);

      expect(policy.ruleSet[0].rules[0].condition).toBe(AbilityCondition.always);
      expect(policy.ruleSet[1].rules[0].condition).toBe(AbilityCondition.never);

      const resolver = new AbilityResolver([policy], DenyOverridesStrategy);
      expect(resolver.resolve('a', {}).isAllowed()).toBeTruthy();
    });

    it('should parse `less` and `greater` without `than`', () => {
      const [policy] = parse(`
      permit permission.a if all:
        user.a less 5
        user.b greater 5
      `);

      expect(policy.ruleSet[0].rules[0].condition).toBe(AbilityCondition.less_than);
      expect(policy.ruleSet[0].rules[1].condition).toBe(AbilityCondition.greater_than);
    });
  });
});
