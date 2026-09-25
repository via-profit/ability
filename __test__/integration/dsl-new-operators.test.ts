import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import AbilityResolver from '../../src/core/AbilityResolver';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';
import { AbilityCondition, fromLiteral, toLiteral } from '../../src/core/AbilityCondition';
import { AbilityCompare } from '../../src/core/AbilityCompare';
import { AbilityJSONParser } from '../../src/parsers/json/AbilityJSONParser';
import { AbilityTypeGenerator } from '../../src/core/AbilityTypeGenerator';

const resolve = (dsl: string, resource: Record<string, unknown>) => {
  const policies = new AbilityDSLParser(dsl).parse();
  const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

  return resolver.resolve('test', resource as never);
};

const conditionOf = (dsl: string) => new AbilityDSLParser(dsl).parse()[0].ruleSet[0].rules[0];

describe('DSL new operators', () => {
  describe('Operator "is empty"', () => {
    const dsl = `
    permit permission.test if all:
      user.tags is empty
    `;

    it('should parse without value', () => {
      const rule = conditionOf(dsl);
      expect(rule.condition).toBe(AbilityCondition.empty);
      expect(rule.resource).toBeNull();
    });

    it('should match empty array and empty string', () => {
      expect(resolve(dsl, { user: { tags: [] } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { user: { tags: '' } }).isAllowed()).toBeTruthy();
    });

    it('should not match non-empty values, null and undefined', () => {
      expect(resolve(dsl, { user: { tags: ['a'] } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { tags: 'a' } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { tags: null } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: {} }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { tags: 0 } }).isAllowed()).toBeFalsy();
    });
  });

  describe('Operator "is not empty"', () => {
    const dsl = `
    permit permission.test if all:
      user.tags is not empty
    `;

    it('should parse without value', () => {
      expect(conditionOf(dsl).condition).toBe(AbilityCondition.not_empty);
    });

    it('should match non-empty array and string', () => {
      expect(resolve(dsl, { user: { tags: ['a'] } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { user: { tags: 'a' } }).isAllowed()).toBeTruthy();
    });

    it('should not match empty values, null and undefined', () => {
      expect(resolve(dsl, { user: { tags: [] } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { tags: '' } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { tags: null } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: {} }).isAllowed()).toBeFalsy();
    });
  });

  describe('Operator "starts with"', () => {
    const dsl = `
    permit permission.test if all:
      file.path starts with '/public/'
    `;

    it('should parse', () => {
      const rule = conditionOf(dsl);
      expect(rule.condition).toBe(AbilityCondition.starts_with);
      expect(rule.resource).toBe('/public/');
    });

    it('should match string prefix', () => {
      expect(resolve(dsl, { file: { path: '/public/a.txt' } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { file: { path: '/private/a.txt' } }).isAllowed()).toBeFalsy();
    });

    it('should be case-sensitive and not match non-strings', () => {
      expect(resolve(dsl, { file: { path: '/PUBLIC/a.txt' } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { file: { path: ['/public/'] } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { file: {} }).isAllowed()).toBeFalsy();
    });

    it('should compare with path', () => {
      const pathDsl = `
      permit permission.test if all:
        file.path starts with user.homeDir
      `;

      expect(
        resolve(pathDsl, { file: { path: '/home/john/a' }, user: { homeDir: '/home/john' } }).isAllowed(),
      ).toBeTruthy();
      expect(
        resolve(pathDsl, { file: { path: '/home/bob/a' }, user: { homeDir: '/home/john' } }).isAllowed(),
      ).toBeFalsy();
    });
  });

  describe('Operator "ends with"', () => {
    const dsl = `
    permit permission.test if all:
      user.email ends with '@example.com'
    `;

    it('should parse', () => {
      expect(conditionOf(dsl).condition).toBe(AbilityCondition.ends_with);
    });

    it('should match string suffix', () => {
      expect(resolve(dsl, { user: { email: 'john@example.com' } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { user: { email: 'john@example.org' } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { email: null } }).isAllowed()).toBeFalsy();
    });
  });

  describe('Operator "contains all"', () => {
    const dsl = `
    permit permission.test if all:
      user.roles contains all ['editor', 'reviewer']
    `;

    it('should parse', () => {
      const rule = conditionOf(dsl);
      expect(rule.condition).toBe(AbilityCondition.contains_all);
      expect(rule.resource).toEqual(['editor', 'reviewer']);
    });

    it('should match if the array contains every item', () => {
      expect(
        resolve(dsl, { user: { roles: ['editor', 'reviewer', 'user'] } }).isAllowed(),
      ).toBeTruthy();
      expect(resolve(dsl, { user: { roles: ['editor'] } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { roles: [] } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { roles: 'editor reviewer' } }).isAllowed()).toBeFalsy();
    });

    it('should accept a single value', () => {
      const single = `
      permit permission.test if all:
        user.roles contains all 'editor'
      `;

      expect(resolve(single, { user: { roles: ['editor'] } }).isAllowed()).toBeTruthy();
      expect(resolve(single, { user: { roles: ['user'] } }).isAllowed()).toBeFalsy();
    });

    it('should compare with path', () => {
      const pathDsl = `
      permit permission.test if all:
        user.roles contains all document.requiredRoles
      `;

      expect(
        resolve(pathDsl, {
          user: { roles: ['a', 'b', 'c'] },
          document: { requiredRoles: ['a', 'c'] },
        }).isAllowed(),
      ).toBeTruthy();
      expect(
        resolve(pathDsl, {
          user: { roles: ['a'] },
          document: { requiredRoles: ['a', 'c'] },
        }).isAllowed(),
      ).toBeFalsy();
      expect(
        resolve(pathDsl, {
          user: { roles: ['a'] },
          document: { requiredRoles: [] },
        }).isAllowed(),
      ).toBeFalsy();
    });
  });

  describe('Operator "contains any"', () => {
    const dsl = `
    permit permission.test if all:
      user.roles contains any ['admin', 'moderator']
    `;

    it('should parse', () => {
      expect(conditionOf(dsl).condition).toBe(AbilityCondition.contains_any);
    });

    it('should match if the array contains at least one item', () => {
      expect(resolve(dsl, { user: { roles: ['user', 'moderator'] } }).isAllowed()).toBeTruthy();
      expect(resolve(dsl, { user: { roles: ['user'] } }).isAllowed()).toBeFalsy();
      expect(resolve(dsl, { user: { roles: null } }).isAllowed()).toBeFalsy();
    });
  });

  describe('Operators "contains all/any" and groups', () => {
    it('should not confuse the "contains" operator with the next "all of:" group', () => {
      const [policy] = new AbilityDSLParser(`
      permit permission.test if any:
        any of:
          user.roles contains 'admin'
        all of:
          user.roles contains 'editor'
      `).parse();

      expect(policy.ruleSet).toHaveLength(2);
      expect(policy.ruleSet[0].compareMethod).toBe(AbilityCompare.or);
      expect(policy.ruleSet[0].rules[0].condition).toBe(AbilityCondition.contains);
      expect(policy.ruleSet[1].compareMethod).toBe(AbilityCompare.and);
    });

    it('should throw on the empty array', () => {
      expect(() =>
        new AbilityDSLParser(`
        permit permission.test if all:
          user.roles contains all []
        `).parse(),
      ).toThrow(/non-empty array/);

      expect(() =>
        new AbilityDSLParser(`
        permit permission.test if all:
          user.roles contains any []
        `).parse(),
      ).toThrow(/non-empty array/);
    });
  });

  describe('Value validation', () => {
    it.each([
      ['user.name starts with 1', /quoted string or a path/],
      ['user.name ends with true', /quoted string or a path/],
      ["user.age greater than '18'", /number or a path/],
      ["user.name length equals '3'", /number or a path/],
      ["user.role in 'admin'", /array or a path/],
      ["user.role not in 'admin'", /array or a path/],
      ["user.role equals ['admin']", /Use `in`/],
    ])('should throw on `%s`', (rule, error) => {
      expect(() =>
        new AbilityDSLParser(`
        permit permission.test if all:
          ${rule}
        `).parse(),
      ).toThrow(error);
    });
  });

  describe('Conditions', () => {
    it('should convert literals of the new conditions', () => {
      const pairs = [
        ['empty', AbilityCondition.empty],
        ['not_empty', AbilityCondition.not_empty],
        ['starts_with', AbilityCondition.starts_with],
        ['ends_with', AbilityCondition.ends_with],
        ['contains_all', AbilityCondition.contains_all],
        ['contains_any', AbilityCondition.contains_any],
      ] as const;

      pairs.forEach(([literal, condition]) => {
        expect(fromLiteral(literal)).toBe(condition);
        expect(toLiteral(condition)).toBe(literal);
      });
    });

    it('should keep the new operators after JSON round trip', () => {
      const dsl = `
      permit permission.test if all:
        user.tags is not empty
        user.email ends with '@example.com'
        user.roles contains all ['a', 'b']
      `;
      const json = AbilityJSONParser.toJSON(new AbilityDSLParser(dsl).parse());
      const policies = AbilityJSONParser.parse(JSON.parse(JSON.stringify(json)));
      const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

      expect(
        resolver
          .resolve('test', {
            user: { tags: ['x'], email: 'j@example.com', roles: ['a', 'b'] },
          } as never)
          .isAllowed(),
      ).toBeTruthy();
    });
  });

  describe('Types generation', () => {
    it('should generate types for the new operators', () => {
      const policies = new AbilityDSLParser(`
      permit permission.test if all:
        user.tags is empty
        user.email ends with '@example.com'
        user.path starts with '/'
        user.roles contains all ['a', 'b']
        user.groups contains any [1, 2]
      `).parse();

      const typeDefs = new AbilityTypeGenerator(policies).generateTypeDefs();

      expect(typeDefs).toContain('readonly tags?: string | readonly unknown[]');
      expect(typeDefs).toContain('readonly email?: string | null | undefined');
      expect(typeDefs).toContain('readonly path?: string | null | undefined');
      expect(typeDefs).toContain('readonly roles?: readonly string[] | null | undefined');
      expect(typeDefs).toContain('readonly groups?: readonly number[] | null | undefined');
    });
  });
});
