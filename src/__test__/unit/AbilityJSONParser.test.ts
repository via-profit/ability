/* eslint-disable @typescript-eslint/no-explicit-any */

import AbilityParser, { NestedDict } from '../../core/AbilityParser';
import AbilityPolicy from '../../core/AbilityPolicy';
import { AbilityParserError } from '../../core/AbilityError';

describe('AbilityJSONParser', () => {
  describe('setValueDotValue', () => {
    it('should set value on simple path', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'user.name', 'John');

      expect(obj).toEqual({
        user: {
          name: 'John',
        },
      });
    });

    it('should set value on nested path', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'user.profile.address.city', 'Moscow');

      expect(obj).toEqual({
        user: {
          profile: {
            address: {
              city: 'Moscow',
            },
          },
        },
      });
    });

    it('should create arrays for numeric indices', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'users[0].name', 'John');
      AbilityParser.setValueDotValue(obj, 'users[1].name', 'Jane');

      expect(obj).toEqual({
        users: [{ name: 'John' }, { name: 'Jane' }],
      });
    });

    it('should handle mixed object and array paths', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'company.employees[0].profile.name', 'John');

      expect(obj).toEqual({
        company: {
          employees: [
            {
              profile: {
                name: 'John',
              },
            },
          ],
        },
      });
    });

    it('should throw error for invalid path', () => {
      const obj: NestedDict = {};

      expect(() => AbilityParser.setValueDotValue(obj, '', 'value')).toThrow(AbilityParserError);

      expect(() => AbilityParser.setValueDotValue(obj, '   ', 'value')).toThrow(AbilityParserError);
    });

    it('should throw error when trying to traverse non-object', () => {
      const obj = {
        user: 'not an object',
      };

      expect(() => AbilityParser.setValueDotValue(obj, 'user.name', 'John')).toThrow(
        AbilityParserError,
      );

      expect(() => AbilityParser.setValueDotValue(obj, 'user.name', 'John')).toThrow(
        "Cannot set property 'user' on non-object value at path: user.name",
      );
    });

    it('should throw error when trying to set primitive on existing object', () => {
      const obj: NestedDict = {
        user: {
          profile: {
            name: 'John',
          },
        },
      };

      expect(() => AbilityParser.setValueDotValue(obj, 'user.profile', 'not object')).toThrow(
        AbilityParserError,
      );

      expect(() => AbilityParser.setValueDotValue(obj, 'user.profile', 'not object')).toThrow(
        'Cannot set primitive value on existing object',
      );
    });

    it('should handle array with index', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'users[0].name', 'John');

      expect(obj).toEqual({
        users: [{ name: 'John' }],
      });
    });

    it('should preserve existing values', () => {
      const obj: NestedDict = {
        user: {
          name: 'John',
          age: 30,
        },
      };

      AbilityParser.setValueDotValue(obj, 'user.city', 'Moscow');

      expect(obj).toEqual({
        user: {
          name: 'John',
          age: 30,
          city: 'Moscow',
        },
      });
    });
  });

  describe('generateTypeDefs', () => {

    it('should generate type for simple equality rule', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.name',
                  resource: 'John',
                  condition: '=',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain("['test.permission']");
      expect(result).toContain('readonly user: {');
      expect(result).toContain('readonly name: string;');
    });

    it('should generate number type for numeric comparisons', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.age',
                  resource: 18,
                  condition: '>',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly age: number;');
    });

    it('should generate array type for in/not in conditions', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.roles',
                  resource: ['admin', 'manager'],
                  condition: 'in',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly roles: string[];');
    });

    it('should generate union types for conflicting types', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy 1',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.value',
                  resource: 'string',
                  condition: '=',
                },
              ],
            },
          ],
        }),
        AbilityPolicy.fromJSON({
          id: 'policy-2',
          name: 'Test Policy 2',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-2',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.value',
                  resource: 42,
                  condition: '>',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly value: string | number;');
    });

    it('should handle multiple permissions', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Policy 1',
          permission: 'order.create',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'RuleSet 1',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.name',
                  resource: 'John',
                  condition: '=',
                },
              ],
            },
          ],
        }),
        AbilityPolicy.fromJSON({
          id: 'policy-2',
          name: 'Policy 2',
          permission: 'user.update',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-2',
              name: 'RuleSet 2',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.age',
                  resource: 18,
                  condition: '>=',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain("['order.create']");
      expect(result).toContain("['user.update']");
      expect(result).toContain('readonly name: string;');
      expect(result).toContain('readonly age: number;');
    });

    it('should handle nested paths correctly', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.profile.address.city',
                  resource: 'Moscow',
                  condition: '=',
                },
                {
                  subject: 'user.profile.address.zip',
                  resource: 101000,
                  condition: '=',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly user: {');
      expect(result).toContain('readonly profile: {');
      expect(result).toContain('readonly address: {');
      expect(result).toContain('readonly city: string;');
      expect(result).toContain('readonly zip: number;');
    });

    it('should handle boolean values', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.isActive',
                  resource: true,
                  condition: '=',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly isActive: boolean;');
    });

    it('should handle empty arrays', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.tags',
                  resource: [],
                  condition: 'in',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly tags: any[];');
    });

    it('should handle arrays with mixed types', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.mixed',
                  resource: ['string', 42, true] as any,
                  condition: 'in',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain('readonly mixed: (string | number | boolean)[];');
    });

    it('should sort keys alphabetically', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'policy-1',
          name: 'Test Policy',
          permission: 'test.permission',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'rule-set-1',
              name: 'Test RuleSet',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.zField',
                  resource: 'z',
                  condition: '=',
                },
                {
                  subject: 'user.aField',
                  resource: 'a',
                  condition: '=',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      const aIndex = result.indexOf('aField');
      const zIndex = result.indexOf('zField');

      expect(aIndex).toBeLessThan(zIndex);
    });
  });

  describe('integration with real policies', () => {
    it('should generate types for auth policy', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'auth-policy',
          name: 'Authorization Required',
          permission: '*.authorized',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'token-check',
              name: 'Valid Token Check',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.token.id',
                  resource: 'NOT_ASSIGNED',
                  condition: '<>',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain("['*.authorized']");
      expect(result).toContain('readonly user: {');
      expect(result).toContain('readonly token: {');
      expect(result).toContain('readonly id: string;');
    });

    it('should generate types for status change policy', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'status-policy',
          name: 'Prevent Status Change',
          permission: 'order.status.update',
          effect: 'deny',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'status-check',
              name: 'Status Change Check',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'order.currentStatus',
                  resource: 'completed',
                  condition: '=',
                },
                {
                  subject: 'order.newStatus',
                  resource: 'new',
                  condition: '=',
                },
              ],
            },
            {
              id: 'admin-check',
              name: 'Not Administrator',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'user.roles',
                  resource: 'administrator',
                  condition: 'not in',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain("['order.status.update']");
      expect(result).toContain('readonly order: {');
      expect(result).toContain('readonly currentStatus: string;');
      expect(result).toContain('readonly newStatus: string;');
      expect(result).toContain('readonly user: {');
      expect(result).toContain('readonly roles: string[];');
    });

    it('should generate types for password access policy', () => {
      const policies = [
        AbilityPolicy.fromJSON({
          id: 'password-policy',
          name: 'Password Access Restriction',
          permission: 'user.account.password.read',
          effect: 'deny',
          compareMethod: 'and',
          ruleSet: [
            {
              id: 'password-check',
              name: 'Password Access Check',
              compareMethod: 'and',
              rules: [
                {
                  subject: 'resource.field',
                  resource: 'password',
                  condition: '=',
                },
                {
                  subject: 'user.account.id',
                  resource: 'user.id',
                  condition: '<>',
                },
              ],
            },
          ],
        }),
      ];

      const result = AbilityParser.generateTypeDefs(policies);

      expect(result).toContain("['user.account.password.read']");
      expect(result).toContain('readonly resource: {');
      expect(result).toContain('readonly field: string;');
      expect(result).toContain('readonly user: {');
      expect(result).toContain('readonly account: {');
      expect(result).toContain('readonly id: string;');
    });
  });
});
