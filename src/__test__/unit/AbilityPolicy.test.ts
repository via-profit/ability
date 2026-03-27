import type { AbilityPolicyConfig } from '../../core/AbilityPolicy';
import AbilityPolicy from '../../core/AbilityPolicy';
import AbilityRule from '../../core/AbilityRule';
import AbilityRuleSet from '../../core/AbilityRuleSet';
import AbilityMatch from '../../core/AbilityMatch';
import AbilityCompare from '../../core/AbilityCompare';
import AbilityPolicyEffect from '../../core/AbilityPolicyEffect';

describe('AbilityPolicy', () => {
  describe('constructor', () => {
    it('should create policy with provided params', () => {
      const policy = new AbilityPolicy({
        id: 'test-id',
        name: 'Test Policy',
        permission: 'order.create',
        effect: AbilityPolicyEffect.permit,
      });

      expect(policy.id).toBe('test-id');
      expect(policy.name).toBe('Test Policy');
      expect(policy.permission).toBe('order.create');
      expect(policy.effect).toBe(AbilityPolicyEffect.permit);
      expect(policy.ruleSet).toHaveLength(0);
      expect(policy.matchState).toBe(AbilityMatch.pending);
      expect(policy.compareMethod).toBe(AbilityCompare.and); // default value
    });

  });

  describe('addRuleSet', () => {
    it('should add a rule set to the policy', () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      const ruleSet = AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]);

      policy.addRuleSet(ruleSet);

      expect(policy.ruleSet).toHaveLength(1);
      expect(policy.ruleSet[0]).toBe(ruleSet);
    });

    it('should support chaining', () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      const ruleSet1 = AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]);
      const ruleSet2 = AbilityRuleSet.or([AbilityRule.moreThan('user.age', 18)]);

      policy.addRuleSet(ruleSet1).addRuleSet(ruleSet2);

      expect(policy.ruleSet).toHaveLength(2);
    });
  });

  describe('check method', () => {
    describe('with AND comparison', () => {
      it('should return match when all rule sets match', async () => {
        const policy = new AbilityPolicy({
          id: 'test',
          name: 'Test',
          permission: 'test.permission',
          effect: AbilityPolicyEffect.permit,
        });
        policy.compareMethod = AbilityCompare.and;

        policy
          .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]))
          .addRuleSet(AbilityRuleSet.and([AbilityRule.moreThan('user.age', 18)]));

        const result = await policy.check({
          user: {
            name: 'John',
            age: 25,
          },
        });

        expect(result).toBe(AbilityMatch.match);
        expect(policy.matchState).toBe(AbilityMatch.match);
      });

      it('should return mismatch when any rule set mismatches', async () => {
        const policy = new AbilityPolicy({
          id: 'test',
          name: 'Test',
          permission: 'test.permission',
          effect: AbilityPolicyEffect.permit,
        });
        policy.compareMethod = AbilityCompare.and;

        policy
          .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]))
          .addRuleSet(AbilityRuleSet.and([AbilityRule.moreThan('user.age', 18)]));

        const result = await policy.check({
          user: {
            name: 'John',
            age: 16, // mismatches
          },
        });

        expect(result).toBe(AbilityMatch.mismatch);
        expect(policy.matchState).toBe(AbilityMatch.mismatch);
      });

      it('should return mismatch when multiple rule sets mismatch', async () => {
        const policy = new AbilityPolicy({
          id: 'test',
          name: 'Test',
          permission: 'test.permission',
          effect: AbilityPolicyEffect.permit,
        });
        policy.compareMethod = AbilityCompare.and;

        policy
          .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]))
          .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.city', 'Moscow')]));

        const result = await policy.check({
          user: {
            name: 'Jane', // mismatches
            city: 'SPB', // mismatches
          },
        });

        expect(result).toBe(AbilityMatch.mismatch);
      });
    });

    describe('with OR comparison', () => {
      it('should return match when at least one rule set matches', async () => {
        const policy = new AbilityPolicy({
          id: 'test',
          name: 'Test',
          permission: 'test.permission',
          effect: AbilityPolicyEffect.permit,
        });
        policy.compareMethod = AbilityCompare.or;

        policy
          .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]))
          .addRuleSet(AbilityRuleSet.and([AbilityRule.moreThan('user.age', 18)]));

        const result = await policy.check({
          user: {
            name: 'Jane', // mismatches
            age: 25, // matches
          },
        });

        expect(result).toBe(AbilityMatch.match);
        expect(policy.matchState).toBe(AbilityMatch.match);
      });

      it('should return mismatch when no rule sets match', async () => {
        const policy = new AbilityPolicy({
          id: 'test',
          name: 'Test',
          permission: 'test.permission',
          effect: AbilityPolicyEffect.permit,
        });
        policy.compareMethod = AbilityCompare.or;

        policy
          .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]))
          .addRuleSet(AbilityRuleSet.and([AbilityRule.moreThan('user.age', 18)]));

        const result = await policy.check({
          user: {
            name: 'Jane', // mismatches
            age: 16, // mismatches
          },
        });

        expect(result).toBe(AbilityMatch.mismatch);
      });
    });

    it('should return mismatch when no rule sets exist', async () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      const result = await policy.check({ any: 'data' });

      expect(result).toBe(AbilityMatch.mismatch);
    });

    it('should handle complex nested rule sets', async () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
        compareMethod: AbilityCompare.or,
      });

      // (user.name = John AND user.age > 18) OR (user.city = Moscow)
      policy
        .addRuleSet(
          AbilityRuleSet.and([
            AbilityRule.equals('user.name', 'John'),
            AbilityRule.moreThan('user.age', 18),
          ]),
        )
        .addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.city', 'Moscow')]));

      // Case 1: First rule set matches
      let result = await policy.check({
        user: {
          name: 'John',
          age: 25,
          city: 'SPB',
        },
      });
      expect(result).toBe(AbilityMatch.match);

      // Case 2: Second rule set matches
      result = await policy.check({
        user: {
          name: 'Jane',
          age: 16,
          city: 'Moscow',
        },
      });
      expect(result).toBe(AbilityMatch.match);

      // Case 3: No rule sets match
      result = await policy.check({
        user: {
          name: 'Jane',
          age: 16,
          city: 'SPB',
        },
      });
      expect(result).toBe(AbilityMatch.mismatch);
    });
  });

  describe('explain method', () => {
    it('should throw error if check not called first', () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      expect(() => policy.explain()).toThrow('First, run the check method, then explain');
    });

    it('should return explain object after check', async () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test Policy',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      policy.addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]));

      await policy.check({ user: { name: 'John' } });
      const explain = policy.explain();

      expect(explain).toBeDefined();
      expect(explain.type).toBe('policy');
      expect(explain.name).toBe('Test Policy');
      expect(explain.match).toBe(AbilityMatch.match);
      expect(explain.children).toHaveLength(1);
    });
  });

  describe('parse method', () => {
    it('should parse config to policy', () => {
      const config: AbilityPolicyConfig = {
        id: 'test-id',
        name: 'Test Policy',
        permission: 'order.create',
        effect: 'permit',
        compareMethod: 'or',
        ruleSet: [
          {
            id: 'rule-set-1',
            name: 'Rule Set 1',
            compareMethod: 'and',
            rules: [
              {
                subject: 'user.name',
                resource: 'John',
                condition: '=',
              },
              {
                subject: 'user.age',
                resource: 18,
                condition: '>',
              },
            ],
          },
        ],
      };

      const policy = AbilityPolicy.fromJSON(config);

      expect(policy.id).toBe('test-id');
      expect(policy.name).toBe('Test Policy');
      expect(policy.permission).toBe('order.create');
      expect(policy.effect.code).toBe('permit');
      expect(policy.compareMethod.code).toBe('or');
      expect(policy.ruleSet).toHaveLength(1);
      expect(policy.ruleSet[0].rules).toHaveLength(2);
    });

    it('should handle empty ruleSet', () => {
      const config: AbilityPolicyConfig = {
        id: 'test-id',
        name: 'Test Policy',
        permission: 'order.create',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      };

      const policy = AbilityPolicy.fromJSON(config);

      expect(policy.ruleSet).toHaveLength(0);
    });
  });

  describe('parseAll method', () => {
    it('should parse multiple configs to policies', () => {
      const configs: AbilityPolicyConfig[] = [
        {
          id: 'policy-1',
          name: 'Policy 1',
          permission: 'order.create',
          effect: 'permit',
          compareMethod: 'and',
          ruleSet: [],
        },
        {
          id: 'policy-2',
          name: 'Policy 2',
          permission: 'order.update',
          effect: 'deny',
          compareMethod: 'or',
          ruleSet: [],
        },
      ];

      const policies = AbilityPolicy.fromJSONAll(configs);

      expect(policies).toHaveLength(2);
      expect(policies[0].id).toBe('policy-1');
      expect(policies[1].id).toBe('policy-2');
      expect(policies[0].effect.code).toBe('permit');
      expect(policies[1].effect.code).toBe('deny');
    });

    it('should return empty array for empty configs', () => {
      const policies = AbilityPolicy.fromJSONAll([]);
      expect(policies).toHaveLength(0);
    });
  });

  describe('export method', () => {
    it('should export policy to config', () => {
      const originalConfig: AbilityPolicyConfig = {
        id: 'test-id',
        name: 'Test Policy',
        permission: 'order.create',
        effect: 'deny',
        compareMethod: 'and',
        ruleSet: [
          {
            id: 'rule-set-1',
            name: 'Rule Set 1',
            compareMethod: 'or',
            rules: [
              {
                id: "rule-1",
                name: 'Rule 1',
                subject: 'user.role',
                resource: 'admin',
                condition: '=',
              },
            ],
          },
        ],
      };

      const policy = AbilityPolicy.fromJSON(originalConfig);
      const exportedConfig = policy.toJSON();

      expect(exportedConfig).toEqual(originalConfig);
    });

    it('should handle policy without rule sets', () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      const config = policy.toJSON();

      expect(config.id).toBe('test');
      expect(config.name).toBe('Test');
      expect(config.permission).toBe('test.permission');
      expect(config.effect).toBe('permit');
      expect(config.compareMethod).toBe('and');
      expect(config.ruleSet).toHaveLength(0);
    });
  });

  describe('integration with real-world scenario', () => {
    type Resources = {
      'order.status': {
        readonly user: {
          readonly roles: readonly string[];
        };
        readonly order: {
          readonly status: string;
        };
        readonly feature: {
          readonly status: string;
        };
      };
    };

    it('should deny status change for non-admins', async () => {
      const config: AbilityPolicyConfig = {
        id: 'policy-1',
        name: 'Deny status change for non-admins',
        permission: 'order.status',
        effect: 'deny',
        compareMethod: 'and',
        ruleSet: [
          {
            id: 'rule-set-1',
            name: 'Not administrator',
            compareMethod: 'and',
            rules: [
              {
                subject: 'user.roles',
                resource: 'administrator',
                condition: 'not contains',
              },
            ],
          },
          {
            id: 'rule-set-2',
            name: 'Status change check',
            compareMethod: 'and',
            rules: [
              {
                subject: 'order.status',
                resource: 'не обработан',
                condition: '=',
              },
              {
                subject: 'feature.status',
                resource: 'завершен',
                condition: '=',
              },
            ],
          },
        ],
      };

      const policy = AbilityPolicy.fromJSON<Resources['order.status']>(config);

      await policy.check({
        user: { roles: ['user', 'manager'] },
        order: { status: 'не обработан' },
        feature: { status: 'завершен' },
      });

      // console.debug(policy.explain().toString());

      expect(policy.matchState).toBe(AbilityMatch.match);
    });

    it('should not match for admins', () => {
      const config: AbilityPolicyConfig = {
        id: 'policy-1',
        name: 'Deny status change for non-admins',
        permission: 'order.status',
        effect: 'deny',
        compareMethod: 'and',
        ruleSet: [
          {
            id: 'rule-set-1',
            name: 'Not administrator',
            compareMethod: 'and',
            rules: [
              {
                subject: 'user.roles',
                resource: 'administrator',
                condition: 'not in',
              },
            ],
          },
          {
            id: 'rule-set-2',
            name: 'Status change check',
            compareMethod: 'and',
            rules: [
              {
                subject: 'order.status',
                resource: 'не обработан',
                condition: '=',
              },
              {
                subject: 'feature.status',
                resource: 'завершен',
                condition: '=',
              },
            ],
          },
        ],
      };

      const policy = AbilityPolicy.fromJSON<Resources['order.status']>(config);

      policy.check({
        user: { roles: ['administrator'] },
        order: { status: 'не обработан' },
        feature: { status: 'завершен' },
      });

      expect(policy.matchState).toBe(AbilityMatch.mismatch);
    });
  });
});
