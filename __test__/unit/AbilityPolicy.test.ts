import AbilityPolicy from '../../src/core/AbilityPolicy';
import AbilityRule from '../../src/core/AbilityRule';
import AbilityRuleSet from '../../src/core/AbilityRuleSet';
import { AbilityMatch } from '../../src/core/AbilityMatch';
import { AbilityCompare } from '../../src/core/AbilityCompare';
import { AbilityPolicyEffect } from '../../src/core/AbilityPolicyEffect';

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
      it('should return match when all rule sets match', () => {
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

        const result = policy.check({
          user: {
            name: 'John',
            age: 25,
          },
        });

        expect(result).toBe(AbilityMatch.match);
        expect(policy.matchState).toBe(AbilityMatch.match);
      });

      it('should return mismatch when any rule set mismatches', () => {
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

        const result = policy.check({
          user: {
            name: 'John',
            age: 16, // mismatches
          },
        });

        expect(result).toBe(AbilityMatch.mismatch);
        expect(policy.matchState).toBe(AbilityMatch.mismatch);
      });

      it('should return mismatch when multiple rule sets mismatch', () => {
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

        const result = policy.check({
          user: {
            name: 'Jane', // mismatches
            city: 'SPB', // mismatches
          },
        });

        expect(result).toBe(AbilityMatch.mismatch);
      });
    });

    describe('with OR comparison', () => {
      it('should return match when at least one rule set matches', () => {
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

        const result = policy.check({
          user: {
            name: 'Jane', // mismatches
            age: 25, // matches
          },
        });

        expect(result).toBe(AbilityMatch.match);
        expect(policy.matchState).toBe(AbilityMatch.match);
      });

      it('should return mismatch when no rule sets match', () => {
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

        const result = policy.check({
          user: {
            name: 'Jane', // mismatches
            age: 16, // mismatches
          },
        });

        expect(result).toBe(AbilityMatch.mismatch);
      });
    });

    it('should return mismatch when no rule sets exist', () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      const result = policy.check({ any: 'data' });

      expect(result).toBe(AbilityMatch.mismatch);
    });

    it('should handle complex nested rule sets', () => {
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
      let result = policy.check({
        user: {
          name: 'John',
          age: 25,
          city: 'SPB',
        },
      });
      expect(result).toBe(AbilityMatch.match);

      // Case 2: Second rule set matches
      result = policy.check({
        user: {
          name: 'Jane',
          age: 16,
          city: 'Moscow',
        },
      });
      expect(result).toBe(AbilityMatch.match);

      // Case 3: No rule sets match
      result = policy.check({
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

    it('should return explain object after check', () => {
      const policy = new AbilityPolicy({
        id: 'test',
        name: 'Test Policy',
        permission: 'test.permission',
        effect: AbilityPolicyEffect.permit,
      });

      policy.addRuleSet(AbilityRuleSet.and([AbilityRule.equals('user.name', 'John')]));

      policy.check({ user: { name: 'John' } });
      const explain = policy.explain();

      expect(explain).toBeDefined();
      expect(explain.type).toBe('policy');
      expect(explain.name).toBe('Test Policy');
      expect(explain.match).toBe(AbilityMatch.match);
      expect(explain.children).toHaveLength(1);
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
  });
});
