import { AbilityDSLParser } from '~/parsers/dsl/AbilityDSLParser';
import AbilityCondition from '~/core/AbilityCondition';
import AbilityCompare from '~/core/AbilityCompare';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';

describe('AbilityDSLParser', () => {
  describe('Units', () => {
    it('should parse a policy with two rule sets (all of and any of)', () => {
      const dsl = `
# Policy with 2 rule sets
permit order.update if any:
  all of:
    user.roles contains 'admin'
    user.token is not null
  any of:
    user.roles contains 'developer'
    user.logit equals 'dev'
`;

      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();

      // Expect exactly one policy
      expect(policies).toHaveLength(1);
      const policy = policies[0];

      // Policy level checks
      expect(policy.action).toBe('order.update');
      expect(policy.effect).toBe(AbilityPolicyEffect.permit);
      expect(policy.compareMethod).toBe(AbilityCompare.or);

      // There should be two rule sets
      expect(policy.ruleSet).toHaveLength(2);

      // First rule set: "all of"
      const ruleSet1 = policy.ruleSet[0];
      expect(ruleSet1.compareMethod).toBe(AbilityCompare.and);
      expect(ruleSet1.rules).toHaveLength(2);

      // Rule 1: user.roles contains 'admin'
      const rule1 = ruleSet1.rules[0];
      expect(rule1.subject).toBe('user.roles');
      expect(rule1.condition).toBe(AbilityCondition.in);
      expect(rule1.resource).toBe('admin');

      // Rule 2: user.token is not null
      const rule2 = ruleSet1.rules[1];
      expect(rule2.subject).toBe('user.token');
      expect(rule2.condition).toBe(AbilityCondition.not_equal);
      expect(rule2.resource).toBeNull();

      // Second rule set: "any of"
      const ruleSet2 = policy.ruleSet[1];
      expect(ruleSet2.compareMethod).toBe(AbilityCompare.or);
      expect(ruleSet2.rules).toHaveLength(2);

      // Rule 3: user.roles contains 'developer'
      const rule3 = ruleSet2.rules[0];
      expect(rule3.subject).toBe('user.roles');
      expect(rule3.condition).toBe(AbilityCondition.in);
      expect(rule3.resource).toBe('developer');

      // Rule 4: user.logit equals 'dev'
      const rule4 = ruleSet2.rules[1];
      expect(rule4.subject).toBe('user.logit');
      expect(rule4.condition).toBe(AbilityCondition.equal);
      expect(rule4.resource).toBe('dev');
    });

    // Additional test for a simple policy (single rule)
    it('should parse a simple policy with one rule', () => {
      const dsl = `
permit order.view if all:
  user.id equals order.owner
`;

      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();

      expect(policies).toHaveLength(1);
      const policy = policies[0];

      expect(policy.action).toBe('order.view');
      expect(policy.effect).toBe(AbilityPolicyEffect.permit);
      expect(policy.compareMethod).toBe(AbilityCompare.and);
      expect(policy.ruleSet).toHaveLength(1);

      const ruleSet = policy.ruleSet[0];
      expect(ruleSet.rules).toHaveLength(1);

      const rule = ruleSet.rules[0];
      expect(rule.subject).toBe('user.id');
      expect(rule.condition).toBe(AbilityCondition.equal);
      expect(rule.resource).toBe('order.owner');
    });

    // Test with numeric values
    it('should parse numeric values correctly', () => {
      const dsl = `
deny order.cancel if any:
  order.amount greater than 1000
  order.amount less than 50
`;

      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();

      expect(policies).toHaveLength(1);
      const policy = policies[0];

      expect(policy.action).toBe('order.cancel');
      expect(policy.effect).toBe(AbilityPolicyEffect.deny);
      expect(policy.ruleSet).toHaveLength(1);

      const ruleSet = policy.ruleSet[0];
      expect(ruleSet.rules).toHaveLength(2);

      const rule1 = ruleSet.rules[0];
      expect(rule1.subject).toBe('order.amount');
      expect(rule1.condition).toBe(AbilityCondition.more_than);
      expect(rule1.resource).toBe(1000);

      const rule2 = ruleSet.rules[1];
      expect(rule2.subject).toBe('order.amount');
      expect(rule2.condition).toBe(AbilityCondition.less_than);
      expect(rule2.resource).toBe(50);
    });
  });
});
