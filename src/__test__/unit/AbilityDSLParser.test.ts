import { AbilityDSLParser } from '../../parsers/dsl/AbilityDSLParser';
import AbilityCondition from '../../core/AbilityCondition';
import AbilityCompare from '../../core/AbilityCompare';
import AbilityPolicyEffect from '../../core/AbilityPolicyEffect';
import { AbilityDSLLexer } from '../../parsers/dsl/AbilityDSLLexer';

describe('AbilityDSLParser', () => {
  it('should parse a policy with two rule sets (all of and any of)', () => {
    const dsl = `
# @name can order update
permit order.update if any:
  # @name authorized admin
  all of:
    # @name contains role admin
    user.roles contains 'admin'
    user.token is not null

  # @name if is developer
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
    expect(policy.name).toBe('can order update');

    // There should be two rule sets
    expect(policy.ruleSet).toHaveLength(2);

    // First rule set: "all of"
    const ruleSet1 = policy.ruleSet[0];
    expect(ruleSet1.compareMethod).toBe(AbilityCompare.and);
    expect(ruleSet1.rules).toHaveLength(2);
    expect(ruleSet1.name).toBe('authorized admin');

    // Rule 1: user.roles contains 'admin'
    const rule1 = ruleSet1.rules[0];
    expect(rule1.subject).toBe('user.roles');
    expect(rule1.condition).toBe(AbilityCondition.contains);
    expect(rule1.resource).toBe('admin');
    expect(rule1.name).toBe('contains role admin');

    // Rule 2: user.token is not null
    const rule2 = ruleSet1.rules[1];
    expect(rule2.subject).toBe('user.token');
    expect(rule2.condition).toBe(AbilityCondition.not_equals);
    expect(rule2.resource).toBeNull();

    // Second rule set: "any of"
    const ruleSet2 = policy.ruleSet[1];
    expect(ruleSet2.compareMethod).toBe(AbilityCompare.or);
    expect(ruleSet2.rules).toHaveLength(2);
    expect(ruleSet2.name).toBe('if is developer');

    // Rule 3: user.roles contains 'developer'
    const rule3 = ruleSet2.rules[0];
    expect(rule3.subject).toBe('user.roles');
    expect(rule3.condition).toBe(AbilityCondition.contains);
    expect(rule3.resource).toBe('developer');

    // Rule 4: user.logit equals 'dev'
    const rule4 = ruleSet2.rules[1];
    expect(rule4.subject).toBe('user.logit');
    expect(rule4.condition).toBe(AbilityCondition.equals);
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
    expect(rule.condition).toBe(AbilityCondition.equals);
    expect(rule.resource).toBe('order.owner');
  });

  it('should parse a simple policy with one rule', () => {
    const dsl = `
deny order.view if all:
  user.id not equals order.owner
`;

    const parser = new AbilityDSLParser(dsl);
    const tokens = new AbilityDSLLexer(dsl).tokenize();
    const policies = parser.parse();

    expect(policies).toHaveLength(1);
    const policy = policies[0];

    expect(policy.action).toBe('order.view');
    expect(policy.effect).toBe(AbilityPolicyEffect.deny);
    expect(policy.compareMethod).toBe(AbilityCompare.and);
    expect(policy.ruleSet).toHaveLength(1);

    const ruleSet = policy.ruleSet[0];
    expect(ruleSet.rules).toHaveLength(1);

    const rule = ruleSet.rules[0];
    expect(rule.subject).toBe('user.id');
    expect(rule.condition).toBe(AbilityCondition.not_equals);
    expect(rule.resource).toBe('order.owner');
  });

  // Test with numeric values
  it('should parse numeric values correctly', () => {
    const dsl = `
deny order.cancel if any:
  order.amount > 1000
  order.amount < 50
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
    expect(rule1.condition).toBe(AbilityCondition.greater_than);
    expect(rule1.resource).toBe(1000);

    const rule2 = ruleSet.rules[1];
    expect(rule2.subject).toBe('order.amount');
    expect(rule2.condition).toBe(AbilityCondition.less_than);
    expect(rule2.resource).toBe(50);
  });

  it('should parse numeric comparisons, arrays, and booleans', () => {
    const dsl = `
# Policy with numbers, arrays, and booleans
deny order.update if any:
  order.amount > 1000
  order.amount < 50
  user.role equals 'admin'
  user.department contains 'IT'
  user.roles in ['admin', 'manager']
  user.active equals true
`;
    const parser = new AbilityDSLParser(dsl);
    const policies = parser.parse();

    expect(policies).toHaveLength(1);
    const policy = policies[0];

    expect(policy.action).toBe('order.update');
    expect(policy.effect).toBe(AbilityPolicyEffect.deny);
    expect(policy.compareMethod).toBe(AbilityCompare.or);
    expect(policy.ruleSet).toHaveLength(1);

    const ruleSet = policy.ruleSet[0];
    expect(ruleSet.compareMethod).toBe(AbilityCompare.or);
    expect(ruleSet.rules).toHaveLength(6);

    // Rule 1: order.amount > 1000
    const rule1 = ruleSet.rules[0];
    expect(rule1.subject).toBe('order.amount');
    expect(rule1.condition).toBe(AbilityCondition.greater_than);
    expect(rule1.resource).toBe(1000);

    // Rule 2: order.amount < 50
    const rule2 = ruleSet.rules[1];
    expect(rule2.subject).toBe('order.amount');
    expect(rule2.condition).toBe(AbilityCondition.less_than);
    expect(rule2.resource).toBe(50);

    // Rule 3: user.role = 'admin'
    const rule3 = ruleSet.rules[2];
    expect(rule3.subject).toBe('user.role');
    expect(rule3.condition).toBe(AbilityCondition.equals);
    expect(rule3.resource).toBe('admin');

    // Rule 4: user.department contains 'IT'
    const rule4 = ruleSet.rules[3];
    expect(rule4.subject).toBe('user.department');
    expect(rule4.condition).toBe(AbilityCondition.contains);
    expect(rule4.resource).toBe('IT');

    // Rule 5: user.roles in ['admin', 'manager']
    const rule5 = ruleSet.rules[4];
    expect(rule5.subject).toBe('user.roles');
    expect(rule5.condition).toBe(AbilityCondition.in);
    expect(Array.isArray(rule5.resource)).toBe(true);
    expect(rule5.resource).toEqual(['admin', 'manager']);

    // Rule 6: user.active = true
    const rule6 = ruleSet.rules[5];
    expect(rule6.subject).toBe('user.active');
    expect(rule6.condition).toBe(AbilityCondition.equals);
    expect(rule6.resource).toBe(true);
  });

  it('should parse shorthand policy with implicit group', () => {
    const dsl = `
permit order.update if any:
  user.role equals 'admin'
  user.department contains 'IT'
`;

    const parser = new AbilityDSLParser(dsl);
    const policies = parser.parse();

    expect(policies).toHaveLength(1);
    const policy = policies[0];

    expect(policy.action).toBe('order.update');
    expect(policy.effect).toBe(AbilityPolicyEffect.permit);
    expect(policy.compareMethod).toBe(AbilityCompare.or);
    expect(policy.ruleSet).toHaveLength(1);

    const ruleSet = policy.ruleSet[0];
    expect(ruleSet.compareMethod).toBe(AbilityCompare.or);
    expect(ruleSet.rules).toHaveLength(2);

    const rule1 = ruleSet.rules[0];
    expect(rule1.subject).toBe('user.role');
    expect(rule1.condition).toBe(AbilityCondition.equals);
    expect(rule1.resource).toBe('admin');

    const rule2 = ruleSet.rules[1];
    expect(rule2.subject).toBe('user.department');
    expect(rule2.condition).toBe(AbilityCondition.contains);
    expect(rule2.resource).toBe('IT');
  });

  it('should parse mixed implicit and explicit groups', () => {
    const dsl = `
permit order.update if any:
  user.active equals true
  any of:
    user.roles contains 'admin'
    user.department contains 'IT'
  all of:
    order.status equals 'draft'
    order.owner equals user.id
`;

    const parser = new AbilityDSLParser(dsl);
    const policies = parser.parse();

    expect(policies).toHaveLength(1);
    const policy = policies[0];

    expect(policy.compareMethod).toBe(AbilityCompare.or);
    expect(policy.ruleSet).toHaveLength(3);

    // RuleSet 1 — with policy compare
    const implicitGroup = policy.ruleSet[0];
    expect(implicitGroup.compareMethod).toBe(AbilityCompare.or);
    expect(implicitGroup.rules).toHaveLength(1);
    expect(implicitGroup.rules[0].subject).toBe('user.active');
    expect(implicitGroup.rules[0].condition).toBe(AbilityCondition.equals);
    expect(implicitGroup.rules[0].resource).toBe(true);

    // RuleSet 2 — any of:
    const anyGroup = policy.ruleSet[1];
    expect(anyGroup.compareMethod).toBe(AbilityCompare.or);
    expect(anyGroup.rules).toHaveLength(2);
    expect(anyGroup.rules[0].subject).toBe('user.roles');
    expect(anyGroup.rules[0].condition).toBe(AbilityCondition.contains);
    expect(anyGroup.rules[0].resource).toBe('admin');
    expect(anyGroup.rules[1].subject).toBe('user.department');
    expect(anyGroup.rules[1].condition).toBe(AbilityCondition.contains);
    expect(anyGroup.rules[1].resource).toBe('IT');

    // RuleSet 3 — all of:
    const allGroup = policy.ruleSet[2];
    expect(allGroup.compareMethod).toBe(AbilityCompare.and);
    expect(allGroup.rules).toHaveLength(2);
    expect(allGroup.rules[0].subject).toBe('order.status');
    expect(allGroup.rules[0].condition).toBe(AbilityCondition.equals);
    expect(allGroup.rules[0].resource).toBe('draft');
    expect(allGroup.rules[1].subject).toBe('order.owner');
    expect(allGroup.rules[1].condition).toBe(AbilityCondition.equals);
    expect(allGroup.rules[1].resource).toBe('user.id');
  });

  it('should parse null comparisons correctly', () => {
    const dsl = `
permit order.update if all:
  user.deletedAt is null
  user.token is not null
  order.amount > 1000
`;

    const parser = new AbilityDSLParser(dsl);
    const policies = parser.parse();

    expect(policies).toHaveLength(1);
    const policy = policies[0];
    expect(policy.action).toBe('order.update');
    expect(policy.effect).toBe(AbilityPolicyEffect.permit);
    expect(policy.compareMethod).toBe(AbilityCompare.and);
    expect(policy.ruleSet).toHaveLength(1);

    const ruleSet = policy.ruleSet[0];
    expect(ruleSet.compareMethod).toBe(AbilityCompare.and);
    expect(ruleSet.rules).toHaveLength(3);

    // Rule 1: user.deletedAt is null
    const rule1 = ruleSet.rules[0];
    expect(rule1.subject).toBe('user.deletedAt');
    expect(rule1.condition).toBe(AbilityCondition.equals);
    expect(rule1.resource).toBeNull();

    // Rule 2: user.token is not null
    const rule2 = ruleSet.rules[1];
    expect(rule2.subject).toBe('user.token');
    expect(rule2.condition).toBe(AbilityCondition.not_equals);
    expect(rule2.resource).toBeNull();

    // Rule 3: order.amount greater than 1000 (numeric still works)
    const rule3 = ruleSet.rules[2];
    expect(rule3.subject).toBe('order.amount');
    expect(rule3.condition).toBe(AbilityCondition.greater_than);
    expect(rule3.resource).toBe(1000);
  });

  it('should parse environment paths correctly', () => {
    const dsl = `
permit order.update if any:
  env.time.hour > 9
  user.role equals 'admin'
`;

    const parser = new AbilityDSLParser(dsl);
    const policies = parser.parse();

    expect(policies).toHaveLength(1);
    const policy = policies[0];

    expect(policy.action).toBe('order.update');
    expect(policy.effect).toBe(AbilityPolicyEffect.permit);
    expect(policy.compareMethod).toBe(AbilityCompare.or);
    expect(policy.ruleSet).toHaveLength(1);

    const ruleSet = policy.ruleSet[0];
    expect(ruleSet.rules).toHaveLength(2);

    const rule1 = ruleSet.rules[0];
    expect(rule1.subject).toBe('env.time.hour');
    expect(rule1.condition).toBe(AbilityCondition.greater_than);
    expect(rule1.resource).toBe(9);

    const rule2 = ruleSet.rules[1];
    expect(rule2.subject).toBe('user.role');
    expect(rule2.condition).toBe(AbilityCondition.equals);
    expect(rule2.resource).toBe('admin');
  });

  describe('Operators', () => {
    it('should parse "is equal" correctly', () => {
      const dsl = `
permit test.action if all:
  user.name is equals 'John'
`;
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();
      const rule = policies[0].ruleSet[0].rules[0];
      expect(rule.condition).toBe(AbilityCondition.equals);
      expect(rule.resource).toBe('John');
    });

    it('should parse "is not equal" correctly', () => {
      const dsl = `
permit test.action if all:
  user.name is not equals 'John'
`;
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();
      const rule = policies[0].ruleSet[0].rules[0];
      expect(rule.condition).toBe(AbilityCondition.not_equals);
      expect(rule.resource).toBe('John');
    });

    it('should parse "contains" correctly', () => {
      const dsl = `
permit test.action if all:
  user.roles contains 'admin'
`;
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();
      const rule = policies[0].ruleSet[0].rules[0];
      expect(rule.condition).toBe(AbilityCondition.contains);
      expect(rule.resource).toBe('admin');
    });

    it('should parse "not contains" correctly', () => {
      const dsl = `
permit test.action if all:
  user.roles not contains 'admin'
`;
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();
      const rule = policies[0].ruleSet[0].rules[0];
      expect(rule.condition).toBe(AbilityCondition.not_contains);
      expect(rule.resource).toBe('admin');
    });

    it('should parse "in" correctly (with array)', () => {
      const dsl = `
permit test.action if all:
  user.role in ['admin', 'manager']
`;
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();
      const rule = policies[0].ruleSet[0].rules[0];
      expect(rule.condition).toBe(AbilityCondition.in);
      expect(rule.resource).toEqual(['admin', 'manager']);
    });

    it('should parse "not in" correctly (with array)', () => {
      const dsl = `
permit test.action if all:
  user.role not in ['admin', 'manager']
`;
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();
      const rule = policies[0].ruleSet[0].rules[0];
      expect(rule.condition).toBe(AbilityCondition.not_in);
      expect(rule.resource).toEqual(['admin', 'manager']);
    });
  });
});
