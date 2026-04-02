import AbilityRule from '~/core/AbilityRule';
import AbilityRuleSet from '~/core/AbilityRuleSet';
import AbilityMatch from '~/core/AbilityMatch';
import AbilityCompare from '~/core/AbilityCompare';

describe('AbilityRuleSet', () => {
  describe('constructor', () => {
    it('should create rule set with provided params', () => {
      const ruleSet = new AbilityRuleSet({
        id: 'test-id',
        name: 'Test RuleSet',
        compareMethod: AbilityCompare.and,
      });

      expect(ruleSet.id).toBe('test-id');
      expect(ruleSet.name).toBe('Test RuleSet');
      expect(ruleSet.compareMethod).toBe(AbilityCompare.and);
      expect(ruleSet.rules).toHaveLength(0);
    });

    it('should generate name and id if not provided', () => {
      const ruleSet = new AbilityRuleSet({
        compareMethod: AbilityCompare.or,
      });

      expect(ruleSet.id).toBeDefined();
      expect(ruleSet.name).toBeDefined();
    });
  });

  describe('addRule and addRules', () => {
    it('should add single rule', () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and });
      const rule = AbilityRule.equals('user.name', 'John');

      ruleSet.addRule(rule);

      expect(ruleSet.rules).toHaveLength(1);
      expect(ruleSet.rules[0]).toBe(rule);
    });

    it('should add multiple rules', () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and });
      const rules = [AbilityRule.equals('user.name', 'John'), AbilityRule.equals('user.age', 25)];

      ruleSet.addRules(rules);

      expect(ruleSet.rules).toHaveLength(2);
      expect(ruleSet.rules).toEqual(rules);
    });

    it('should support chaining', () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and })
        .addRule(AbilityRule.equals('user.name', 'John'))
        .addRule(AbilityRule.equals('user.age', 25));

      expect(ruleSet.rules).toHaveLength(2);
    });
  });

  describe('check method with AND comparison', () => {
    it('should return match when all rules match', () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equals('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'John',
          age: 25,
        },
      });

      expect(result).toBe(AbilityMatch.match);
    });

    it('should return mismatch when any rule mismatches', () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equals('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'John',
          age: 16,
        },
      });

      expect(result).toBe(AbilityMatch.mismatch);
    });

    it('should return mismatch when multiple rules mismatch', () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equals('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
        AbilityRule.equals('user.city', 'Moscow'),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'Jane',
          age: 16,
          city: 'Moscow',
        },
      });

      expect(result).toBe(AbilityMatch.mismatch);
    });
  });

  describe('check method with OR comparison', () => {
    it('should return match when at least one rule matches', () => {
      const ruleSet = AbilityRuleSet.or([
        AbilityRule.equals('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'Jane',
          age: 25,
        },
      });

      expect(result).toBe(AbilityMatch.match);
    });

    it('should return mismatch when no rules match', () => {
      const ruleSet = AbilityRuleSet.or([
        AbilityRule.equals('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'Jane',
          age: 16,
        },
      });

      expect(result).toBe(AbilityMatch.mismatch);
    });

    it('should return match when multiple rules match', () => {
      const ruleSet = AbilityRuleSet.or([
        AbilityRule.equals('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
        AbilityRule.equals('user.city', 'Moscow'),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'John',
          age: 25,
          city: 'Moscow',
        },
      });

      expect(result).toBe(AbilityMatch.match);
    });
  });

  describe('complex scenarios', () => {
    it('should handle comparison with dot notation resources', () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equals('user.name', 'opponent.name'),
        AbilityRule.moreThan('user.age', 'opponent.age'),
      ]);

      const result = ruleSet.check({
        user: {
          name: 'Oleg',
          age: 32,
        },
        opponent: {
          name: 'Oleg',
          age: 22,
        },
      });

      expect(result).toBe(AbilityMatch.match);
    });

    it('should handle empty rule set', () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and });

      const result = ruleSet.check({ any: 'data' });

      expect(result).toBe(AbilityMatch.mismatch);
    });
  });

  describe('static factory methods', () => {
    it('should create AND rule set', () => {
      const rules = [AbilityRule.equals('user.name', 'John')];
      const ruleSet = AbilityRuleSet.and(rules);

      expect(ruleSet.compareMethod).toBe(AbilityCompare.and);
      expect(ruleSet.rules).toEqual(rules);
    });

    it('should create OR rule set', () => {
      const rules = [AbilityRule.equals('user.name', 'John')];
      const ruleSet = AbilityRuleSet.or(rules);

      expect(ruleSet.compareMethod).toBe(AbilityCompare.or);
      expect(ruleSet.rules).toEqual(rules);
    });
  });
});
