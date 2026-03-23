import AbilityRule from '../../core/AbilityRule';
import AbilityRuleSet from '../../core/AbilityRuleSet';
import AbilityMatch from '../../core/AbilityMatch';
import AbilityCompare from '../../core/AbilityCompare';
import { AbilityJSONParser } from '../../parsers/AbilityJSONParser';

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
      const rule = AbilityRule.equal('user.name', 'John');

      ruleSet.addRule(rule);

      expect(ruleSet.rules).toHaveLength(1);
      expect(ruleSet.rules[0]).toBe(rule);
    });

    it('should add multiple rules', () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and });
      const rules = [AbilityRule.equal('user.name', 'John'), AbilityRule.equal('user.age', 25)];

      ruleSet.addRules(rules);

      expect(ruleSet.rules).toHaveLength(2);
      expect(ruleSet.rules).toEqual(rules);
    });

    it('should support chaining', () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and })
        .addRule(AbilityRule.equal('user.name', 'John'))
        .addRule(AbilityRule.equal('user.age', 25));

      expect(ruleSet.rules).toHaveLength(2);
    });
  });

  describe('check method with AND comparison', () => {
    it('should return match when all rules match', async () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = await ruleSet.check({
        user: {
          name: 'John',
          age: 25,
        },
      });

      expect(result).toBe(AbilityMatch.match);
    });

    it('should return mismatch when any rule mismatches', async () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = await ruleSet.check({
        user: {
          name: 'John',
          age: 16,
        },
      });

      expect(result).toBe(AbilityMatch.mismatch);
    });

    it('should return mismatch when multiple rules mismatch', async () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
        AbilityRule.equal('user.city', 'Moscow'),
      ]);

      const result = await ruleSet.check({
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
    it('should return match when at least one rule matches', async () => {
      const ruleSet = AbilityRuleSet.or([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = await ruleSet.check({
        user: {
          name: 'Jane',
          age: 25,
        },
      });

      expect(result).toBe(AbilityMatch.match);
    });

    it('should return mismatch when no rules match', async () => {
      const ruleSet = AbilityRuleSet.or([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const result = await ruleSet.check({
        user: {
          name: 'Jane',
          age: 16,
        },
      });

      expect(result).toBe(AbilityMatch.mismatch);
    });

    it('should return match when multiple rules match', async () => {
      const ruleSet = AbilityRuleSet.or([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
        AbilityRule.equal('user.city', 'Moscow'),
      ]);

      const result = await ruleSet.check({
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
    it('should handle comparison with dot notation resources', async () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equal('user.name', 'opponent.name'),
        AbilityRule.moreThan('user.age', 'opponent.age'),
      ]);

      const result = await ruleSet.check({
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

    it('should handle empty rule set', async () => {
      const ruleSet = new AbilityRuleSet({ compareMethod: AbilityCompare.and });

      const result = await ruleSet.check({ any: 'data' });

      expect(result).toBe(AbilityMatch.mismatch);
    });
  });

  describe('parse and export', () => {
    it('should parse config to rule set', () => {
      const config = {
        id: 'test-id',
        name: 'Test RuleSet',
        compareMethod: 'or' as const,
        rules: [
          {
            subject: 'user.name',
            resource: 'John',
            condition: '=' as const,
          },
          {
            subject: 'user.age',
            resource: 18,
            condition: '>' as const,
          },
        ],
      };

      const ruleSet = AbilityRuleSet.fromJSON(config);

      expect(ruleSet.id).toBe('test-id');
      expect(ruleSet.name).toBe('Test RuleSet');
      expect(ruleSet.compareMethod.code).toBe('or');
      expect(ruleSet.rules).toHaveLength(2);
    });

    it('should export rule set to config', () => {
      const ruleSet = AbilityRuleSet.and([
        AbilityRule.equal('user.name', 'John'),
        AbilityRule.moreThan('user.age', 18),
      ]);

      const config = AbilityJSONParser.ruleSetToJSON(ruleSet);

      expect(config.compareMethod).toBe('and');
      expect(config.rules).toHaveLength(2);
    });
  });

  describe('static factory methods', () => {
    it('should create AND rule set', () => {
      const rules = [AbilityRule.equal('user.name', 'John')];
      const ruleSet = AbilityRuleSet.and(rules);

      expect(ruleSet.compareMethod).toBe(AbilityCompare.and);
      expect(ruleSet.rules).toEqual(rules);
    });

    it('should create OR rule set', () => {
      const rules = [AbilityRule.equal('user.name', 'John')];
      const ruleSet = AbilityRuleSet.or(rules);

      expect(ruleSet.compareMethod).toBe(AbilityCompare.or);
      expect(ruleSet.rules).toEqual(rules);
    });
  });
});
