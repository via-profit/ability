import AbilityRule from '../../src/core/AbilityRule';
import {AbilityCondition} from '../../src/core/AbilityCondition';
import {AbilityMatch} from '../../src/core/AbilityMatch';

describe('AbilityRule', () => {
  describe('constructor', () => {
    it('should create rule with provided params', () => {
      const rule = new AbilityRule({
        id: 'test-id',
        name: 'Test Rule',
        subject: 'user.age',
        resource: 18,
        condition: AbilityCondition.greater_than,
      });

      expect(rule.id).toBe('test-id');
      expect(rule.name).toBe('Test Rule');
      expect(rule.subject).toBe('user.age');
      expect(rule.resource).toBe(18);
      expect(rule.condition).toBe(AbilityCondition.greater_than);
    });

    it('should generate name and id if not provided', () => {
      const rule = new AbilityRule({
        subject: 'user.age',
        resource: 18,
        condition: AbilityCondition.greater_than,
      });

      expect(rule.id).toBeDefined();
      expect(rule.name).toBeDefined();
    });
  });

  describe('check method', () => {
    describe('equality operations', () => {
      it('should return match when values are equal',() => {
        const rule = AbilityRule.equals('user.name', 'John');

        const result = rule.check({ user: { name: 'John' } });

        expect(result).toBe(AbilityMatch.match);
      });

      it('should return mismatch when values are not equal',() => {
        const rule = AbilityRule.equals('user.name', 'John');

        const result = rule.check({ user: { name: 'Jane' } });

        expect(result).toBe(AbilityMatch.mismatch);
      });

      it('should compare with dot notation resource',() => {
        const rule = AbilityRule.equals('user.name', 'opponent.name');

        const result = rule.check({
          user: { name: 'John' },
          opponent: { name: 'John' },
        });

        expect(result).toBe(AbilityMatch.match);
      });
    });

    describe('numeric comparisons', () => {
      it('should handle more_than condition',() => {
        const rule = AbilityRule.moreThan('user.age', 18);

        expect(rule.check({ user: { age: 25 } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { age: 18 } })).toBe(AbilityMatch.mismatch);
        expect(rule.check({ user: { age: 15 } })).toBe(AbilityMatch.mismatch);
      });

      it('should handle less_than condition', () => {
        const rule = AbilityRule.lessThan('user.age', 18);

        expect(rule.check({ user: { age: 15 } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { age: 18 } })).toBe(AbilityMatch.mismatch);
        expect(rule.check({ user: { age: 25 } })).toBe(AbilityMatch.mismatch);
      });

      it('should handle more_or_equal condition',() => {
        const rule = AbilityRule.moreOrEqual('user.age', 18);

        expect(rule.check({ user: { age: 25 } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { age: 18 } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { age: 15 } })).toBe(AbilityMatch.mismatch);
      });

      it('should handle less_or_equal condition', () => {
        const rule = AbilityRule.lessOrEqual('user.age', 18);

        expect(rule.check({ user: { age: 15 } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { age: 18 } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { age: 25 } })).toBe(AbilityMatch.mismatch);
      });
    });

    describe('array operations', () => {
      it('should handle in condition with array resource',() => {
        const rule = AbilityRule.in('user.role', ['admin', 'manager']);

        expect(rule.check({ user: { role: 'admin' } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { role: 'user' } })).toBe(AbilityMatch.mismatch);
      });

      it('should handle in condition with dot notation array',() => {
        const rule = AbilityRule.in('user.role', 'allowed.roles');

        const result = rule.check({
          user: { role: 'admin' },
          allowed: { roles: ['admin', 'manager'] },
        });

        expect(result).toBe(AbilityMatch.match);
      });

      it('should handle not_in condition', () => {
        const rule = AbilityRule.notIn('user.role', ['banned', 'blocked']);

        expect(rule.check({ user: { role: 'admin' } })).toBe(AbilityMatch.match);
        expect(rule.check({ user: { role: 'banned' } })).toBe(AbilityMatch.mismatch);
      });
    });

    describe('extractValues method', () => {
      it('should extract values from nested objects', () => {
        const rule = new AbilityRule({
          subject: 'user.profile.address.city',
          resource: 'default.city',
          condition: AbilityCondition.equals,
        });

        const [subjectValue, resourceValue] = rule.extractValues({
          user: {
            profile: {
              address: {
                city: 'Moscow',
              },
            },
          },
          default: {
            city: 'Moscow',
          },
        });

        expect(subjectValue).toBe('Moscow');
        expect(resourceValue).toBe('Moscow');
      });

      it('should handle array indices in path', () => {
        const rule = new AbilityRule({
          subject: 'users[0].name',
          resource: 'expected.name',
          condition: AbilityCondition.equals,
        });

        const [subjectValue] = rule.extractValues({
          users: [{ name: 'John' }, { name: 'Jane' }],
          expected: { name: 'John' },
        });

        expect(subjectValue).toBe('John');
      });

      it('should return [NaN, NaN] for null resource', () => {
        const rule = AbilityRule.equals('user.name', 'John');
        const [s, r] = rule.extractValues(null);

        expect(s).toBeNaN();
        expect(r).toBeNaN();
      });
    });
  });

  describe('getDotNotationValue', () => {
    class TestRule extends AbilityRule {
      public proxyGetDotNotation(o: unknown, p: string) {
        return this.getDotNotationValue(o, p);
      }
    }

    it('should extract value by dot notation', () => {
      const resource = {
        foo: {
          bar: {
            baz: {
              taz: 42,
            },
          },
        },
      };

      const rule = new TestRule({
        id: 'test',
        name: 'test',
        condition: AbilityCondition.equals,
        subject: 'test',
        resource: '',
      });

      const value = rule.proxyGetDotNotation(resource, 'foo.bar.baz.taz');
      expect(value).toBe(42);
    });

    it('should handle array notation', () => {
      const resource = {
        users: [{ name: 'John' }, { name: 'Jane' }],
      };

      const rule = new TestRule({
        id: 'test',
        name: 'test',
        condition: AbilityCondition.equals,
        subject: 'test',
        resource: '',
      });

      const value = rule.proxyGetDotNotation(resource, 'users[1].name');
      expect(value).toBe('Jane');
    });

    it('should return undefined for non-existent path', () => {
      const resource = { foo: { bar: 42 } };

      const rule = new TestRule({
        id: 'test',
        name: 'test',
        condition: AbilityCondition.equals,
        subject: 'test',
        resource: '',
      });

      const value = rule.proxyGetDotNotation(resource, 'foo.baz.qux');
      expect(value).toBeUndefined();
    });
  });

  describe('static factory methods', () => {
    it('should create equal rule', () => {
      const rule = AbilityRule.equals('user.name', 'John');
      expect(rule.condition).toBe(AbilityCondition.equals);
      expect(rule.subject).toBe('user.name');
      expect(rule.resource).toBe('John');
    });

    it('should create notEqual rule', () => {
      const rule = AbilityRule.notEqual('user.name', 'John');
      expect(rule.condition).toBe(AbilityCondition.not_equals);
    });

    it('should create in rule', () => {
      const rule = AbilityRule.in('user.role', ['admin', 'manager']);
      expect(rule.condition).toBe(AbilityCondition.in);
    });

    it('should create notIn rule', () => {
      const rule = AbilityRule.notIn('user.role', ['banned']);
      expect(rule.condition).toBe(AbilityCondition.not_in);
    });

    it('should create numeric comparison rules', () => {
      expect(AbilityRule.lessThan('user.age', 18).condition).toBe(AbilityCondition.less_than);
      expect(AbilityRule.lessOrEqual('user.age', 18).condition).toBe(
        AbilityCondition.less_or_equal,
      );
      expect(AbilityRule.moreThan('user.age', 18).condition).toBe(AbilityCondition.greater_than);
      expect(AbilityRule.moreOrEqual('user.age', 18).condition).toBe(
        AbilityCondition.greater_or_equal,
      );
    });
  });
});
