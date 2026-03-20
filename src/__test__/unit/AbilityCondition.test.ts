/* eslint-disable @typescript-eslint/no-explicit-any */

import AbilityCondition from '../../core/AbilityCondition';
import { AbilityParserError } from '../../core/AbilityError';

describe('AbilityCondition', () => {
  describe('static properties', () => {
    it('should have all condition types defined', () => {
      expect(AbilityCondition.equal).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.equal.code).toBe('=');

      expect(AbilityCondition.not_equal).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.not_equal.code).toBe('<>');

      expect(AbilityCondition.more_than).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.more_than.code).toBe('>');

      expect(AbilityCondition.less_than).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.less_than.code).toBe('<');

      expect(AbilityCondition.less_or_equal).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.less_or_equal.code).toBe('<=');

      expect(AbilityCondition.more_or_equal).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.more_or_equal.code).toBe('>=');

      expect(AbilityCondition.in).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.in.code).toBe('in');

      expect(AbilityCondition.not_in).toBeInstanceOf(AbilityCondition);
      expect(AbilityCondition.not_in.code).toBe('not in');
    });

    it('should have exactly 8 condition types', () => {
      const conditions = [
        AbilityCondition.equal,
        AbilityCondition.not_equal,
        AbilityCondition.more_than,
        AbilityCondition.less_than,
        AbilityCondition.less_or_equal,
        AbilityCondition.more_or_equal,
        AbilityCondition.in,
        AbilityCondition.not_in,
      ];

      expect(conditions).toHaveLength(8);
    });
  });

  describe('constructor', () => {
    it('should create condition with code', () => {
      const condition = new AbilityCondition('=');
      expect(condition.code).toBe('=');
    });

    it('should create condition with different codes', () => {
      const conditions = [
        new AbilityCondition('='),
        new AbilityCondition('<>'),
        new AbilityCondition('>'),
        new AbilityCondition('<'),
        new AbilityCondition('<='),
        new AbilityCondition('>='),
        new AbilityCondition('in'),
        new AbilityCondition('not in'),
      ];

      expect(conditions[0].code).toBe('=');
      expect(conditions[1].code).toBe('<>');
      expect(conditions[2].code).toBe('>');
      expect(conditions[3].code).toBe('<');
      expect(conditions[4].code).toBe('<=');
      expect(conditions[5].code).toBe('>=');
      expect(conditions[6].code).toBe('in');
      expect(conditions[7].code).toBe('not in');
    });
  });

  describe('isEqual method', () => {
    it('should return true for equal conditions', () => {
      const condition1 = new AbilityCondition('=');
      const condition2 = new AbilityCondition('=');

      expect(condition1.isEqual(condition2)).toBe(true);
    });

    it('should return false for different conditions', () => {
      const condition1 = new AbilityCondition('=');
      const condition2 = new AbilityCondition('<>');

      expect(condition1.isEqual(condition2)).toBe(false);
    });

    it('should return false when comparing with null', () => {
      const condition = new AbilityCondition('=');

      expect(condition.isEqual(null)).toBe(false);
    });

    it('should work with static instances', () => {
      expect(AbilityCondition.equal.isEqual(AbilityCondition.equal)).toBe(true);
      expect(AbilityCondition.equal.isEqual(AbilityCondition.not_equal)).toBe(false);
      expect(AbilityCondition.in.isEqual(AbilityCondition.in)).toBe(true);
      expect(AbilityCondition.in.isEqual(AbilityCondition.not_in)).toBe(false);
    });
  });

  describe('isNotEqual method', () => {
    it('should return false for equal conditions', () => {
      const condition1 = new AbilityCondition('=');
      const condition2 = new AbilityCondition('=');

      expect(condition1.isNotEqual(condition2)).toBe(false);
    });

    it('should return true for different conditions', () => {
      const condition1 = new AbilityCondition('=');
      const condition2 = new AbilityCondition('<>');

      expect(condition1.isNotEqual(condition2)).toBe(true);
    });

    it('should return true when comparing with null', () => {
      const condition = new AbilityCondition('=');

      expect(condition.isNotEqual(null)).toBe(true);
    });

    it('should work with static instances', () => {
      expect(AbilityCondition.equal.isNotEqual(AbilityCondition.equal)).toBe(false);
      expect(AbilityCondition.equal.isNotEqual(AbilityCondition.not_equal)).toBe(true);
    });
  });

  describe('fromLiteral method', () => {
    it('should create condition from valid literals', () => {
      expect(AbilityCondition.fromLiteral('equal').code).toBe('=');
      expect(AbilityCondition.fromLiteral('not_equal').code).toBe('<>');
      expect(AbilityCondition.fromLiteral('more_than').code).toBe('>');
      expect(AbilityCondition.fromLiteral('less_than').code).toBe('<');
      expect(AbilityCondition.fromLiteral('less_or_equal').code).toBe('<=');
      expect(AbilityCondition.fromLiteral('more_or_equal').code).toBe('>=');
      expect(AbilityCondition.fromLiteral('in').code).toBe('in');
      expect(AbilityCondition.fromLiteral('not_in').code).toBe('not in');
    });

    it('should return same instance for static literals', () => {
      expect(AbilityCondition.fromLiteral('equal')).toBe(AbilityCondition.equal);
      expect(AbilityCondition.fromLiteral('not_equal')).toBe(AbilityCondition.not_equal);
      expect(AbilityCondition.fromLiteral('in')).toBe(AbilityCondition.in);
      expect(AbilityCondition.fromLiteral('not_in')).toBe(AbilityCondition.not_in);
    });

    it('should throw AbilityParserError for invalid literal', () => {
      expect(() => AbilityCondition.fromLiteral('invalid' as any)).toThrow(AbilityParserError);

      expect(() => AbilityCondition.fromLiteral('invalid' as any)).toThrow(
        'Literal invalid does not found in AbilityCondition class',
      );
    });

    it('should throw for empty string literal', () => {
      expect(() => AbilityCondition.fromLiteral('' as any)).toThrow(AbilityParserError);
    });

    it('should be case sensitive', () => {
      expect(() => AbilityCondition.fromLiteral('EQUAL' as any)).toThrow(AbilityParserError);

      expect(() => AbilityCondition.fromLiteral('Not_Equal' as any)).toThrow(AbilityParserError);
    });
  });

  describe('literal getter', () => {
    it('should return correct literal for code', () => {
      expect(new AbilityCondition('=').literal).toBe('equal');
      expect(new AbilityCondition('<>').literal).toBe('not_equal');
      expect(new AbilityCondition('>').literal).toBe('more_than');
      expect(new AbilityCondition('<').literal).toBe('less_than');
      expect(new AbilityCondition('<=').literal).toBe('less_or_equal');
      expect(new AbilityCondition('>=').literal).toBe('more_or_equal');
      expect(new AbilityCondition('in').literal).toBe('in');
      expect(new AbilityCondition('not in').literal).toBe('not_in');
    });

    it('should work with static instances', () => {
      expect(AbilityCondition.equal.literal).toBe('equal');
      expect(AbilityCondition.not_equal.literal).toBe('not_equal');
      expect(AbilityCondition.more_than.literal).toBe('more_than');
      expect(AbilityCondition.less_than.literal).toBe('less_than');
      expect(AbilityCondition.less_or_equal.literal).toBe('less_or_equal');
      expect(AbilityCondition.more_or_equal.literal).toBe('more_or_equal');
      expect(AbilityCondition.in.literal).toBe('in');
      expect(AbilityCondition.not_in.literal).toBe('not_in');
    });
  });

  describe('inheritance from AbilityCode', () => {
    it('should inherit isEqual method from AbilityCode', () => {
      expect(AbilityCondition.equal.isEqual).toBeDefined();
      expect(AbilityCondition.equal.isEqual(AbilityCondition.equal)).toBe(true);
    });

    it('should inherit isNotEqual method from AbilityCode', () => {
      expect(AbilityCondition.equal.isNotEqual).toBeDefined();
      expect(AbilityCondition.equal.isNotEqual(AbilityCondition.not_equal)).toBe(true);
    });

    it('should inherit code getter from AbilityCode', () => {
      expect(AbilityCondition.equal.code).toBeDefined();
      expect(AbilityCondition.equal.code).toBe('=');
    });
  });

  describe('type safety', () => {
    it('should have correct TypeScript literal types', () => {
      const equal: AbilityCondition = AbilityCondition.equal;
      const notEqual: AbilityCondition = AbilityCondition.not_equal;
      const in_: AbilityCondition = AbilityCondition.in;
      const notIn: AbilityCondition = AbilityCondition.not_in;

      expect(equal.code).toBe('=');
      expect(notEqual.code).toBe('<>');
      expect(in_.code).toBe('in');
      expect(notIn.code).toBe('not in');
    });

    it('should work in switch statements', () => {
      const condition = AbilityCondition.equal;
      let result = '';

      switch (condition) {
        case AbilityCondition.equal:
          result = 'equality';
          break;
        case AbilityCondition.not_equal:
          result = 'inequality';
          break;
        default:
          result = 'other';
      }

      expect(result).toBe('equality');
    });

    it('should be usable in conditional logic', () => {
      const isEquality = (cond: AbilityCondition) => cond.isEqual(AbilityCondition.equal);

      expect(isEquality(AbilityCondition.equal)).toBe(true);
      expect(isEquality(AbilityCondition.not_equal)).toBe(false);
      expect(isEquality(AbilityCondition.in)).toBe(false);
    });
  });

  describe('edge cases', () => {
    it('should handle special characters in codes', () => {
      expect(AbilityCondition.not_in.code).toBe('not in');
      expect(AbilityCondition.not_equal.code).toBe('<>');
    });

    it('should maintain singleton instances', () => {
      // Статические свойства должны быть одним и тем же экземпляром
      expect(AbilityCondition.equal).toBe(AbilityCondition.equal);
      expect(AbilityCondition.equal).not.toBe(AbilityCondition.not_equal);

      // Новые экземпляры с тем же кодом не равны статическим
      const newEqual = new AbilityCondition('=');
      expect(newEqual).not.toBe(AbilityCondition.equal);
      expect(newEqual.isEqual(AbilityCondition.equal)).toBe(true);
    });

    it('should work with fromLiteral and then literal', () => {
      const condition = AbilityCondition.fromLiteral('more_than');
      expect(condition.literal).toBe('more_than');
      expect(condition.code).toBe('>');
    });
  });
});
