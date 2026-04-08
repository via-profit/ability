import {
  AbilityCondition,
  AbilityConditionType,
  fromLiteral,
  toLiteral,
  isConditionEqual,
  isConditionNotEqual,
} from '../../src/core/AbilityCondition';
import { AbilityParserError } from '../../src/core/AbilityError';

describe('AbilityCondition (branded literal enum)', () => {
  describe('enum object structure', () => {
    it('should expose all condition values as branded literals', () => {
      expect(AbilityCondition.equals).toBe('=');
      expect(AbilityCondition.not_equals).toBe('<>');
      expect(AbilityCondition.greater_than).toBe('>');
      expect(AbilityCondition.less_than).toBe('<');
      expect(AbilityCondition.less_or_equal).toBe('<=');
      expect(AbilityCondition.greater_or_equal).toBe('>=');
      expect(AbilityCondition.in).toBe('in');
      expect(AbilityCondition.not_in).toBe('not in');
    });

    it('should have exactly 8 basic comparison conditions', () => {
      const list = [
        AbilityCondition.equals,
        AbilityCondition.not_equals,
        AbilityCondition.greater_than,
        AbilityCondition.less_than,
        AbilityCondition.less_or_equal,
        AbilityCondition.greater_or_equal,
        AbilityCondition.in,
        AbilityCondition.not_in,
      ];

      expect(list).toHaveLength(8);
    });
  });

  describe('branding', () => {
    it('should produce branded literal types', () => {
      const c: AbilityConditionType = AbilityCondition.equals;
      expect(c).toBe('=');
    });

    it('should reject plain strings', () => {
      // @ts-expect-error — plain string is not branded
      const c: AbilityConditionType = '=';
      expect(c).toBeDefined();
    });
  });

  describe('fromLiteral()', () => {
    it('should map literals to branded conditions', () => {
      expect(fromLiteral('equals')).toBe(AbilityCondition.equals);
      expect(fromLiteral('not_equals')).toBe(AbilityCondition.not_equals);
      expect(fromLiteral('greater_than')).toBe(AbilityCondition.greater_than);
      expect(fromLiteral('less_than')).toBe(AbilityCondition.less_than);
      expect(fromLiteral('less_or_equal')).toBe(AbilityCondition.less_or_equal);
      expect(fromLiteral('greater_or_equal')).toBe(AbilityCondition.greater_or_equal);
      expect(fromLiteral('in')).toBe(AbilityCondition.in);
      expect(fromLiteral('not_in')).toBe(AbilityCondition.not_in);
    });

    it('should throw AbilityParserError for invalid literal', () => {
      expect(() => fromLiteral('invalid' as any)).toThrow(AbilityParserError);
      expect(() => fromLiteral('invalid' as any)).toThrow(
        'Literal "invalid" does not found in AbilityCondition'
      );
    });

    it('should be case sensitive', () => {
      expect(() => fromLiteral('EQUALS' as any)).toThrow(AbilityParserError);
      expect(() => fromLiteral('Not_Equal' as any)).toThrow(AbilityParserError);
    });
  });

  describe('toLiteral()', () => {
    it('should convert branded conditions back to literals', () => {
      expect(toLiteral(AbilityCondition.equals)).toBe('equals');
      expect(toLiteral(AbilityCondition.not_equals)).toBe('not_equals');
      expect(toLiteral(AbilityCondition.greater_than)).toBe('greater_than');
      expect(toLiteral(AbilityCondition.less_than)).toBe('less_than');
      expect(toLiteral(AbilityCondition.less_or_equal)).toBe('less_or_equal');
      expect(toLiteral(AbilityCondition.greater_or_equal)).toBe('greater_or_equal');
      expect(toLiteral(AbilityCondition.in)).toBe('in');
      expect(toLiteral(AbilityCondition.not_in)).toBe('not_in');
    });
  });

  describe('comparison helpers', () => {
    it('isConditionEqual should compare branded values', () => {
      expect(isConditionEqual(AbilityCondition.equals, AbilityCondition.equals)).toBe(true);
      expect(isConditionEqual(AbilityCondition.equals, AbilityCondition.not_equals)).toBe(false);
    });

    it('isConditionNotEqual should compare branded values', () => {
      expect(isConditionNotEqual(AbilityCondition.equals, AbilityCondition.equals)).toBe(false);
      expect(isConditionNotEqual(AbilityCondition.equals, AbilityCondition.not_equals)).toBe(true);
    });
  });

  describe('switch compatibility', () => {
    it('should work in switch statements', () => {
      const cond = AbilityCondition.equals;
      let result = '';

      switch (cond) {
        case AbilityCondition.equals:
          result = 'equality';
          break;
        case AbilityCondition.not_equals:
          result = 'inequality';
          break;
        default:
          result = 'other';
      }

      expect(result).toBe('equality');
    });
  });

  describe('serialization', () => {
    it('should serialize to plain string', () => {
      expect(JSON.stringify(AbilityCondition.equals)).toBe('"="');
      expect(JSON.stringify(AbilityCondition.not_in)).toBe('"not in"');
    });
  });

  describe('edge cases', () => {
    it('should preserve singleton identity', () => {
      expect(AbilityCondition.equals).toBe(AbilityCondition.equals);
      expect(AbilityCondition.equals).not.toBe(AbilityCondition.not_equals);
    });

    it('should map literal → branded → literal', () => {
      const c = fromLiteral('greater_than');
      expect(toLiteral(c)).toBe('greater_than');
      expect(c).toBe('>');
    });
  });
});
