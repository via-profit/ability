import AbilityCode from './AbilityCode';
import { AbilityParserError } from './AbilityError';

export type AbilityConditionCodeType =
  | '='
  | '<>'
  | '>'
  | '<'
  | '>='
  | '<='
  | 'in'
  | 'not in'
  | 'contains'
  | 'not contains'
  | 'length greater than'
  | 'length less than'
  | 'length equals'
  | 'always'
  | 'never';

export type AbilityConditionLiteralType =
  | 'equals'
  | 'not_equals'
  | 'contains'
  | 'not_contains'
  | 'in'
  | 'not_in'
  | 'greater_than'
  | 'less_than'
  | 'less_or_equal'
  | 'greater_or_equal'
  | 'length_greater_than'
  | 'length_less_than'
  | 'length_equals'
  | 'always'
  | 'never';

export class AbilityCondition extends AbilityCode<AbilityConditionCodeType> {
  public static equals: AbilityCondition;
  public static not_equals: AbilityCondition;
  public static greater_than: AbilityCondition;
  public static less_than: AbilityCondition;
  public static less_or_equal: AbilityCondition;
  public static greater_or_equal: AbilityCondition;
  public static in: AbilityCondition;
  public static not_in: AbilityCondition;
  public static contains: AbilityCondition;
  public static not_contains: AbilityCondition;
  public static length_greater_than: AbilityCondition;
  public static length_less_than: AbilityCondition;
  public static length_equals: AbilityCondition;
  public static always: AbilityCondition;
  public static never: AbilityCondition;

  static {
    this.equals = new AbilityCondition('=');
    this.not_equals = new AbilityCondition('<>');
    this.greater_than = new AbilityCondition('>');
    this.less_than = new AbilityCondition('<');
    this.less_or_equal = new AbilityCondition('<=');
    this.greater_or_equal = new AbilityCondition('>=');
    this.in = new AbilityCondition('in');
    this.not_in = new AbilityCondition('not in');
    this.contains = new AbilityCondition('contains');
    this.not_contains = new AbilityCondition('not contains');
    this.length_greater_than = new AbilityCondition('length greater than');
    this.length_less_than = new AbilityCondition('length less than');
    this.length_equals = new AbilityCondition('length equals');
    this.always = new AbilityCondition('always');
    this.never = new AbilityCondition('never');
  }

  public static fromLiteral(literal: AbilityConditionLiteralType): AbilityCondition {
    const map: Record<AbilityConditionLiteralType, AbilityCondition> = {
      equals: this.equals,
      not_equals: this.not_equals,
      greater_than: this.greater_than,
      less_than: this.less_than,
      less_or_equal: this.less_or_equal,
      greater_or_equal: this.greater_or_equal,
      in: this.in,
      not_in: this.not_in,
      contains: this.contains,
      not_contains: this.not_contains,
      length_greater_than: this.length_greater_than,
      length_equals: this.length_equals,
      always: this.always,
      never: this.never,
      length_less_than: this.length_less_than,
    };

    const condition = map[literal];
    if (!condition) {
      throw new AbilityParserError(`Literal "${literal}" does not found in AbilityCondition class`);
    }
    return condition;
  }

  public get literal(): AbilityConditionLiteralType {
    switch (this.code) {
      case '=':
        return 'equals';
      case '<>':
        return 'not_equals';
      case '>':
        return 'greater_than';
      case '<':
        return 'less_than';
      case '>=':
        return 'greater_or_equal';
      case '<=':
        return 'less_or_equal';
      case 'in':
        return 'in';
      case 'not in':
        return 'not_in';
      case 'contains':
        return 'contains';
      case 'not contains':
        return 'not_contains';
      case 'length greater than':
        return 'length_greater_than';
      case 'length less than':
        return 'length_less_than';
      case 'length equals':
        return 'length_equals';
      case 'always':
        return 'always';
      case 'never':
        return 'never';
      default:
        throw new Error(`Unknown condition code: ${String(this.code)}`);
    }
  }
}

export default AbilityCondition;
