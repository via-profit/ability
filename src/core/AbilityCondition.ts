import AbilityCode from './AbilityCode';
import { AbilityParserError } from '../core/AbilityError';

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
  // plain values
  | 'equals'
  | 'not_equals'

  // arrays
  | 'contains'
  | 'no_contains'
  | 'in'
  | 'not_in'
  // numeric
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
  public static equals = new AbilityCondition('=');
  public static not_equals = new AbilityCondition('<>');
  public static greater_than = new AbilityCondition('>');
  public static less_than = new AbilityCondition('<');
  public static less_or_equal = new AbilityCondition('<=');
  public static greater_or_equal = new AbilityCondition('>=');
  public static in = new AbilityCondition('in');
  public static not_in = new AbilityCondition('not in');
  public static contains = new AbilityCondition('contains');
  public static not_contains = new AbilityCondition('not contains');
  public static length_greater_than = new AbilityCondition('length greater than');
  public static length_less_than = new AbilityCondition('length less than');
  public static length_equals = new AbilityCondition('length equals');
  public static always = new AbilityCondition('always');
  public static never = new AbilityCondition('never');

  public static fromLiteral(literal: AbilityConditionLiteralType): AbilityCondition {
    switch (literal) {
      case 'equals':
        return this.equals;
      case 'not_equals':
        return this.not_equals;
      case 'greater_than':
        return this.greater_than;
      case 'less_than':
        return this.less_than;
      case 'less_or_equal':
        return this.less_or_equal;
      case 'greater_or_equal':
        return this.greater_or_equal;
      case 'contains':
        return this.contains;
      case 'no_contains':
        return this.not_contains;
      case 'in':
        return this.in;
      case 'not_in':
        return this.not_in;
      case 'length_greater_than':
        return this.length_greater_than;
      case 'length_equals':
        return this.length_equals;
      case 'always':
        return this.always;
      case 'never':
        return this.never;
      default:
        throw new AbilityParserError(`Literal ${literal} does not found in AbilityCondition class`);
    }
  }

  public get literal() {
    const literal = Object.keys(AbilityCondition).find(member => {
      const val = AbilityCondition[member as keyof typeof AbilityCondition] as AbilityCondition;
      return val.code === this.code;
    }) as AbilityConditionLiteralType;

    if (typeof literal === 'undefined') {
      throw new Error(`Literal value does not found in class AbilityCondition`);
    }

    return literal;
  }
}

export default AbilityCondition;
