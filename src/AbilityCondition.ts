import AbilityCode from './AbilityCode';

export type AbilityConditionCodeType = '=' | '<>' | '>' | '<' | '>=' | '<=' | 'in' | 'not in';
export type AbilityConditionLiteralType =
  | 'equal'
  | 'not_equal'
  | 'more_than'
  | 'less_than'
  | 'less_or_equal'
  | 'more_or_equal'
  | 'in'
  | 'not_in';

export class AbilityCondition extends AbilityCode<
  AbilityConditionLiteralType,
  AbilityConditionCodeType
> {
  public static equal = new AbilityCondition('=');
  public static not_equal = new AbilityCondition('<>');
  public static more_than = new AbilityCondition('>');
  public static less_than = new AbilityCondition('<');
  public static less_or_equal = new AbilityCondition('<=');
  public static more_or_equal = new AbilityCondition('>=');
  public static in = new AbilityCondition('in');
  public static not_in = new AbilityCondition('not in');

  public static fromLiteral(literal: AbilityConditionLiteralType): AbilityCondition {
    return new this(this[literal].code);
  }
}

export default AbilityCondition;
