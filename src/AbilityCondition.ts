import AbilityCode from '~/AbilityCode';

class AbilityCondition extends AbilityCode<string> {
  public static EQUAL = new AbilityCondition('=');
  public static NOT_EQUAL = new AbilityCondition('<>');
  public static MORE_THAN = new AbilityCondition('>');
  public static LESS_THAN = new AbilityCondition('<');
  public static LESS_OR_EQUAL = new AbilityCondition('<=');
  public static MORE_OR_EQUAL = new AbilityCondition('>=');
  public static IN = new AbilityCondition('in');
  public static NOT_IN = new AbilityCondition('not in');
}

export default AbilityCondition;
