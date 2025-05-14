import AbilityCode from './AbilityCode';
import { AbilityParserError } from '~/AbilityError';

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

export class AbilityCondition extends AbilityCode<AbilityConditionCodeType> {
  public static equal = new AbilityCondition('=');
  public static not_equal = new AbilityCondition('<>');
  public static more_than = new AbilityCondition('>');
  public static less_than = new AbilityCondition('<');
  public static less_or_equal = new AbilityCondition('<=');
  public static more_or_equal = new AbilityCondition('>=');
  public static in = new AbilityCondition('in');
  public static not_in = new AbilityCondition('not in');

  public static fromLiteral(literal: AbilityConditionLiteralType): AbilityCondition {
    const code = AbilityCondition[literal]?.code || null;
    if (code === null) {
      throw new AbilityParserError(`Literal ${literal} does not found in AbilityCondition class`);
    }

    return new AbilityCondition(code);
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
