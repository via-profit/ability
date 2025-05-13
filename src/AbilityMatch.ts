import AbilityCode from './AbilityCode';

export type AbilityMatchLiteralType = 'pending' | 'match' | 'mismatch';

export class AbilityMatch extends AbilityCode<AbilityMatchLiteralType>{
  public static pending = new AbilityMatch('pending');
  public static match = new AbilityMatch('match');
  public static mismatch = new AbilityMatch('mismatch');

  public static fromLiteral(literal: AbilityMatchLiteralType): AbilityMatch {
    return new this(this[literal].code);
  }
}

export default AbilityMatch;