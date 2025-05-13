import AbilityCode from './AbilityCode';

export type AbilityCompareLiteralType = 'and' | 'or'

export class AbilityCompare extends AbilityCode<AbilityCompareLiteralType> {
  public static and = new AbilityCompare('and');
  public static or = new AbilityCompare('or');

  public static fromLiteral(literal: AbilityCompareLiteralType): AbilityCompare {
    return new this(this[literal].code);
  }
}

export default AbilityCompare;
