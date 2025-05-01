import AbilityCode from './AbilityCode';

export type AbilityCompareVariantType = 'and' | 'or'

export class AbilityCompare extends AbilityCode {
  public static and = new AbilityCompare(0);
  public static or = new AbilityCompare(1);
}

export default AbilityCompare;
