import AbilityCode from '~/core/AbilityCode';

export type AbilityCompareCodeType = 'and' | 'or'

export class AbilityCompare extends AbilityCode<AbilityCompareCodeType> {
  public static and = new AbilityCompare('and');
  public static or = new AbilityCompare('or');
}

export default AbilityCompare;
