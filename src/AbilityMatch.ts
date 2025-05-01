import AbilityCode from './AbilityCode';

export type AbilityMatchVariantType = 'pending' | 'match' | 'mismatch';

export class AbilityMatch extends AbilityCode<AbilityMatchVariantType>{
  public static pending = new AbilityMatch('pending');
  public static match = new AbilityMatch('match');
  public static mismatch = new AbilityMatch('mismatch');
}

export default AbilityMatch;