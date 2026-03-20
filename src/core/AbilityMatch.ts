import AbilityCode from './AbilityCode';

export type AbilityMatchCodeType = 'pending' | 'match' | 'mismatch';

export class AbilityMatch extends AbilityCode<AbilityMatchCodeType>{
  public static pending = new AbilityMatch('pending');
  public static match = new AbilityMatch('match');
  public static mismatch = new AbilityMatch('mismatch');
}

export default AbilityMatch;