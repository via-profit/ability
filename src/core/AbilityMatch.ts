import AbilityCode from '~/core/AbilityCode';

export type AbilityMatchCodeType = 'pending' | 'match' | 'mismatch' | 'except-mismatch';

export class AbilityMatch extends AbilityCode<AbilityMatchCodeType> {
  public static pending = new AbilityMatch('pending');
  public static match = new AbilityMatch('match');
  public static mismatch = new AbilityMatch('mismatch');
  public static exceptMismatch = new AbilityMatch('except-mismatch');
}

export default AbilityMatch;
