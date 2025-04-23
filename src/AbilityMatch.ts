import AbilityCode from './AbilityCode';

class AbilityMatch extends AbilityCode{
  public static PENDING = new AbilityMatch(0);
  public static MATCH = new AbilityMatch(1);
  public static MISMATCH = new AbilityMatch(2);
}

export default AbilityMatch;