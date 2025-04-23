import AbilityCode from './AbilityCode';

class AbilityMatch extends AbilityCode{
  public static PENDING = new AbilityMatch(2);
  public static MATCH = new AbilityMatch(1);
  public static MISMATCH = new AbilityMatch(0);
}

export default AbilityMatch;