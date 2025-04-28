import AbilityCode from './AbilityCode';

export class AbilityCompare extends AbilityCode {
  public static OR = new AbilityCompare(0);
  public static AND = new AbilityCompare(1);
}

export default AbilityCompare;
