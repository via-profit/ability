import AbilityCode from '~/core/AbilityCode';

export type AbilityResultStateCodeType = 'allow' | 'deny' | 'neutral';

export class AbilityResultState extends AbilityCode<AbilityResultStateCodeType> {
  public static allow = new AbilityResultState('allow');
  public static deny = new AbilityResultState('deny');
  public static neutral = new AbilityResultState('neutral');
}

export default AbilityResultState;
