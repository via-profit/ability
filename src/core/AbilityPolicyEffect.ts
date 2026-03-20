import AbilityCode from './AbilityCode';

export type AbilityPolicyEffectCodeType = 'deny' | 'permit';

export class AbilityPolicyEffect extends AbilityCode<AbilityPolicyEffectCodeType> {
  public static deny = new AbilityPolicyEffect('deny');
  public static permit = new AbilityPolicyEffect('permit');
}

export default AbilityPolicyEffect;
