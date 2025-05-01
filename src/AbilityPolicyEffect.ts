import AbilityCode from './AbilityCode';

export type AbilityPolicyEffectVariantType = 'deny' | 'permit';

export class AbilityPolicyEffect extends AbilityCode<AbilityPolicyEffectVariantType> {
  public static deny = new AbilityPolicyEffect('deny');
  public static permit = new AbilityPolicyEffect('permit');
}

export default AbilityPolicyEffect;
