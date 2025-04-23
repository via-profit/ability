import AbilityCode from '~/AbilityCode';

class AbilityPolicyEffect extends AbilityCode{
  public static DENY = new AbilityPolicyEffect(0);
  public static PERMIT = new AbilityPolicyEffect(1);
}

export default  AbilityPolicyEffect;