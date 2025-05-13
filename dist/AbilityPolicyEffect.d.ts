import AbilityCode from './AbilityCode';
export type AbilityPolicyEffectCodeType = 'deny' | 'permit';
export declare class AbilityPolicyEffect extends AbilityCode<AbilityPolicyEffectCodeType> {
    static deny: AbilityPolicyEffect;
    static permit: AbilityPolicyEffect;
}
export default AbilityPolicyEffect;
