import AbilityCode from './AbilityCode';
export type AbilityPolicyEffectVariantType = 'deny' | 'permit';
export declare class AbilityPolicyEffect extends AbilityCode<AbilityPolicyEffectVariantType> {
    static deny: AbilityPolicyEffect;
    static permit: AbilityPolicyEffect;
    static fromLiteral(literal: AbilityPolicyEffectVariantType): AbilityPolicyEffect;
}
export default AbilityPolicyEffect;
