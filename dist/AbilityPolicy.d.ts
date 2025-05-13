import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import AbilityMatch from './AbilityMatch';
import AbilityCompare, { AbilityCompareCodeType } from './AbilityCompare';
import AbilityPolicyEffect, { AbilityPolicyEffectCodeType } from './AbilityPolicyEffect';
export type AbilityPolicyConfig = {
    readonly action: string;
    readonly effect: AbilityPolicyEffectCodeType;
    readonly compareMethod: AbilityCompareCodeType;
    readonly ruleSet: readonly AbilityRuleSetConfig[];
    readonly id: string;
    readonly name: string;
};
export declare class AbilityPolicy<Resources extends object = object> {
    matchState: AbilityMatch;
    /**
     * List of rules
     */
    ruleSet: AbilityRuleSet[];
    /**
     * Policy effect
     */
    effect: AbilityPolicyEffect;
    /**
     * Rules compare method.\
     * For the «and» method the rule will be permitted if all\
     * rules will be returns «permit» status and for the «or» - if\
     * one of the rules returns as «permit»
     */
    compareMethod: AbilityCompare;
    /**
     * Policy name
     */
    name: string;
    /**
     * Policy ID
     */
    id: string;
    /**
     * Soon
     */
    action: string;
    constructor(params: {
        id: string;
        name: string;
        action: string;
        effect: AbilityPolicyEffect;
    });
    /**
     * Add rule set to the policy
     * @param ruleSet - The rule set to add
     */
    addRuleSet(ruleSet: AbilityRuleSet): this;
    /**
     * Check if the policy is matched
     * @param resources - The resource to check
     */
    check(resources: Resources): AbilityMatch;
    /**
     * Parse the config JSON format to Policy class instance
     */
    static parse<Resources extends object = object>(config: AbilityPolicyConfig): AbilityPolicy<Resources>;
    export(): AbilityPolicyConfig;
}
export default AbilityPolicy;
