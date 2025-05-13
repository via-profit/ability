import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import AbilityCompare, { AbilityCompareLiteralType } from './AbilityCompare';
import AbilityMatch from './AbilityMatch';
export type AbilityRuleSetConfig = {
    readonly id: string;
    readonly name: string;
    readonly compareMethod: AbilityCompareLiteralType;
    readonly rules: readonly AbilityRuleConfig[];
};
export declare class AbilityRuleSet<Resources extends object = object> {
    state: AbilityMatch;
    /**
     * List of rules
     */
    rules: AbilityRule[];
    /**
     * Rules compare method.\
     * For the «and» method the rule will be permitted if all\
     * rules will be returns «permit» status and for the «or» - if\
     * one of the rules returns as «permit»
     */
    compareMethod: AbilityCompare;
    /**
     * Group name
     */
    name: string;
    /**
     * Group ID
     */
    id: string;
    constructor(params: Pick<AbilityRuleSetConfig, 'id' | 'name' | 'compareMethod'>);
    addRule(rule: AbilityRule, compareMethod: AbilityCompare): this;
    addRules(rules: AbilityRule[], compareMethod: AbilityCompare): this;
    check(resources: Resources | null): AbilityMatch;
    /**
     * Parse the config JSON format to Group class instance
     */
    static parse<Resource extends object = object>(config: AbilityRuleSetConfig): AbilityRuleSet<Resource>;
    export(): AbilityRuleSetConfig;
}
export default AbilityRuleSet;
