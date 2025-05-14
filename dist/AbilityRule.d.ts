import AbilityMatch from './AbilityMatch';
import AbilityCondition, { AbilityConditionCodeType } from './AbilityCondition';
export type AbilityRuleConfig = {
    readonly id: string;
    readonly name: string;
    /**
     * Subject key path like a 'user.name'
     */
    readonly subject: string;
    /**
     * Resource key path like a 'user.name' or value
     */
    readonly resource: string | number | boolean | (string | number)[];
    readonly condition: AbilityConditionCodeType;
};
export type AbilityRuleConstructorProps = AbilityRuleConfig;
export declare class AbilityRule<Resources extends object = object> {
    /**
     * Subject key path like a 'user.name'
     */
    subject: string;
    /**
     * Resource key path like a 'user.name' or value
     */
    resource: string | number | boolean | (string | number)[];
    condition: AbilityCondition;
    name: string;
    id: string;
    state: AbilityMatch;
    constructor(params: AbilityRuleConstructorProps);
    /**
     * Check if the rule is matched
     * @param resource - The resource to check
     */
    check(resource: Resources | null): AbilityMatch;
    /**
     * Extract values from the resourceData
     * @param resourceData - The resourceData to extract values from
     */
    extractValues(resourceData: Resources | null): [
        string | number | boolean | (string | number)[] | null | undefined,
        string | number | boolean | (string | number)[] | null | undefined
    ];
    /**
     * Get the value of the object by dot notation
     * @param resource - The object to get the value from
     * @param desc - The dot notation string
     */
    getDotNotationValue<T = unknown>(resource: unknown, desc: string): T | undefined;
    static parse<Resources extends object>(config: AbilityRuleConfig): AbilityRule<Resources>;
    /**
     * Export the rule to config object
     */
    export(): AbilityRuleConfig;
}
export default AbilityRule;
