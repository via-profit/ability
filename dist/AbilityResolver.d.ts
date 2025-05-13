import AbilityPolicy from './AbilityPolicy';
import AbilityPolicyEffect from './AbilityPolicyEffect';
export declare class AbilityResolver<Resources extends object = object> {
    policies: readonly AbilityPolicy<Resources>[];
    constructor(policyOrListOfPolicies: readonly AbilityPolicy<Resources>[] | AbilityPolicy<Resources>);
    /**
     * Resolve policy for the resource and action
     *
     @param action - Action
     * @param resource - Resource
     */
    resolve<Action extends keyof Resources>(action: Action, resource: Resources[Action]): this;
    enforce<Action extends keyof Resources>(action: Action, resource: Resources[Action]): void | never;
    /**
     * Get the last effect of the policy
     *
     * @returns {AbilityPolicyEffect | null}
     */
    getEffect(): AbilityPolicyEffect | null;
    isPermit(): boolean;
    isDeny(): boolean;
    getMatchedPolicy(): AbilityPolicy<Resources> | null;
    /**
     * Check if the action is contained in another action
     * @param actionA - The first action to check
     * @param actionB - The second action to check
     */
    static isInActionContain(actionA: string, actionB: string): boolean;
}
export default AbilityResolver;
