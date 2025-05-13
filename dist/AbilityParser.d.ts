import AbilityPolicy from './AbilityPolicy';
export declare class AbilityParser {
    /**
     * Sets a value in a nested object structure based on a dot/bracket notation path.
     * @param object - The target object to modify.
     * @param path - The path to the property in dot/bracket notation.
     * @param value - The value to set at the specified path.
     */
    static setValueDotValue(object: Record<string, any>, path: string, value: string | number | boolean): void;
    /**
     * Generates TypeScript type definitions based on the provided policies.
     * @param policies - An array of AbilityPolicy instances.
     * @param outPath - The output path for the generated type definitions.
     * @returns A record containing the generated type definitions.
     */
    static generateTypeDefs(policies: readonly AbilityPolicy[], outPath: string): Record<string, any>;
}
export default AbilityParser;
