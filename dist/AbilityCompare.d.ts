import AbilityCode from './AbilityCode';
export type AbilityCompareCodeType = 'and' | 'or';
export declare class AbilityCompare extends AbilityCode<AbilityCompareCodeType> {
    static and: AbilityCompare;
    static or: AbilityCompare;
}
export default AbilityCompare;
