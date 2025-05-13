import AbilityCode from './AbilityCode';
export type AbilityCompareLiteralType = 'and' | 'or';
export declare class AbilityCompare extends AbilityCode<AbilityCompareLiteralType> {
    static and: AbilityCompare;
    static or: AbilityCompare;
    static fromLiteral(literal: AbilityCompareLiteralType): AbilityCompare;
}
export default AbilityCompare;
