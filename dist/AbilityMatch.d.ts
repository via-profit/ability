import AbilityCode from './AbilityCode';
export type AbilityMatchLiteralType = 'pending' | 'match' | 'mismatch';
export declare class AbilityMatch extends AbilityCode<AbilityMatchLiteralType> {
    static pending: AbilityMatch;
    static match: AbilityMatch;
    static mismatch: AbilityMatch;
    static fromLiteral(literal: AbilityMatchLiteralType): AbilityMatch;
}
export default AbilityMatch;
