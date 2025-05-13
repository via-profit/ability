import AbilityCode from './AbilityCode';
export type AbilityMatchCodeType = 'pending' | 'match' | 'mismatch';
export declare class AbilityMatch extends AbilityCode<AbilityMatchCodeType> {
    static pending: AbilityMatch;
    static match: AbilityMatch;
    static mismatch: AbilityMatch;
}
export default AbilityMatch;
