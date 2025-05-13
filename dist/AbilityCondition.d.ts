import AbilityCode from './AbilityCode';
export type AbilityConditionCodeType = '=' | '<>' | '>' | '<' | '>=' | '<=' | 'in' | 'not in';
export type AbilityConditionLiteralType = 'equal' | 'not_equal' | 'more_than' | 'less_than' | 'less_or_equal' | 'more_or_equal' | 'in' | 'not_in';
export declare class AbilityCondition extends AbilityCode<AbilityConditionLiteralType, AbilityConditionCodeType> {
    static equal: AbilityCondition;
    static not_equal: AbilityCondition;
    static more_than: AbilityCondition;
    static less_than: AbilityCondition;
    static less_or_equal: AbilityCondition;
    static more_or_equal: AbilityCondition;
    static in: AbilityCondition;
    static not_in: AbilityCondition;
    static fromLiteral(literal: AbilityConditionLiteralType): AbilityCondition;
}
export default AbilityCondition;
