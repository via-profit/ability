export declare class AbilityCode<Code extends string | number> {
    _code: Code;
    constructor(code: Code);
    get code(): Code;
    isEqual(compareWith: AbilityCode<Code> | null): boolean;
    isNotEqual(compareWith: AbilityCode<Code> | null): boolean;
}
export default AbilityCode;
