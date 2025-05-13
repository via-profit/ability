export declare class AbilityCode<Literal extends string, Code extends string | number | undefined = Literal> {
    code: Code;
    constructor(code: Code);
    isEqual(compareWith: AbilityCode<Literal, Code> | null): boolean;
    isNotEqual(compareWith: AbilityCode<Literal, Code> | null): boolean;
}
export default AbilityCode;
