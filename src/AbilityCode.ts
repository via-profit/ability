export class AbilityCode<
  Literal extends string,
  Code extends string | number | undefined = Literal,
> {
  public code: Code;

  constructor(code: Code) {
    this.code = code;
  }

  public isEqual(compareWith: AbilityCode<Literal, Code> | null): boolean {
    return compareWith !== null && this.code === compareWith.code;
  }

  public isNotEqual(compareWith: AbilityCode<Literal, Code> | null): boolean {
    return !this.isEqual(compareWith);
  }
}

export default AbilityCode;
