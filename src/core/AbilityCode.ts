export class AbilityCode<
  Code extends string | number,
> {
  public _code: Code;

  constructor(code: Code) {
    this._code = code;
  }

  public get code(): Code {
    return this._code;
  }

  public isEqual(compareWith: AbilityCode<Code> | null): boolean {
    return compareWith !== null && this.code === compareWith.code;
  }

  public isNotEqual(compareWith: AbilityCode<Code> | null): boolean {
    return !this.isEqual(compareWith);
  }
}

export default AbilityCode;
