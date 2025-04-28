export class AbilityCode<T extends string | number | undefined = number> {
  public code: T;

  constructor(code: T) {
    this.code = code;
  }

  public isEqual(compareWith: AbilityCode<T> | null): boolean {
    return compareWith !== null && this.code === compareWith.code;
  }

  public isNotEqual(compareWith: AbilityCode<T> | null): boolean {
    return !this.isEqual(compareWith);
  }
}

export default AbilityCode;