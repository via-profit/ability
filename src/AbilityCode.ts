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

  public static fromLiteral(literal: string | number): AbilityCode {
    let Code: AbilityCode | null = null;

    Object.keys(this).forEach(member => {
      if (member === literal) {
        Code = this[member as keyof typeof this] as AbilityCode;
      }
    });

    if (!Code) {
      throw new Error(
        `Mismatch error. The literal ${literal} can not be find as a member of this class`,
      );
    }

    return new this((Code as AbilityCode).code) as AbilityCode;
  }
}

export default AbilityCode;
