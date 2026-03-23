import { AbilityDSLTokenType } from './AbilityDSLTokenType';

export class AbilityDSLToken {
  readonly type: AbilityDSLTokenType;
  readonly value: string;

  public constructor(
    type: AbilityDSLTokenType,
    value: string,
  ) {
    this.type = type;
    this.value = value;
  }

  public toString() {
    return `AbilityDSLToken([${this.type.code}] ${this.value}`;
  }
}
