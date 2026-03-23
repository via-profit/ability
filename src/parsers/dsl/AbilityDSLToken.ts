import { AbilityDSLTokenType } from './AbilityDSLTokenType';

export class AbilityDSLToken {
  readonly type: AbilityDSLTokenType;
  readonly value: string;
  readonly position: number;

  public constructor(
    type: AbilityDSLTokenType,
    value: string,
    position: number,
  ) {
    this.type = type;
    this.value = value;
    this.position = position;
  }

  public toString() {
    return `AbilityDSLToken([${this.type.code}] ${this.value}`;
  }
}
