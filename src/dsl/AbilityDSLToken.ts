import { AbilityDSLTokenType } from './AbilityDSLTokenType';

export class AbilityDSLToken {
  readonly type: AbilityDSLTokenType;
  readonly value: string;
  readonly line: number;
  readonly parent: AbilityDSLToken | null;

  public constructor(
    type: AbilityDSLTokenType,
    parent: AbilityDSLToken | null,
    value: string,
    line: number,
  ) {
    this.type = type;
    this.parent = parent;
    this.value = value;
    this.line = line;
  }

  public toString() {
    return `AbilityDSLToken(type: ${this.type}, value: #${this.value}, line: ${this.line}, parent: ${this.parent?.value})`;
  }
}
