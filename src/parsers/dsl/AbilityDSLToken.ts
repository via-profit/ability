import { AbilityDSLTokenType } from './AbilityDSLTokenType';

export class AbilityDSLToken {
  readonly type: AbilityDSLTokenType;
  readonly value: string;
  readonly position: number;
  public readonly parent: AbilityDSLToken | null;

  public constructor(
    type: AbilityDSLTokenType,
    value: string,
    position: number,
    parent: AbilityDSLToken | null = null,
  ) {
    this.type = type;
    this.parent = parent;
    this.value = value;
    this.position = position;
  }

  public previous(): AbilityDSLToken | null {
    return this.parent;
  }

  // Найти предыдущий токен определенного типа
  public findPreviousType(type: AbilityDSLTokenType): AbilityDSLToken | null {
    let current = this.parent;
    while (current) {
      if (current.type === type) {
        return current;
      }
      current = current.parent;
    }
    return null;
  }

  public toString() {
    return `AbilityDSLToken([${this.type.code}] ${this.value}`;
  }
}
