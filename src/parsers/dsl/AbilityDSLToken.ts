import { AbilityDSLTokenType } from './AbilityDSLTokenType';

/**
 * Represents a single token produced by the Ability DSL lexer.
 * Each token carries a type (e.g., EFFECT, IDENTIFIER, STRING) and its raw string value.
 */
export class AbilityDSLToken {
  /** The token type (e.g., EFFECT, IDENTIFIER, STRING). */
  readonly type: AbilityDSLTokenType;

  /** The literal text of the token as it appeared in the input (e.g., "permit", "user.roles", "admin"). */
  readonly value: string;

  /**
   * Creates a new DSL token.
   * @param type - The token type.
   * @param value - The raw string value.
   */
  public constructor(type: AbilityDSLTokenType, value: string) {
    this.type = type;
    this.value = value;
  }

  /**
   * Returns a human-readable representation of the token, useful for debugging.
   * Example output: "AbilityDSLToken([EFFECT] permit"
   */
  public toString(): string {
    return `AbilityDSLToken([${this.type.code}] ${this.value}`;
  }
}
