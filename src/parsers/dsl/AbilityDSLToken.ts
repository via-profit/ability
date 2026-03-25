import { AbilityDSLTokenType } from './AbilityDSLTokenType';

export type AbilityDSLTokenConstructorProps = {
  readonly line: number;
  readonly column: number;
};

/**
 * Represents a single token produced by the Ability DSL lexer.
 * Each token carries a type (e.g., EFFECT, IDENTIFIER, STRING) and its raw string value.
 */
export class AbilityDSLToken {
  /** The token type (e.g., EFFECT, IDENTIFIER, STRING). */
  readonly type: AbilityDSLTokenType;

  /** The literal text of the token as it appeared in the input (e.g., "permit", "user.roles", "admin"). */
  readonly value: string;

  /** The line number in DSL */
  readonly line: number = 1;

  /** The column in dsl */
  readonly column: number = 1;

  /**
   * Creates a new DSL token.
   * @param type - The token type.
   * @param value - The raw string value.
   * @param props - info about line and columns
   */
  public constructor(
    type: AbilityDSLTokenType,
    value: string,
    props?: AbilityDSLTokenConstructorProps,
  ) {
    this.type = type;
    this.value = value;

    this.line = props?.line || this.line;
    this.column = props?.column || this.column;
  }

  /**
   * Returns a human-readable representation of the token, useful for debugging.
   * Example output: "AbilityDSLToken([EFFECT] permit"
   */
  public toString(): string {
    return `AbilityDSLToken([${this.type.code}] "${this.value}" at ${this.line}:${this.column})`;
  }
}
