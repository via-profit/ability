import AbilityCode from '../../core/AbilityCode';

export type TokenType =
  // -------------------------------------------------------------------------
  // #region Structural tokens – define the shape of the policy
  // -------------------------------------------------------------------------
  | 'EFFECT' // permit, deny – overall policy effect
  | 'IF' // if – start of condition block
  | 'PERMISSION' // order.update – the permission being governed
  | 'IDENTIFIER' // user.roles, env.time.hour – dot‑notation paths or simple names
  | 'COLON' // : – separates keyword from block content
  | 'COMMA' // , – separates array elements
  | 'DOT' // . – used inside identifiers (handled in the lexer)
  | 'LBRACKET' // [ – start of array literal
  | 'RBRACKET' // ] – end of array literal
  | 'ALL' // all – group operator (logical AND)
  | 'ANY' // any – group operator (logical OR)
  | 'OF' // of – part of 'all of' / 'any of'
  | 'EOF' // end of file – signals end of input
  | 'COMMENT' // comments with annotations (@name ..., @description ...)

  // -------------------------------------------------------------------------
  // #region Comparison operators (word‑based)
  // -------------------------------------------------------------------------
  | 'EQ' // equal, is – equality
  | 'CONTAINS' // contains – membership in array / substring
  | 'IN' // in – membership in array
  | 'NOT_IN' // in – membership in array
  | 'NOT_CONTAINS' // array not contains
  | 'GT' // greater
  | 'GTE' // greater than
  | 'LT' // less
  | 'LTE' // less than
  | 'NULL' // null – literal null
  | 'EQ_NULL' // is null – special null equality
  | 'NOT_EQ_NULL' // is not null – special null inequality
  | 'NOT_EQ' // not equal
  | 'LEN_GT' // length greater
  | 'LEN_LT' // length less
  | 'LEN_EQ' // length equals
  | 'ALWAYS' // always
  | 'NEVER' // never
  | 'EXCEPT' // except block
  | 'ANNOTATION' // annotations like a @name

  // -------------------------------------------------------------------------
  // #region Literal values
  // -------------------------------------------------------------------------
  | 'STRING' // any text inside single or double quotes
  | 'NUMBER' // integer or decimal number
  | 'BOOLEAN' // true / false
  | 'SYMBOL' // symbols

  // -------------------------------------------------------------------------
  // #region Row operator
  // -------------------------------------------------------------------------
  | 'KEYWORD'
  // -------------------------------------------------------------------------
  // #region Fallback
  // -------------------------------------------------------------------------
  | 'UNKNOWN';

/**
 * Represents a single token produced by the Ability DSL lexer.
 * Each token carries a type (e.g., EFFECT, IDENTIFIER, STRING) and its raw string value.
 */
export class AbilityDSLToken<Code extends TokenType = TokenType> extends AbilityCode<Code> {
  /** The literal text of the token as it appeared in the input (e.g., "permit", "user.roles", "admin"). */
  readonly value: string = '';

  /** The line number in DSL */
  readonly line: number = 1;

  /** The column in dsl */
  readonly column: number = 1;

  public constructor(type: Code, value: string, line: number, column: number) {
    super(type);

    this.value = value;
    this.line = line;
    this.column = column;
  }

  /**
   * Returns a human-readable representation of the token, useful for debugging.
   * Example output: "AbilityDSLToken([EFFECT] permit"
   */
  public toString(): string {
    return `AbilityDSLToken([${this.code}] "${this.value}" at ${this.line}:${this.column})`;
  }

  public static readonly EFFECT: TokenType = 'EFFECT';
  public static readonly IF: TokenType = 'IF';
  public static readonly PERMISSION: TokenType = 'PERMISSION';
  public static readonly IDENTIFIER: TokenType = 'IDENTIFIER';
  public static readonly COLON: TokenType = 'COLON';
  public static readonly COMMA: TokenType = 'COMMA';
  public static readonly DOT: TokenType = 'DOT';
  public static readonly LBRACKET: TokenType = 'LBRACKET';
  public static readonly RBRACKET: TokenType = 'RBRACKET';
  public static readonly ALL: TokenType = 'ALL';
  public static readonly ANY: TokenType = 'ANY';
  public static readonly OF: TokenType = 'OF';
  public static readonly EOF: TokenType = 'EOF';
  public static readonly COMMENT: TokenType = 'COMMENT';
  public static readonly EQ: TokenType = 'EQ';
  public static readonly CONTAINS: TokenType = 'CONTAINS';
  public static readonly IN: TokenType = 'IN';
  public static readonly NOT_IN: TokenType = 'NOT_IN';
  public static readonly NOT_CONTAINS: TokenType = 'NOT_CONTAINS';
  public static readonly GT: TokenType = 'GT';
  public static readonly GTE: TokenType = 'GTE';
  public static readonly LT: TokenType = 'LT';
  public static readonly LTE: TokenType = 'LTE';
  public static readonly NULL: TokenType = 'NULL';
  public static readonly EQ_NULL: TokenType = 'EQ_NULL';
  public static readonly NOT_EQ_NULL: TokenType = 'NOT_EQ_NULL';
  public static readonly LEN_GT: TokenType = 'LEN_GT';
  public static readonly LEN_LT: TokenType = 'LEN_LT';
  public static readonly LEN_EQ: TokenType = 'LEN_EQ';
  public static readonly NOT_EQ: TokenType = 'NOT_EQ';
  public static readonly ALWAYS: TokenType = 'ALWAYS';
  public static readonly NEVER: TokenType = 'NEVER';
  public static readonly EXCEPT: TokenType = 'EXCEPT';
  public static readonly STRING: TokenType = 'STRING';
  public static readonly NUMBER: TokenType = 'NUMBER';
  public static readonly BOOLEAN: TokenType = 'BOOLEAN';
  public static readonly SYMBOL: TokenType = 'SYMBOL';
  public static readonly KEYWORD: TokenType = 'KEYWORD';
  public static readonly ANNOTATION: TokenType = 'ANNOTATION';
}
