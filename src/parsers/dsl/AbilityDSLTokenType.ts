import { AbilityCode } from '~/core/AbilityCode';

/**
 * Discriminated union of all token types recognized by the Ability DSL lexer.
 */
export type TokenType =
  // -------------------------------------------------------------------------
  // #region Structural tokens – define the shape of the policy
  // -------------------------------------------------------------------------
  | 'EFFECT' // permit, deny – overall policy effect
  | 'IF' // if – start of condition block
  | 'ACTION' // order.update – the action being governed
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
 * Strongly‑typed representation of a DSL token type.
 * Each instance holds a string code and inherits comparison methods from AbilityCode.
 */
export class AbilityDSLTokenType<Code extends TokenType = TokenType> extends AbilityCode<Code> {
  // =========================================================================
  // #region Structural tokens
  // =========================================================================
  public static EOF = new AbilityDSLTokenType('EOF');
  public static DOT = new AbilityDSLTokenType('DOT');
  public static EFFECT = new AbilityDSLTokenType('EFFECT');
  public static IF = new AbilityDSLTokenType('IF');
  public static ACTION = new AbilityDSLTokenType('ACTION');
  public static IDENTIFIER = new AbilityDSLTokenType('IDENTIFIER');
  public static COLON = new AbilityDSLTokenType('COLON');
  public static COMMA = new AbilityDSLTokenType('COMMA');
  public static LBRACKET = new AbilityDSLTokenType('LBRACKET');
  public static RBRACKET = new AbilityDSLTokenType('RBRACKET');
  public static ALL = new AbilityDSLTokenType('ALL');
  public static ANY = new AbilityDSLTokenType('ANY');
  public static OF = new AbilityDSLTokenType('OF');
  public static COMMENT = new AbilityDSLTokenType('COMMENT');

  //

  public static SYMBOL = new AbilityDSLTokenType('SYMBOL');

  // =========================================================================
  // #region Comparison operators
  // =========================================================================
  public static NOT_EQ = new AbilityDSLTokenType('NOT_EQ'); // not equals
  public static EQ = new AbilityDSLTokenType('EQ'); // equals, is
  public static IN = new AbilityDSLTokenType('IN');
  public static NOT_IN = new AbilityDSLTokenType('NOT_IN');
  public static CONTAINS = new AbilityDSLTokenType('CONTAINS');
  public static NOT_CONTAINS = new AbilityDSLTokenType('NOT_CONTAINS');
  public static GT = new AbilityDSLTokenType('GT'); // greater
  public static GTE = new AbilityDSLTokenType('GTE'); // greater than
  public static LT = new AbilityDSLTokenType('LT'); // less
  public static LTE = new AbilityDSLTokenType('LTE'); // less than
  public static NULL = new AbilityDSLTokenType('NULL');
  public static EQ_NULL = new AbilityDSLTokenType('EQ_NULL'); // is null
  public static NOT_EQ_NULL = new AbilityDSLTokenType('NOT_EQ_NULL'); // is not null

  // =========================================================================
  // #region Literal value tokens
  // =========================================================================
  public static STRING = new AbilityDSLTokenType('STRING');
  public static NUMBER = new AbilityDSLTokenType('NUMBER');
  public static BOOLEAN = new AbilityDSLTokenType('BOOLEAN');

  // =========================================================================
  // #region Raw operator token
  // =========================================================================
  public static KEYWORD = new AbilityDSLTokenType('KEYWORD');
}
