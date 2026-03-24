import AbilityCode from '~/core/AbilityCode';

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

  // -------------------------------------------------------------------------
  // #region Comparison operators (word‑based)
  // -------------------------------------------------------------------------
  | 'EQ' // equals, is – equality
  | 'CONTAINS' // contains – membership in array / substring
  | 'IN' // in – membership in array
  | 'GT_WORD' // greater – greater than
  | 'LT_WORD' // less – less than
  | 'NULL' // null – literal null
  | 'EQ_NULL' // is null – special null equality
  | 'NOT_EQ_NULL' // is not null – special null inequality

  // -------------------------------------------------------------------------
  // #region Literal values
  // -------------------------------------------------------------------------
  | 'STRING' // any text inside single or double quotes
  | 'NUMBER' // integer or decimal number
  | 'BOOLEAN' // true / false

  // -------------------------------------------------------------------------
  // #region Fallback
  // -------------------------------------------------------------------------
  | 'UNKNOWN';

/**
 * Strongly‑typed representation of a DSL token type.
 * Each instance holds a string code and inherits comparison methods from AbilityCode.
 */
export class AbilityDSLTokenType extends AbilityCode<TokenType> {
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

  // =========================================================================
  // #region Comparison operators
  // =========================================================================
  public static EQ = new AbilityDSLTokenType('EQ'); // equals, is
  public static CONTAINS = new AbilityDSLTokenType('CONTAINS');
  public static IN = new AbilityDSLTokenType('IN');
  public static GT_WORD = new AbilityDSLTokenType('GT_WORD'); // greater
  public static LT_WORD = new AbilityDSLTokenType('LT_WORD'); // less
  public static NULL = new AbilityDSLTokenType('NULL');
  public static EQ_NULL = new AbilityDSLTokenType('EQ_NULL'); // is null
  public static NOT_EQ_NULL = new AbilityDSLTokenType('NOT_EQ_NULL'); // is not null

  // =========================================================================
  // #region Literal value tokens
  // =========================================================================
  public static STRING = new AbilityDSLTokenType('STRING');
  public static NUMBER = new AbilityDSLTokenType('NUMBER');
  public static BOOLEAN = new AbilityDSLTokenType('BOOLEAN');

  /**
   * Resolves a raw word (or symbol) to the corresponding token type.
   * Used during lexical analysis when the exact type is not yet known.
   *
   * @param str - The string fragment to classify.
   * @returns The matching AbilityDSLTokenType.
   */
  public static resolve(str: string): AbilityDSLTokenType {
    const lower = str.toLowerCase();

    // Effects (policy outcome)
    if (lower === 'permit' || lower === 'allow') {
      return AbilityDSLTokenType.EFFECT;
    }
    if (lower === 'deny' || lower === 'forbidden') {
      return AbilityDSLTokenType.EFFECT;
    }

    // Conditional keyword
    if (lower === 'if') {
      return AbilityDSLTokenType.IF;
    }

    // Group keywords
    if (lower === 'all') {
      return AbilityDSLTokenType.ALL;
    }
    if (lower === 'any') {
      return AbilityDSLTokenType.ANY;
    }
    if (lower === 'of') {
      return AbilityDSLTokenType.OF;
    }

    // Comparison operators
    if (lower === 'equals' || lower === 'is') {
      return AbilityDSLTokenType.EQ;
    }
    if (lower === 'contains') {
      return AbilityDSLTokenType.CONTAINS;
    }
    if (lower === 'in') {
      return AbilityDSLTokenType.IN;
    }
    if (lower === 'greater') {
      return AbilityDSLTokenType.GT_WORD;
    }
    if (lower === 'less') {
      return AbilityDSLTokenType.LT_WORD;
    }
    if (lower === 'null') {
      return AbilityDSLTokenType.NULL;
    }

    // Literal values
    if (lower === 'true' || lower === 'false') {
      return AbilityDSLTokenType.BOOLEAN;
    }
    if (!isNaN(Number(lower))) {
      return AbilityDSLTokenType.NUMBER;
    }

    // Default: identifier (path, action, or plain word)
    return AbilityDSLTokenType.IDENTIFIER;
  }
}
