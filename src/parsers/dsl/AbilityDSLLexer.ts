import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

/**
 * Lexer for the Ability DSL.
 *
 * Converts a raw DSL string into a stream of tokens. Handles:
 * - Comments (starting with '#')
 * - String literals (single or double quotes with escape sequences)
 * - Numbers (integer or decimal)
 * - Symbols: ., :, ,, [, ]
 * - Identifiers, keywords, and operators (including multi-word operators like "is null")
 *
 * The lexer does not interpret the tokens; it only classifies them by type.
 */
export class AbilityDSLLexer {
  private readonly input: string;
  private pos = 0;
  private tokens: AbilityDSLToken[] = [];

  constructor(input: string) {
    this.input = input;
  }

  /**
   * Main entry point: process the entire input and return a list of tokens.
   */
  public tokenize(): AbilityDSLToken[] {
    while (!this.isAtEnd()) {
      // Skip any whitespace (spaces, tabs, newlines)
      this.skipWhitespace();

      // If we've reached the end after skipping whitespace, stop.
      if (this.isAtEnd()) {
        break;
      }

      const char = this.peek();

      // Comments: everything after '#' until newline
      if (char === '#') {
        this.tokens.push(this.readComment());
        continue;
      }

      // String literals: start with " or '
      if (char === '"' || char === "'") {
        this.tokens.push(this.readString());
        continue;
      }

      // Numbers: sequence of digits (with optional decimal point)
      if (this.isDigit(char)) {
        this.tokens.push(this.readNumber());
        continue;
      }

      // Single-character symbols (punctuation)
      if (this.isSymbol(char)) {
        this.tokens.push(this.readSymbol());
        continue;
      }

      // Identifiers, keywords, operators: start with a letter or underscore
      if (this.isAlpha(char)) {
        this.tokens.push(this.readIdentifierOrKeyword());
        continue;
      }

      // If none of the above matched, the character is unexpected.
      throw new Error(`Unexpected character '${char}' at position ${this.pos}`);
    }

    // End-of-file marker to simplify parser termination.
    this.tokens.push(new AbilityDSLToken(AbilityDSLTokenType.EOF, ''));

    return this.tokens;
  }

  // -------------------------------------------------------------------------
  // #region String literal parsing
  // -------------------------------------------------------------------------

  private readComment(): AbilityDSLToken {
    this.advance(); // skip the shark symbol ('#')

    let value = '';

    while (!this.isAtEnd()) {
      const char = this.advance();

      // if is end of line, then stop
      if(char === '\n') {
        break;
      }

      value += char;
    }

    return new AbilityDSLToken(AbilityDSLTokenType.COMMENT, value.trim());
  }

  /**
   * Reads a string literal enclosed in single or double quotes.
   * Supports backslash escape sequences.
   * @returns A STRING token with the unescaped content.
   */
  private readString(): AbilityDSLToken {
    const quote = this.advance(); // opening quote
    let value = '';
    let escaped = false;

    while (!this.isAtEnd()) {
      const char = this.advance();

      if (escaped) {
        // Escaped character: add as-is (no special handling beyond removing backslash)
        value += char;
        escaped = false;
        continue;
      }

      if (char === '\\') {
        escaped = true;
        continue;
      }

      if (char === quote) {
        // Closing quote found: return the token
        return new AbilityDSLToken(AbilityDSLTokenType.STRING, value);
      }

      value += char;
    }

    throw new Error(`Unterminated string`);
  }

  // -------------------------------------------------------------------------
  // #region Symbol parsing (single characters)
  // -------------------------------------------------------------------------

  /**
   * Reads a single-character symbol and returns the corresponding token.
   */
  private readSymbol(): AbilityDSLToken {
    const char = this.advance();

    switch (char) {
      case '.':
        return new AbilityDSLToken(AbilityDSLTokenType.DOT, char);
      case ':':
        return new AbilityDSLToken(AbilityDSLTokenType.COLON, char);
      case ',':
        return new AbilityDSLToken(AbilityDSLTokenType.COMMA, char);
      case '[':
        return new AbilityDSLToken(AbilityDSLTokenType.LBRACKET, char);
      case ']':
        return new AbilityDSLToken(AbilityDSLTokenType.RBRACKET, char);
    }

    throw new Error(`Unknown symbol '${char}'`);
  }

  // -------------------------------------------------------------------------
  // #region Number parsing
  // -------------------------------------------------------------------------

  /**
   * Reads a numeric literal (integer or decimal). Does not support scientific notation.
   * @returns A NUMBER token with the raw string representation.
   */
  private readNumber(): AbilityDSLToken {
    const start = this.pos;

    // Consume digits and optional decimal point (but only one decimal point)
    while (!this.isAtEnd() && this.isDigit(this.peek())) {
      this.advance();
    }

    const value = this.input.slice(start, this.pos);
    return new AbilityDSLToken(AbilityDSLTokenType.NUMBER, value);
  }

  // -------------------------------------------------------------------------
  // #region Identifier, keyword, and operator parsing
  // -------------------------------------------------------------------------

  /**
   * Reads a sequence of characters that can form identifiers, keywords,
   * operators, or paths (dotted names).
   *
   * This method also handles special multi-word operators like "is null"
   * and "is not null" by peeking ahead and consuming the additional words.
   */
  private readIdentifierOrKeyword(): AbilityDSLToken {
    const start = this.pos;

    // Read the first segment (letters, digits, underscore)
    while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
      this.advance();
    }

    // Read subsequent segments separated by dots (e.g., user.roles)
    while (!this.isAtEnd() && this.peek() === '.') {
      this.advance(); // consume the dot

      // After a dot, we expect at least one letter or underscore
      if (!/[a-zA-Z_]/.test(this.peek())) {
        break; // dot not followed by a valid start of an identifier
      }

      while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
        this.advance();
      }
    }

    const word = this.input.slice(start, this.pos);

    // Handle "not equals"
    // Handle "not equals"
    if (word === 'not') {
      const next = this.peekWord();
      if (next === 'equals') {
        this.consumeWord();
        return new AbilityDSLToken(AbilityDSLTokenType.NOT_EQ, 'not equals');
      }
    }

    // Handle "is null" / "is not null" / "is not equals"
    if (word === 'is') {
      const next = this.peekWord();

      if (next === 'null') {
        this.consumeWord();
        return new AbilityDSLToken(AbilityDSLTokenType.EQ_NULL, 'is null');
      }

      if (next === 'not') {
        this.consumeWord();
        const next2 = this.peekWord();

        if (next2 === 'null') {
          this.consumeWord();
          return new AbilityDSLToken(AbilityDSLTokenType.NOT_EQ_NULL, 'is not null');
        }

        if (next2 === 'equals') {
          this.consumeWord();
          return new AbilityDSLToken(AbilityDSLTokenType.NOT_EQ, 'not equals');
        }

        return new AbilityDSLToken(AbilityDSLTokenType.EQ, 'is');
      }

      return new AbilityDSLToken(AbilityDSLTokenType.EQ, 'is');
    }

    // Only now handle plain "equals"
    if (word === 'equals') {
      return new AbilityDSLToken(AbilityDSLTokenType.EQ, word);
    }

    // If the token contains a dot, it's either a path (identifier) or an action.
    if (word.includes('.')) {
      const last = this.tokens[this.tokens.length - 1];
      // If the previous token is EFFECT, then this is an ACTION token.
      if (last?.type === AbilityDSLTokenType.EFFECT) {
        return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word);
      }
      return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word);
    }
    // Only now handle plain "equals"
    if (word === 'equals') {
      return new AbilityDSLToken(AbilityDSLTokenType.EQ, word);
    }
    // Group keywords
    if (word === 'all') {
      return new AbilityDSLToken(AbilityDSLTokenType.ALL, word);
    }
    if (word === 'any') {
      return new AbilityDSLToken(AbilityDSLTokenType.ANY, word);
    }
    if (word === 'of') {
      return new AbilityDSLToken(AbilityDSLTokenType.OF, word);
    }

    // Effects (policy outcome)
    if (word === 'permit' || word === 'allow') {
      return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'permit');
    }
    if (word === 'deny' || word === 'forbidden') {
      return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'deny');
    }

    // "if" keyword
    if (word === 'if') {
      return new AbilityDSLToken(AbilityDSLTokenType.IF, word);
    }

    // Word-based comparison operators
    if (word === 'equals') {
      return new AbilityDSLToken(AbilityDSLTokenType.EQ, word);
    }
    // if (word === 'is') {
    //   return new AbilityDSLToken(AbilityDSLTokenType.EQ, word);
    // }
    if (word === 'contains') {
      return new AbilityDSLToken(AbilityDSLTokenType.CONTAINS, word);
    }
    if (word === 'in') {
      return new AbilityDSLToken(AbilityDSLTokenType.IN, word);
    }
    if (word === 'greater') {
      return new AbilityDSLToken(AbilityDSLTokenType.GT_WORD, word);
    }
    if (word === 'less') {
      return new AbilityDSLToken(AbilityDSLTokenType.LT_WORD, word);
    }
    if (word === 'null') {
      return new AbilityDSLToken(AbilityDSLTokenType.NULL, word);
    }
    if (word === 'true' || word === 'false') {
      return new AbilityDSLToken(AbilityDSLTokenType.BOOLEAN, word);
    }

    // If the token appears immediately after an EFFECT and it doesn't contain a dot,
    // it could be a simple action name (e.g., "create").
    const lastToken = this.tokens[this.tokens.length - 1];
    if (lastToken?.type === AbilityDSLTokenType.EFFECT) {
      return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word);
    }

    // Default: treat as an identifier (e.g., a variable name or plain word)
    return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word);
  }

  // -------------------------------------------------------------------------
  // #region Helper methods for whitespace, comments, lookahead
  // -------------------------------------------------------------------------

  /** Skips any whitespace characters (spaces, tabs, newlines). */
  private skipWhitespace(): void {
    while (!this.isAtEnd() && /\s/.test(this.peek())) {
      this.advance();
    }
  }

  /** Skips a comment starting with '#' until the end of the line. */
  private skipComment(): void {
    while (!this.isAtEnd() && this.peek() !== '\n') {
      this.advance();
    }
  }

  /** Returns true if the character is a digit (0-9). */
  private isDigit(char: string): boolean {
    return char >= '0' && char <= '9';
  }

  /** Returns true if the character is one of the defined symbols. */
  private isSymbol(char: string): boolean {
    return char === '.' || char === ':' || char === ',' || char === '[' || char === ']';
  }

  /** Returns true if the character is a letter or underscore. */
  private isAlpha(char: string): boolean {
    return /[a-zA-Z_]/.test(char);
  }

  /** Peeks at the current character without consuming it. */
  private peek(): string {
    return this.input[this.pos];
  }

  /** Advances the position and returns the character that was at the old position. */
  private advance(): string {
    return this.input[this.pos++];
  }

  /** Checks if we have reached the end of the input. */
  private isAtEnd(): boolean {
    return this.pos >= this.input.length;
  }

  /**
   * Peeks at the next word in the input (skipping whitespace) without consuming it.
   * Used for multi-word operator detection (e.g., "is null").
   */
  private peekWord(): string {
    let i = this.pos;

    // Skip any whitespace
    while (i < this.input.length && /\s/.test(this.input[i])) {
      i++;
    }

    const start = i;

    // Read the next word (letters, digits, underscore)
    while (i < this.input.length && /[a-zA-Z0-9_]/.test(this.input[i])) {
      i++;
    }

    return this.input.slice(start, i);
  }

  /**
   * Consumes the next word (skipping whitespace) without generating a token.
   * Used to consume "not" and "null" during multi-word operator detection.
   */
  private consumeWord(): void {
    // Skip whitespace
    while (!this.isAtEnd() && /\s/.test(this.peek())) {
      this.advance();
    }

    // Consume the word characters
    while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
      this.advance();
    }
  }

  /** Returns true if the character is whitespace (space, tab, newline, carriage return). */
  private isWhitespace(char: string): boolean {
    return char === ' ' || char === '\t' || char === '\n' || char === '\r';
  }

  /** Returns true if the character is alphanumeric or underscore. */
  private isAlphaNumeric(char: string): boolean {
    return /[a-zA-Z0-9_]/.test(char);
  }
}
