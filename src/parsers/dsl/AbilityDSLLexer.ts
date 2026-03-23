import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

export class AbilityDSLLexer {
  private readonly input: string;
  private pos = 0;
  private tokens: AbilityDSLToken[] = [];

  constructor(input: string) {
    this.input = input;
  }

  public tokenize(): AbilityDSLToken[] {
    while (!this.isAtEnd()) {
      this.skipWhitespace();

      if (this.isAtEnd()) break;

      const char = this.peek();

      // Comments
      if (char === '#') {
        this.skipComment();
        continue;
      }

      // Strings
      if (char === '"' || char === "'") {
        this.tokens.push(this.readString());
        continue;
      }

      // Numbers
      if (this.isDigit(char)) {
        this.tokens.push(this.readNumber());
        continue;
      }

      // Symbols
      if (this.isSymbol(char)) {
        this.tokens.push(this.readSymbol());
        continue;
      }

      // Identifiers / keywords / operators
      if (this.isAlpha(char)) {
        this.tokens.push(this.readIdentifierOrKeyword());
        continue;
      }

      throw new Error(`Unexpected character '${char}' at position ${this.pos}`);
    }

    this.tokens.push(new AbilityDSLToken(AbilityDSLTokenType.EOF, ''));

    return this.tokens;
  }

  // ───────────────────────────────────────────────
  // STRINGS
  // ───────────────────────────────────────────────

  private readString(): AbilityDSLToken {
    const quote = this.advance();
    let value = '';
    let escaped = false;

    while (!this.isAtEnd()) {
      const char = this.advance();

      if (escaped) {
        value += char;
        escaped = false;
        continue;
      }

      if (char === '\\') {
        escaped = true;
        continue;
      }

      if (char === quote) {
        return new AbilityDSLToken(AbilityDSLTokenType.STRING, value);
      }

      value += char;
    }

    throw new Error(`Unterminated string`);
  }

  // ───────────────────────────────────────────────
  // SYMBOLS
  // ───────────────────────────────────────────────

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

  // ───────────────────────────────────────────────
  // NUMBERS
  // ───────────────────────────────────────────────

  private readNumber(): AbilityDSLToken {
    const start = this.pos;

    while (!this.isAtEnd() && this.isDigit(this.peek())) {
      this.advance();
    }

    const value = this.input.slice(start, this.pos);
    return new AbilityDSLToken(AbilityDSLTokenType.NUMBER, value);
  }

  // ───────────────────────────────────────────────
  // IDENTIFIERS / KEYWORDS / OPERATORS
  // ───────────────────────────────────────────────

  private readIdentifierOrKeyword(): AbilityDSLToken {
    const start = this.pos;

    // read identifier including dots (user.roles, env.time.hour)
    // while (!this.isAtEnd() && /[a-zA-Z0-9_.]/.test(this.peek())) {
    //   this.advance();
    // }
    // читаем первый сегмент
    while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
      this.advance();
    }

    // читаем .segment .segment .segment
    while (!this.isAtEnd() && this.peek() === '.') {
      this.advance(); // consume dot

      if (!/[a-zA-Z_]/.test(this.peek())) break; // точка не должна завершать слово

      while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
        this.advance();
      }
    }

    const word = this.input.slice(start, this.pos);


    
// is not null
if (word === 'is') {
  const next = this.peekWord();
  if (next === 'not') {
    this.consumeWord(); // not
    const next2 = this.peekWord();
    if (next2 === 'null') {
      this.consumeWord(); // null
      return new AbilityDSLToken(AbilityDSLTokenType.NOT_EQ_NULL, 'is not null');
    }
  }

  // is null
  if (next === 'null') {
    this.consumeWord(); // null
    return new AbilityDSLToken(AbilityDSLTokenType.EQ_NULL, 'is null');
  }
}



    // Если есть точка — это путь или экшен, ключевым словом быть не может
    if (word.includes('.')) {
      const last = this.tokens[this.tokens.length - 1];
      if (last?.type === AbilityDSLTokenType.EFFECT) {
        return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word);
      }
      return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word);
    }


    // GROUP COMPARISON
    if (word === 'all') return new AbilityDSLToken(AbilityDSLTokenType.ALL, word);
    if (word === 'any') return new AbilityDSLToken(AbilityDSLTokenType.ANY, word);
    if (word === 'of') return new AbilityDSLToken(AbilityDSLTokenType.OF, word);

    // EFFECT
    if (word === 'permit' || word === 'allow') {
      return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'permit');
    }
    if (word === 'deny' || word === 'forbidden') {
      return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'deny');
    }

    // IF
    if (word === 'if') {
      return new AbilityDSLToken(AbilityDSLTokenType.IF, word);
    }

    // COMPARISON OPERATORS (word-based)
    if (word === 'equals') return new AbilityDSLToken(AbilityDSLTokenType.EQ, word);
    if (word === 'is') return new AbilityDSLToken(AbilityDSLTokenType.EQ, word);
    if (word === 'contains') return new AbilityDSLToken(AbilityDSLTokenType.CONTAINS, word);
    if (word === 'in') return new AbilityDSLToken(AbilityDSLTokenType.IN, word);
    if (word === 'greater') return new AbilityDSLToken(AbilityDSLTokenType.GT_WORD, word);
    if (word === 'less') return new AbilityDSLToken(AbilityDSLTokenType.LT_WORD, word);
    if (word === 'null') return new AbilityDSLToken(AbilityDSLTokenType.NULL, word);
    if (word === 'true' || word === 'false') {
      return new AbilityDSLToken(AbilityDSLTokenType.BOOLEAN, word);
    }

    // ACTION (after EFFECT)
    const last = this.tokens[this.tokens.length - 1];
    if (last?.type === AbilityDSLTokenType.EFFECT) {
      return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word);
    }

    // IDENTIFIER (path)
    return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word);
  }

  // ───────────────────────────────────────────────
  // HELPERS
  // ───────────────────────────────────────────────

  private skipWhitespace(): void {
    while (!this.isAtEnd() && /\s/.test(this.peek())) {
      this.advance();
    }
  }

  private skipComment(): void {
    while (!this.isAtEnd() && this.peek() !== '\n') {
      this.advance();
    }
  }

  private isDigit(char: string): boolean {
    return char >= '0' && char <= '9';
  }

  private isSymbol(char: string): boolean {
    return char === '.' || char === ':' || char === ',' || char === '[' || char === ']';
  }

  private isAlpha(char: string): boolean {
    return /[a-zA-Z_]/.test(char);
  }

  private peek(): string {
    return this.input[this.pos];
  }

  private advance(): string {
    return this.input[this.pos++];
  }

  private isAtEnd(): boolean {
    return this.pos >= this.input.length;
  }

private peekWord(): string {
  let i = this.pos;

  // пропускаем ВСЕ пробельные символы
  while (i < this.input.length && /\s/.test(this.input[i])) {
    i++;
  }

  const start = i;

  // читаем слово
  while (i < this.input.length && /[a-zA-Z0-9_]/.test(this.input[i])) {
    i++;
  }

  return this.input.slice(start, i);
}



private consumeWord(): void {
  // пропускаем ВСЕ пробелы
  while (!this.isAtEnd() && /\s/.test(this.peek())) {
    this.advance();
  }

  // съедаем слово
  while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
    this.advance();
  }
}


  private isWhitespace(char: string): boolean {
    return char === ' ' || char === '\t' || char === '\n' || char === '\r';
  }

  private isAlphaNumeric(char: string): boolean {
    return /[a-zA-Z0-9_]/.test(char);
  }
}
