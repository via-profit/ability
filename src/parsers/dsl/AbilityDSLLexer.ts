import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

export class AbilityDSLLexer {
  private readonly input: string;
  private pos = 0;
  private tokens: AbilityDSLToken[] = [];

  public constructor(input: string) {
    this.input = input;
  }

  tokenize(): AbilityDSLToken[] {
    while (!this.isAtEnd()) {
      this.skipWhitespace();

      if (this.isAtEnd()) {
        break;
      }

      const char = this.peek();

      // Comments
      if (char === '#') {
        this.skipComment();
        continue;
      }

      // String between quotes
      if (char === '"' || char === "'") {
        this.tokens.push(this.readString());
        continue;
      }

      // Symbols
      if (this.isSymbol(char)) {
        this.tokens.push(this.readSymbol());
        continue;
      }

      // Comparators
      if (this.isOperatorStart(char)) {
        this.tokens.push(this.readSymbolicOperator());
        continue;
      }

      // Digits
      if (this.isDigit(char)) {
        this.tokens.push(this.readNumber());
        continue;
      }

      // Other
      if (this.isAlpha(char)) {
        this.tokens.push(this.readIdentifierOrKeyword());
        continue;
      }

      throw new Error(`Unexpected character '${char}' at position ${this.pos}`);
    }

    return this.tokens;
  }

  // ───────────────────────────────────────────────
  // ЧТЕНИЕ СТРОК
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
  // ЧТЕНИЕ СИМВОЛОВ
  // ───────────────────────────────────────────────

  private isSymbol(char: string): boolean {
    return ['(', ')', '[', ']', ',', '.', ':'].includes(char);
  }

  private readSymbol(): AbilityDSLToken {
    const char = this.advance();

    switch (char) {
      case '(':
        return new AbilityDSLToken(AbilityDSLTokenType.LPAREN, char);
      case ')':
        return new AbilityDSLToken(AbilityDSLTokenType.RPAREN, char);
      case '[':
        return new AbilityDSLToken(AbilityDSLTokenType.LBRACKET, char);
      case ']':
        return new AbilityDSLToken(AbilityDSLTokenType.RBRACKET, char);
      case ',':
        return new AbilityDSLToken(AbilityDSLTokenType.COMMA, char);
      case '.':
        return new AbilityDSLToken(AbilityDSLTokenType.DOT, char);
      case ':':
        return new AbilityDSLToken(AbilityDSLTokenType.COLON, char);
    }

    throw new Error(`Unknown symbol '${char}'`);
  }

  // ───────────────────────────────────────────────
  // СИМВОЛИЧЕСКИЕ ОПЕРАТОРЫ (==, !=, >=, <=)
  // ───────────────────────────────────────────────

  private isOperatorStart(char: string): boolean {
    return ['=', '!', '>', '<', '<>'].includes(char);
  }

  private readSymbolicOperator(): AbilityDSLToken {
    const char = this.advance();
    const next = this.peek();

    if (char === '=' && next === '=') {
      this.advance();
      return new AbilityDSLToken(AbilityDSLTokenType.EQ, '==');
    }
    if (char === '!' && next === '=') {
      this.advance();
      return new AbilityDSLToken(AbilityDSLTokenType.NEQ, '!=');
    }
    if (char === '<' && next === '>') {
      this.advance();
      return new AbilityDSLToken(AbilityDSLTokenType.NEQ, '<>');
    }
    if (char === '>' && next === '=') {
      this.advance();
      return new AbilityDSLToken(AbilityDSLTokenType.GTE, '>=');
    }
    if (char === '<' && next === '=') {
      this.advance();
      return new AbilityDSLToken(AbilityDSLTokenType.LTE, '<=');
    }

    if (char === '>') {
      return new AbilityDSLToken(AbilityDSLTokenType.GT, '>');
    }
    if (char === '<') {
      return new AbilityDSLToken(AbilityDSLTokenType.LT, '<');
    }

    throw new Error(`Invalid operator '${char}${next}'`);
  }

  // ───────────────────────────────────────────────
  // ЧТЕНИЕ ЧИСЕЛ
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
  // ЧТЕНИЕ ИДЕНТИФИКАТОРОВ / КЛЮЧЕВЫХ СЛОВ / ОПЕРАТОРОВ
  // ───────────────────────────────────────────────

  private readIdentifierOrKeyword(): AbilityDSLToken {
    const start = this.pos;

    while (!this.isAtEnd() && this.isAlphaNumeric(this.peek())) {
      this.advance();
    }

    let word = this.input.slice(start, this.pos);

    // Эффекты
    if (['allow', 'permit'].includes(word)) {
      return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'permit');
    }

    if (['deny', 'forbidden'].includes(word)) {
      return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'deny');
    }

    // Если предыдущий токен — EFFECT → собираем ACTION
    const last = this.tokens[this.tokens.length - 1];
    if (last?.type === AbilityDSLTokenType.EFFECT) {
      // Собираем order.bulkUpdate
      while (this.peek() === '.') {
        this.advance(); // consume dot
        const part = this.readIdentifierPart();
        word += '.' + part;
      }
      return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word);
    }

    // IF
    if (['if', 'when', 'whenever'].includes(word)) {
      return new AbilityDSLToken(AbilityDSLTokenType.IF, word);
    }

    // Логические операторы
    if (word === 'and') return new AbilityDSLToken(AbilityDSLTokenType.AND, word);
    if (word === 'or') return new AbilityDSLToken(AbilityDSLTokenType.OR, word);
    if (word === 'not') return new AbilityDSLToken(AbilityDSLTokenType.NOT, word);

    // Блоковые ключевые слова
    if (word === 'all') return new AbilityDSLToken(AbilityDSLTokenType.ALL, word);
    if (word === 'any') return new AbilityDSLToken(AbilityDSLTokenType.ANY, word);
    if (word === 'of') return new AbilityDSLToken(AbilityDSLTokenType.OF, word);

    // Многословные операторы
    const nextWord = this.peekWord();
    if (word === 'not' && nextWord === 'contains') {
      this.consumeWord();
      return new AbilityDSLToken(AbilityDSLTokenType.NOT_CONTAINS, 'not contains');
    }
    if (word === 'not' && nextWord === 'in') {
      this.consumeWord();
      return new AbilityDSLToken(AbilityDSLTokenType.NOT_IN, 'not in');
    }
    if (word === 'contains') return new AbilityDSLToken(AbilityDSLTokenType.CONTAINS, word);
    if (word === 'in') return new AbilityDSLToken(AbilityDSLTokenType.IN, word);

    // Идентификатор
    return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word);
  }

  private readIdentifierPart(): string {
    const start = this.pos;
    while (!this.isAtEnd() && this.isAlphaNumeric(this.peek())) {
      this.advance();
    }
    return this.input.slice(start, this.pos);
  }

  // ───────────────────────────────────────────────
  // ВСПОМОГАТЕЛЬНЫЕ МЕТОДЫ
  // ───────────────────────────────────────────────

  private peekWord(): string {
    let i = this.pos;
    while (i < this.input.length && this.isWhitespace(this.input[i])) {
      i++;
    }

    const start = i;
    while (i < this.input.length && this.isAlphaNumeric(this.input[i])) {
      i++;
    }

    return this.input.slice(start, i);
  }

  private consumeWord(): void {
    this.skipWhitespace();

    while (!this.isAtEnd() && this.isAlphaNumeric(this.peek())) {
      this.advance();
    }
  }

  private skipWhitespace(): void {
    while (!this.isAtEnd() && this.isWhitespace(this.peek())) {
      this.advance();
    }
  }

  private skipComment(): void {
    while (!this.isAtEnd() && this.peek() !== '\n') {
      this.advance();
    }
  }

  private isWhitespace(char: string): boolean {
    return [' ', '\t', '\n', '\r'].includes(char);
  }

  private isAlpha(char: string): boolean {
    return /[a-zA-Z_]/.test(char);
  }

  private isAlphaNumeric(char: string): boolean {
    return /[a-zA-Z0-9_]/.test(char);
  }

  private isDigit(char: string): boolean {
    return char >= '0' && char <= '9';
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
}
