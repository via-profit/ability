import { AbilityDSLToken, TokenTypes } from './AbilityDSLToken';

export class AbilityDSLLexer {
  private readonly input: string;
  private pos = 0;
  private tokens: AbilityDSLToken[] = [];
  private line = 1;
  private column = 1;

  private readonly keywords = new Set([
    'if',
    'all',
    'any',
    'of',
    'permit',
    'allow',
    'deny',
    'forbidden',
    'true',
    'false',
    'null',
    'contains',
    'includes',
    'length',
    'has',
    'in',
    'gt',
    'lt',
    'gte',
    'lte',
    'equals',
    'greater',
    'less',
    'not',
    'is',
    'or',
    'than',
    'always',
    'never',
    'except',
  ]);

  constructor(input: string) {
    this.input = input;
  }

  public tokenize(): AbilityDSLToken[] {
    while (!this.isAtEnd()) {
      this.skipWhitespace();
      if (this.isAtEnd()) break;

      const char = this.peek();

      if (char === '@') {
        this.tokens.push(this.readAnnotation());
        continue;
      }

      if (char === '#') {
        this.tokens.push(this.readComment());
        continue;
      }

      if (char === '"' || char === "'") {
        this.tokens.push(this.readString());
        continue;
      }

      if (this.isDigit(char)) {
        this.tokens.push(this.readNumber());
        continue;
      }

      if (this.isSymbol(char)) {
        this.tokens.push(this.readSymbol());
        continue;
      }

      if (this.isAlpha(char)) {
        this.tokens.push(this.readWord());
        continue;
      }

      throw new Error(`Unexpected character '${char}' at ${this.line}:${this.column}`);
    }

    this.tokens.push(new AbilityDSLToken(TokenTypes.EOF, '', this.line, this.column));
    return this.tokens;
  }

  private readComment(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
    this.advance(); // skip '#'
    let value = '';
    while (!this.isAtEnd() && !this.isNewline()) {
      value += this.advance();
    }
    return new AbilityDSLToken(TokenTypes.COMMENT, value.trim(), startLine, startColumn);
  }

  private readAnnotation(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;

    let value = '';

    while (!this.isAtEnd() && !this.isNewline()) {
      value += this.advance();
    }
    return new AbilityDSLToken(TokenTypes.ANNOTATION, value.trim(), startLine, startColumn);
  }

  private readString(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
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
        return new AbilityDSLToken(TokenTypes.STRING, value, startLine, startColumn);
      }
      value += char;
    }

    throw new Error(`Unterminated string at ${startLine}:${startColumn}`);
  }

  private readNumber(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
    const start = this.pos;
    while (!this.isAtEnd() && this.isDigit(this.peek())) {
      this.advance();
    }
    const value = this.input.slice(start, this.pos);
    return new AbilityDSLToken(TokenTypes.NUMBER, value, startLine, startColumn);
  }

  private readSymbol(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
    const char = this.advance();

    switch (char) {
      case '.':
        return new AbilityDSLToken(TokenTypes.DOT, char, startLine, startColumn);
      case ':':
        return new AbilityDSLToken(TokenTypes.COLON, char, startLine, startColumn);
      case ',':
        return new AbilityDSLToken(TokenTypes.COMMA, char, startLine, startColumn);
      case '[':
        return new AbilityDSLToken(TokenTypes.LBRACKET, char, startLine, startColumn);
      case ']':
        return new AbilityDSLToken(TokenTypes.RBRACKET, char, startLine, startColumn);
      case '>':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(TokenTypes.SYMBOL, '>=', startLine, startColumn);
        }
        return new AbilityDSLToken(TokenTypes.SYMBOL, '>', startLine, startColumn);
      case '<':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(TokenTypes.SYMBOL, '<=', startLine, startColumn);
        }

        if (this.peek() === '>') {
          this.advance();
          return new AbilityDSLToken(TokenTypes.SYMBOL, '<>', startLine, startColumn);
        }

        return new AbilityDSLToken(TokenTypes.SYMBOL, '<', startLine, startColumn);
      case '=':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(TokenTypes.SYMBOL, '==', startLine, startColumn);
        }

        return new AbilityDSLToken(TokenTypes.SYMBOL, '=', startLine, startColumn);
      case '!':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(TokenTypes.SYMBOL, '!=', startLine, startColumn);
        }
        throw new Error(`Unexpected symbol '!' at ${this.line}:${this.column}`);
      default:
        throw new Error(`Unknown symbol '${char}' at ${this.line}:${this.column}`);
    }
  }

  private readWord(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
    const start = this.pos;

    // Первый сегмент
    while (!this.isAtEnd() && /[a-zA-Z0-9_*]/.test(this.peek())) {
      this.advance();
    }
    // Сегменты через точку
    while (!this.isAtEnd() && this.peek() === '.') {
      this.advance(); // dot
      if (!/[a-zA-Z_*]/.test(this.peek())) {
        break;
      }
      while (!this.isAtEnd() && /[a-zA-Z0-9_*]/.test(this.peek())) {
        this.advance();
      }
    }

    const word = this.input.slice(start, this.pos);

    if (word === 'always') {
      return new AbilityDSLToken(TokenTypes.ALWAYS, word, startLine, startColumn);
    }

    if (word === 'never') {
      return new AbilityDSLToken(TokenTypes.NEVER, word, startLine, startColumn);
    }

    // Если есть точка — это путь (identifier или permission)
    if (word.includes('.')) {
      const last = this.tokens[this.tokens.length - 1];
      if (last?.type === TokenTypes.EFFECT) {
        if (word.startsWith('permission.')) {
          return new AbilityDSLToken(TokenTypes.PERMISSION, word, startLine, startColumn);
        }
      }
      return new AbilityDSLToken(TokenTypes.IDENTIFIER, word, startLine, startColumn);
    }

    // Ключевые слова
    if (this.keywords.has(word)) {
      // Эффекты
      if (word === 'permit' || word === 'allow') {
        return new AbilityDSLToken(TokenTypes.EFFECT, 'permit', startLine, startColumn);
      }
      if (word === 'deny' || word === 'forbidden') {
        return new AbilityDSLToken(TokenTypes.EFFECT, 'deny', startLine, startColumn);
      }
      // Групповые ключевые слова
      if (word === 'all') {
        return new AbilityDSLToken(TokenTypes.ALL, word, startLine, startColumn);
      }
      if (word === 'any') {
        return new AbilityDSLToken(TokenTypes.ANY, word, startLine, startColumn);
      }
      if (word === 'of') {
        return new AbilityDSLToken(TokenTypes.OF, word, startLine, startColumn);
      }
      if (word === 'if') {
        return new AbilityDSLToken(TokenTypes.IF, word, startLine, startColumn);
      }
      // Булевы и null
      if (word === 'true' || word === 'false') {
        return new AbilityDSLToken(TokenTypes.BOOLEAN, word, startLine, startColumn);
      }
      if (word === 'null') {
        return new AbilityDSLToken(TokenTypes.NULL, word, startLine, startColumn);
      }
      if (word === 'except') {
        return new AbilityDSLToken(TokenTypes.EXCEPT, word, startLine, startColumn);
      }

      // Остальные ключевые слова (contains, in, equals, greater, less, not, is, or, than, equal)
      return new AbilityDSLToken(TokenTypes.KEYWORD, word, startLine, startColumn);
    }

    // Если после EFFECT и нет точки — действие (например, "create")
    const lastToken = this.tokens[this.tokens.length - 1];
    if (lastToken?.type === TokenTypes.EFFECT) {
      return new AbilityDSLToken(TokenTypes.PERMISSION, word, startLine, startColumn);
    }

    // Обычный идентификатор
    return new AbilityDSLToken(TokenTypes.IDENTIFIER, word, startLine, startColumn);
  }

  private skipWhitespace(): void {
    while (!this.isAtEnd() && /\s/.test(this.peek())) {
      this.advance();
    }
  }

  private isDigit(char: string): boolean {
    return char >= '0' && char <= '9';
  }

  private isAlpha(char: string): boolean {
    return /[a-zA-Z_]/.test(char);
  }

  private isSymbol(char: string): boolean {
    return ['.', ':', ',', '[', ']', '>', '<', '=', '!'].includes(char);
  }

  private isNewline(): boolean {
    return this.peek() === '\n';
  }

  private peek(): string {
    return this.input[this.pos];
  }

  private advance(): string {
    const ch = this.input[this.pos++];
    if (ch === '\n') {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }
    return ch;
  }

  private isAtEnd(): boolean {
    return this.pos >= this.input.length;
  }
}
