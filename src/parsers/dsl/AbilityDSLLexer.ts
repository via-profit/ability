import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';

export class AbilityDSLLexer {
  private readonly input: string;
  private pos = 0;
  private tokens: AbilityDSLToken[] = [];
  private line = 1;
  private column = 1;

  // Список ключевых слов
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
  ]);

  constructor(input: string) {
    this.input = input;
  }

  public tokenize(): AbilityDSLToken[] {
    while (!this.isAtEnd()) {
      this.skipWhitespace();
      if (this.isAtEnd()) break;

      const char = this.peek();

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

    this.tokens.push(new AbilityDSLToken(AbilityDSLToken.EOF, '', this.line, this.column));
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
    return new AbilityDSLToken(AbilityDSLToken.COMMENT, value.trim(), startLine, startColumn);
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
        return new AbilityDSLToken(AbilityDSLToken.STRING, value, startLine, startColumn);
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
    return new AbilityDSLToken(AbilityDSLToken.NUMBER, value, startLine, startColumn);
  }

  private readSymbol(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
    const char = this.advance();

    switch (char) {
      case '.':
        return new AbilityDSLToken(AbilityDSLToken.DOT, char, startLine, startColumn);
      case ':':
        return new AbilityDSLToken(AbilityDSLToken.COLON, char, startLine, startColumn);
      case ',':
        return new AbilityDSLToken(AbilityDSLToken.COMMA, char, startLine, startColumn);
      case '[':
        return new AbilityDSLToken(AbilityDSLToken.LBRACKET, char, startLine, startColumn);
      case ']':
        return new AbilityDSLToken(AbilityDSLToken.RBRACKET, char, startLine, startColumn);
      case '>':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '>=', startLine, startColumn);
        }
        return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '>', startLine, startColumn);
      case '<':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '<=', startLine, startColumn);
        }

        if (this.peek() === '>') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '<>', startLine, startColumn);
        }

        return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '<', startLine, startColumn);
      case '=':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '==', startLine, startColumn);
        }

        return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '=', startLine, startColumn);
      case '!':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLToken.SYMBOL, '!=', startLine, startColumn);
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
    while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
      this.advance();
    }
    // Сегменты через точку
    while (!this.isAtEnd() && this.peek() === '.') {
      this.advance(); // dot
      if (!/[a-zA-Z_]/.test(this.peek())) break;
      while (!this.isAtEnd() && /[a-zA-Z0-9_]/.test(this.peek())) {
        this.advance();
      }
    }

    const word = this.input.slice(start, this.pos);

    // Если есть точка — это путь (identifier или permission)
    if (word.includes('.')) {
      const last = this.tokens[this.tokens.length - 1];
      if (last?.code === AbilityDSLToken.EFFECT) {
        if (word.startsWith('permission.')) {
          return new AbilityDSLToken(AbilityDSLToken.PERMISSION, word, startLine, startColumn);
        }
      }
      return new AbilityDSLToken(AbilityDSLToken.IDENTIFIER, word, startLine, startColumn);
    }

    // Ключевые слова
    if (this.keywords.has(word)) {
      // Эффекты
      if (word === 'permit' || word === 'allow') {
        return new AbilityDSLToken(AbilityDSLToken.EFFECT, 'permit', startLine, startColumn);
      }
      if (word === 'deny' || word === 'forbidden') {
        return new AbilityDSLToken(AbilityDSLToken.EFFECT, 'deny', startLine, startColumn);
      }
      // Групповые ключевые слова
      if (word === 'all') {
        return new AbilityDSLToken(AbilityDSLToken.ALL, word, startLine, startColumn);
      }
      if (word === 'any') {
        return new AbilityDSLToken(AbilityDSLToken.ANY, word, startLine, startColumn);
      }
      if (word === 'of') {
        return new AbilityDSLToken(AbilityDSLToken.OF, word, startLine, startColumn);
      }
      if (word === 'if') {
        return new AbilityDSLToken(AbilityDSLToken.IF, word, startLine, startColumn);
      }
      // Булевы и null
      if (word === 'true' || word === 'false') {
        return new AbilityDSLToken(AbilityDSLToken.BOOLEAN, word, startLine, startColumn);
      }
      if (word === 'null') {
        return new AbilityDSLToken(AbilityDSLToken.NULL, word, startLine, startColumn);
      }

      // Остальные ключевые слова (contains, in, equals, greater, less, not, is, or, than, equal)
      return new AbilityDSLToken(AbilityDSLToken.KEYWORD, word, startLine, startColumn);
    }

    // Если после EFFECT и нет точки — действие (например, "create")
    const lastToken = this.tokens[this.tokens.length - 1];
    if (lastToken?.code === AbilityDSLToken.EFFECT) {
      return new AbilityDSLToken(AbilityDSLToken.PERMISSION, word, startLine, startColumn);
    }

    // Обычный идентификатор
    return new AbilityDSLToken(AbilityDSLToken.IDENTIFIER, word, startLine, startColumn);
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
  // public static readonly WORD_IF = 'if';
  // public static readonly WORD_ALL = 'all';
  // public static readonly WORD_ANY = 'any';
  // public static readonly WORD_OF = 'of';
  // public static readonly WORD_PERMIT = 'permit';
  // public static readonly WORD_ALLOW = 'allow';
  // public static readonly WORD_DENY = 'deny';
  // public static readonly WORD_FORBIDDEN = 'forbidden';
  // public static readonly WORD_TRUE = 'true';
  // public static readonly WORD_FALSE = 'false';
  // public static readonly WORD_NULL = 'null';
  // public static readonly WORD_CONTAINS = 'contains';
  // public static readonly WORD_IN = 'in';
  // public static readonly WORD_EQUALS = 'equals';
  // public static readonly WORD_GREATER = 'greater';
  // public static readonly WORD_LESS = 'less';
  // public static readonly WORD_NOT = 'not';
  // public static readonly WORD_IS = 'is';
  // public static readonly WORD_OR = 'or';
  // public static readonly WORD_THAN = 'than';
}
