import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

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
    'in',
    'equals',
    'greater',
    'less',
    'not',
    'is',
    'or',
    'than',
    'equal',
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

    this.tokens.push(new AbilityDSLToken(AbilityDSLTokenType.EOF, '', {line: this.line,column: this.column}));
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
    return new AbilityDSLToken(AbilityDSLTokenType.COMMENT, value.trim(),{line: startLine, column: startColumn});
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
        return new AbilityDSLToken(AbilityDSLTokenType.STRING, value,{line: startLine, column: startColumn});
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
    return new AbilityDSLToken(AbilityDSLTokenType.NUMBER, value,{line: startLine, column: startColumn});
  }

  private readSymbol(): AbilityDSLToken {
    const startLine = this.line;
    const startColumn = this.column;
    const char = this.advance();

    switch (char) {
      case '.':
        return new AbilityDSLToken(AbilityDSLTokenType.DOT, char,{line: startLine, column: startColumn});
      case ':':
        return new AbilityDSLToken(AbilityDSLTokenType.COLON, char,{line: startLine, column: startColumn});
      case ',':
        return new AbilityDSLToken(AbilityDSLTokenType.COMMA, char,{line: startLine, column: startColumn});
      case '[':
        return new AbilityDSLToken(AbilityDSLTokenType.LBRACKET, char,{line: startLine, column: startColumn});
      case ']':
        return new AbilityDSLToken(AbilityDSLTokenType.RBRACKET, char,{line: startLine, column: startColumn});
      case '>':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLTokenType.SYMBOL, '>=',{line: startLine, column: startColumn});
        }
        return new AbilityDSLToken(AbilityDSLTokenType.SYMBOL, '>',{line: startLine, column: startColumn});
      case '<':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLTokenType.SYMBOL, '<=',{line: startLine, column: startColumn});
        }
        return new AbilityDSLToken(AbilityDSLTokenType.SYMBOL, '<',{line: startLine, column: startColumn});
      case '=':
        return new AbilityDSLToken(AbilityDSLTokenType.SYMBOL, '=',{line: startLine, column: startColumn});
      case '!':
        if (this.peek() === '=') {
          this.advance();
          return new AbilityDSLToken(AbilityDSLTokenType.SYMBOL, '!=',{line: startLine, column: startColumn});
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

    // Если есть точка — это путь (identifier или action)
    if (word.includes('.')) {
      const last = this.tokens[this.tokens.length - 1];
      if (last?.type === AbilityDSLTokenType.EFFECT) {
        return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word,{line: startLine, column: startColumn});
      }
      return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word,{line: startLine, column: startColumn});
    }

    // Ключевые слова
    if (this.keywords.has(word)) {
      // Эффекты
      if (word === 'permit' || word === 'allow') {
        return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'permit',{line: startLine, column: startColumn});
      }
      if (word === 'deny' || word === 'forbidden') {
        return new AbilityDSLToken(AbilityDSLTokenType.EFFECT, 'deny',{line: startLine, column: startColumn});
      }
      // Групповые ключевые слова
      if (word === 'all') {
        return new AbilityDSLToken(AbilityDSLTokenType.ALL, word,{line: startLine, column: startColumn});
      }
      if (word === 'any') {
        return new AbilityDSLToken(AbilityDSLTokenType.ANY, word,{line: startLine, column: startColumn});
      }
      if (word === 'of') {
        return new AbilityDSLToken(AbilityDSLTokenType.OF, word,{line: startLine, column: startColumn});
      }
      if (word === 'if') {
        return new AbilityDSLToken(AbilityDSLTokenType.IF, word,{line: startLine, column: startColumn});
      }
      // Булевы и null
      if (word === 'true' || word === 'false') {
        return new AbilityDSLToken(AbilityDSLTokenType.BOOLEAN, word,{line: startLine, column: startColumn});
      }
      if (word === 'null') {
        return new AbilityDSLToken(AbilityDSLTokenType.NULL, word,{line: startLine, column: startColumn});
      }
      // Остальные ключевые слова (contains, in, equals, greater, less, not, is, or, than, equal)
      return new AbilityDSLToken(AbilityDSLTokenType.KEYWORD, word,{line: startLine, column: startColumn});
    }

    // Если после EFFECT и нет точки — действие (например, "create")
    const lastToken = this.tokens[this.tokens.length - 1];
    if (lastToken?.type === AbilityDSLTokenType.EFFECT) {
      return new AbilityDSLToken(AbilityDSLTokenType.ACTION, word,{line: startLine, column: startColumn});
    }

    // Обычный идентификатор
    return new AbilityDSLToken(AbilityDSLTokenType.IDENTIFIER, word,{line: startLine, column: startColumn});
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
