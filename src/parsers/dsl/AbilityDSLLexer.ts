import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

export class AbilityDSLLexer {
  private readonly input: string;
  private pos: number = 0;
  private tokens: AbilityDSLToken[] = [];
  private lastToken: AbilityDSLToken | null = null;

  private compareMap: Record<string, string> = {
    and: 'and',
    or: 'or',
  };

  private effectsMap: Record<string, string> = {
    permit: 'permit',
    allow: 'permit',
    deny: 'deny',
    forbidden: 'deny',
  };

  // Полный список возможных компараторов (включая составные)
  private comparators: string[] = [
    '=',
    '>',
    '<',
    '>=',
    '<=',
    '<>',
    'in',
    'is',
    'not',
    'not in',
    'equals',
    'is not',
    'is equals',
    'is not equals',
    'not equals',
    'greater',
    'greater than',
    'greater or equal',
    'greater or equals',
    'less',
    'less than',
    'less or equal',
    'less or equals',
    'contains',
    'not contains',
    'содержит',
    'не содержит',
    'равен',
    'не равен',
    'больше',
    'меньше',
    'больше или равно',
    'меньше или равно',
  ];

  public constructor(input: string) {
    this.input = input;
  }

  tokenize(): AbilityDSLToken[] {
    // Первичная токенизация
    while (!this.isAtEnd()) {
      this.skipWhitespace();
      if (this.isAtEnd()) break;

      const token = this.nextToken();
      if (token) {
        this.tokens.push(token);
        this.lastToken = token;
      }
    }

    // Склейка составных компараторов
    const rawTokens = [...this.tokens];
    this.tokens = [];

    let i = 0;
    while (i < rawTokens.length) {
      const token = rawTokens[i];
      if (token.type === AbilityDSLTokenType.condition) {
        // Пытаемся собрать максимально длинную последовательность condition-токенов
        let phrase = token.value;
        let j = i + 1;
        while (j < rawTokens.length && rawTokens[j].type === AbilityDSLTokenType.condition) {
          phrase += ' ' + rawTokens[j].value;
          j++;
        }
        // Проверяем, является ли фраза допустимым компаратором
        if (this.comparators.includes(phrase)) {
          // Создаём один токен для всей фразы
          this.tokens.push(
            new AbilityDSLToken(
              AbilityDSLTokenType.condition,
              this.normalizeComparator(phrase),
              token.position,
              token.parent,
            ),
          );
          i = j; // пропускаем все объединённые токены
          continue;
        }
      }
      // Если не смогли объединить, оставляем как есть
      this.tokens.push(token);
      i++;
    }

    return this.tokens;
  }

  private normalizeComparator(phrase: string): string {
    const map: Record<string, string> = {
      // English
      equals: '=',
      is: '=',
      'is not equals': '<>',
      'is not': '<>',
      'not equals': '<>',
      'greater than': '>',
      greater: '>',
      'more than': '>',
      more: '>',
      'less than': '<',
      less: '<',
      'greater or equal': '>=',
      'greater or equals': '>=',
      'more or equal': '>=',
      'more or equals': '>=',
      'less or equal': '<=',
      'less or equals': '<=',
      contains: 'in',
      has: 'in',
      in: 'in',
      'not contains': 'not in',
      'not has': 'not in',
      'not in': 'not in',

      // Russian
      равен: '=',
      равно: '=',
      является: '=',
      есть: '=',
      'не равен': '<>',
      'не равно': '<>',
      больше: '>',
      'больше чем': '>',
      меньше: '<',
      'меньше чем': '<',
      'больше или равно': '>=',
      'больше или равняется': '>=',
      'меньше или равно': '<=',
      'меньше или равняется': '<=',
      содержит: 'in',
      имеет: 'in',
      входит: 'in',
      'не содержит': 'not in',
      'не имеет': 'not in',
      'не входит': 'not in',
    };

    const normalized = phrase.trim().toLowerCase();
    return map[normalized] ?? phrase;
  }

  private nextToken(): AbilityDSLToken | null {
    const char = this.peek();
    const startPos = this.pos;

    if (char === '#') {
      this.skipComment();
      return null;
    }

    // Строки в кавычках
    if (char === '"' || char === "'") {
      return this.readString(startPos);
    }

    // Символы
    if (char === '(' || char === ')') {
      this.advance();
      return new AbilityDSLToken(AbilityDSLTokenType.symbol, char, startPos, this.lastToken);
    }

    // Слова, числа, пути
    if (this.isAlpha(char)) {
      return this.readWord(startPos);
    }

    // Цифры
    if (this.isDigit(char)) {
      return this.readNumber(startPos);
    }

    throw new Error(`Unexpected character: ${char} at position ${startPos}`);
  }

  private readString(startPos: number): AbilityDSLToken {
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
        return new AbilityDSLToken(AbilityDSLTokenType.string, value, startPos, this.lastToken);
      }

      value += char;
    }

    throw new Error(`Unterminated string at position ${startPos}`);
  }

  private readWord(startPos: number): AbilityDSLToken {
    const start = this.pos;

    while (!this.isAtEnd()) {
      const char = this.peek();
      if (this.isAlpha(char) || this.isDigit(char) || char === '.') {
        this.advance();
      } else {
        break;
      }
    }

    const value = this.input.slice(start, this.pos);

    // Проверяем, является ли слово частью составного компаратора
    if (this.comparators.includes(value)) {
      // Если это "or" и предыдущий токен — "greater" или "less", то это часть компаратора
      if (value === 'or') {
        const prev = this.lastToken;
        if (prev?.value === 'greater' || prev?.value === 'less') {
          return new AbilityDSLToken(
            AbilityDSLTokenType.condition,
            value,
            startPos,
            this.lastToken,
          );
        }
      }
      return new AbilityDSLToken(AbilityDSLTokenType.condition, value, startPos, this.lastToken);
    }

    // Логические операторы (and, or)
    if (this.compareMap[value]) {
      return new AbilityDSLToken(
        AbilityDSLTokenType.compare,
        this.compareMap[value],
        startPos,
        this.lastToken,
      );
    }

    // Эффекты (allow, deny, ...)
    if (this.effectsMap[value]) {
      return new AbilityDSLToken(
        AbilityDSLTokenType.effect,
        this.effectsMap[value],
        startPos,
        this.lastToken,
      );
    }

    // Путь или действие
    if (value.includes('.')) {
      if (this.lastToken?.type === AbilityDSLTokenType.effect) {
        return new AbilityDSLToken(AbilityDSLTokenType.action, value, startPos, this.lastToken);
      }
      return new AbilityDSLToken(AbilityDSLTokenType.path, value, startPos, this.lastToken);
    }

    // Обычное слово
    return new AbilityDSLToken(AbilityDSLTokenType.word, value, startPos, this.lastToken);
  }

  private readNumber(startPos: number): AbilityDSLToken {
    const start = this.pos;

    while (!this.isAtEnd()) {
      const char = this.peek();
      if (this.isDigit(char) || char === '.') {
        this.advance();
      } else {
        break;
      }
    }

    const value = this.input.slice(start, this.pos);

    return new AbilityDSLToken(AbilityDSLTokenType.number, value, startPos, this.lastToken);
  }

  private skipWhitespace(): void {
    while (!this.isAtEnd() && this.isWhitespace(this.peek())) {
      this.advance();
    }
  }

  private skipComment(): void {
    while (!this.isAtEnd() && !this.isNewline()) {
      this.advance();
    }
  }

  private isNewline(): boolean {
    return this.peek() === '\n';
  }

  private isWhitespace(char: string): boolean {
    return char === ' ' || char === '\t' || char === '\n' || char === '\r';
  }

  private isAlpha(char: string): boolean {
    return (
      (char >= 'a' && char <= 'z') ||
      (char >= 'A' && char <= 'Z') ||
      char === '_' ||
      (char >= 'а' && char <= 'я') ||
      (char >= 'А' && char <= 'Я')
    );
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
