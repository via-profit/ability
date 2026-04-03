import { AbilityDSLToken, TokenType } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLSyntaxError } from '~/parsers/dsl/AbilityDSLSyntaxError';

export class AbilityDSLTokenStream {
  private readonly tokens: AbilityDSLToken[];
  private pos = 0;
  private readonly dsl: string;
  private marks: number[] = [];
  private lastToken: AbilityDSLToken | null = null;

  public next(): AbilityDSLToken {
    const token = this.tokens[this.pos++];
    this.lastToken = token;
    return token;
  }

  public previous(): AbilityDSLToken | null {
    return this.lastToken;
  }

  constructor(tokens: AbilityDSLToken[], dsl: string) {
    this.tokens = tokens;
    this.dsl = dsl;
  }

  public peek(): AbilityDSLToken {
    return this.tokens[this.pos];
  }

  public eof(): boolean {
    return this.peek().code === AbilityDSLToken.EOF;
  }

  public check(type: TokenType): boolean {
    if (this.eof()) return false;
    return this.peek().code === type;
  }

  public match(type: TokenType): AbilityDSLToken | null {
    if (this.check(type)) {
      return this.next();
    }
    return null;
  }

  public expect(type: TokenType, message: string): AbilityDSLToken {
    const token = this.peek();
    if (token && token.code === type) {
      return this.next();
    }
    this.syntaxError(message, token, [type]);
  }

  public expectOneOf(types: TokenType[], message: string): AbilityDSLToken {
    const token = this.peek();
    for (const t of types) {
      if (token && token.code === t) {
        return this.next();
      }
    }
    this.syntaxError(message, token, types);
  }

  private syntaxError(message: string, token: AbilityDSLToken, expected?: TokenType[]): never {
    throw new AbilityDSLSyntaxError(token.line, token.column, '', message);
  }

  public mark() {
    this.marks.push(this.pos);
  }

  public reset() {
    const pos = this.marks.pop();
    if (pos !== undefined) {
      this.pos = pos;
    }
  }

  public commit() {
    this.marks.pop();
  }
}
