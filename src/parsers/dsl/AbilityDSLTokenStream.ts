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

  public syntaxError(details: string, token: AbilityDSLToken, expected?: TokenType[]): never {
    const lines = this.dsl.split(/\r?\n/);
    const lineIdx = token.line - 1;
    const lineBefore = lineIdx > 0 ? lines[lineIdx - 1] : '';
    const current = lines[lineIdx];
    const linesAfter = lineIdx + 1 < lines.length ? lines[lineIdx + 1] : '';
    const wave = ' '.repeat(Math.max(0, token.column - 1)) + '~'.repeat(token.value.length);

    const lineNumWidth = String(token.line + 1).length;
    const num = (n: number) => String(n).padStart(lineNumWidth, ' ');

    let context = '';
    if (lineBefore.trim() !== '') {
      context += `${num(token.line - 1)} | ${lineBefore}\n`;
    }
    context += `${num(token.line)} | ${current}\n`;
    context += `${' '.repeat(lineNumWidth)} | ${wave}\n`;
    if (linesAfter.trim() !== '') {
      context += `${num(token.line + 1)} | ${linesAfter}`;
    }

    let finalDetails = details;

    if (expected && expected?.length > 0) {
      const actual = token.value;
      const suggestion = this.suggest(actual, expected);
      const detailsMsg = `${details}\nDetails: Unexpected value token \`${actual}\``;
      finalDetails = suggestion ? `${detailsMsg} Did you mean \`${suggestion}\`?` : detailsMsg;
    }

    throw new AbilityDSLSyntaxError(token.line, token.column, context + '\n', finalDetails);
  }

  private suggest(actual: string, expectedTypes: TokenType[]): string | null {
    const candidates: string[] = [];
    for (const type of expectedTypes) {
      candidates.push(type);
    }
    const uniqueCandidates = [...new Set(candidates)];
    let best: string | null = null;
    let bestDist = 3;
    for (const candidate of uniqueCandidates) {
      const d = this.levenshteinDistance(actual.toLowerCase(), candidate.toLowerCase());
      if (d < bestDist) {
        bestDist = d;
        best = candidate;
      }
    }
    return best;
  }

  private levenshteinDistance(a: string, b: string): number {
    const matrix: number[][] = Array.from({ length: b.length + 1 }, () =>
      Array.from({ length: a.length + 1 }, () => 0),
    );

    for (let i = 0; i <= a.length; i++) matrix[0][i] = i;
    for (let j = 0; j <= b.length; j++) matrix[j][0] = j;

    for (let j = 1; j <= b.length; j++) {
      for (let i = 1; i <= a.length; i++) {
        const cost = a[i - 1] === b[j - 1] ? 0 : 1;

        matrix[j][i] = Math.min(
          matrix[j][i - 1] + 1,
          matrix[j - 1][i] + 1,
          matrix[j - 1][i - 1] + cost,
        );
      }
    }

    return matrix[b.length][a.length];
  }
}
