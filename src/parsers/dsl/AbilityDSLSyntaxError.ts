export class AbilityDSLSyntaxError extends Error {
  private readonly _formattedMessage: string;
  private readonly _originalStack?: string;

  constructor(
    public readonly line: number,
    public readonly column: number,
    public readonly context: string, // строка DSL + ^ + соседние строки
    public readonly details: string, // текст ошибки
  ) {
    super(details.split('\n')[0]); // message = только первая строка
    this.name = 'AbilityDSLSyntaxError';

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, AbilityDSLSyntaxError);
    }
    this._originalStack = this.stack;

    this._formattedMessage = this.formatMessage();

    Object.defineProperty(this, 'stack', {
      get: () => this._formattedMessage,
      configurable: true,
    });
  }

  /**
   * Creates a syntax error with a DSL code fragment around the specified position
   * @param dsl - Full DSL source
   * @param line - Line number (1-based)
   * @param column - Column number (1-based)
   * @param length - Length of the highlighted fragment
   * @param details - Error text
   */
  public static fromPosition(
    dsl: string,
    line: number,
    column: number,
    length: number,
    details: string,
  ): AbilityDSLSyntaxError {
    const lines = dsl.split(/\r?\n/);
    const lineIdx = line - 1;
    const lineBefore = lineIdx > 0 ? (lines[lineIdx - 1] ?? '') : '';
    const current = lines[lineIdx] ?? '';
    const linesAfter = lineIdx + 1 < lines.length ? lines[lineIdx + 1] : '';
    const wave = ' '.repeat(Math.max(0, column - 1)) + '~'.repeat(Math.max(1, length));

    const lineNumWidth = String(line + 1).length;
    const num = (n: number) => String(n).padStart(lineNumWidth, ' ');

    let context = '';
    if (lineBefore.trim() !== '') {
      context += `${num(line - 1)} | ${lineBefore}\n`;
    }
    context += `${num(line)} | ${current}\n`;
    context += `${' '.repeat(lineNumWidth)} | ${wave}\n`;
    if (linesAfter.trim() !== '') {
      context += `${num(line + 1)} | ${linesAfter}`;
    }

    return new AbilityDSLSyntaxError(line, column, context + '\n', details);
  }

  private static supportsColor(): boolean {
    return typeof process !== 'undefined' && process.stdout?.isTTY;
  }

  private formatMessage(): string {
    const useColor = AbilityDSLSyntaxError.supportsColor();

    const BOLD = useColor ? '\x1b[1m' : '';
    const RED = useColor ? '\x1b[31m' : '';
    const ORANGE = useColor ? '\x1b[33;1m' : '';
    const GRAY = useColor ? '\x1b[90m' : '';
    const RESET = useColor ? '\x1b[0m' : '';


    const lines = this.context.split('\n');

    // Find line with ^
    const pointerIndex = lines.findIndex(l => l.includes('^') || l.includes('~'));
    const commentIndex = lines.findIndex(l => l.trim().includes('#'));

    const formattedLines = lines.map((line, idx) => {
      if (idx === pointerIndex - 1) {
        // Error line
        return `${BOLD}${ORANGE}${line}${RESET}`;
      }
      if (idx === pointerIndex) {
        // Error with ~~~~~
        return `${RED}${line}${RESET}`;
      }

      // Comments # ...
      if (idx === commentIndex) {
        return `${GRAY}${line}${RESET}`;
      }

      return line;
    });

    const contextBlock = formattedLines.join('\n');

    return `${BOLD}${RED}${this.name}: ${this.details}${RESET}\n\n` + contextBlock;
  }

  override toString(): string {
    return this._formattedMessage;
  }
}
