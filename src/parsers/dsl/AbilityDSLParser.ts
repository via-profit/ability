import AbilityCompare from '~/core/AbilityCompare';
import AbilityCondition from '~/core/AbilityCondition';
import AbilityPolicy from '~/core/AbilityPolicy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import AbilityRule, { AbilityRuleConfig } from '~/core/AbilityRule';
import AbilityRuleSet from '~/core/AbilityRuleSet';
import { AbilityDSLLexer } from '~/parsers/dsl/AbilityDSLLexer';
import { AbilityDSLToken, TokenType } from '~/parsers/dsl/AbilityDSLToken';
import { ResourceObject } from '~/core/AbilityParser';
import { AbilityDSLSyntaxError } from '~/parsers/dsl/AbilityDSLSyntaxError';

/**
 * Parser for the Ability DSL.
 *
 * Converts a DSL string into one or more AbilityPolicy instances.
 * The grammar follows the structure:
 *
 *   <effect> <action> if <group> [ <group> ... ]
 *
 * where <group> is either "all of:" or "any of:", followed by a colon,
 * and then a list of rules (one per line).
 *
 * Each rule is: <identifier> <operator> <value>
 *
 * Operators can be simple (equals, contains, in) or
 * composed (is null, is not null, greater than, less than or equal, etc.).
 */
export class AbilityDSLParser<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment = unknown,
> {
  private tokens: AbilityDSLToken[] = [];
  private pos = 0;
  private annotationBuffer: Record<'name' | 'description', string | null> = {
    name: null,
    description: null,
  };

  constructor(private readonly dsl: string) {}

  /**
   * Main entry point: tokenize the input and parse all policies.
   * @returns Array of AbilityPolicy instances.
   */
  public parse(): AbilityPolicy<Resource, Environment>[] {
    // Tokenize the entire DSL string.
    this.tokens = new AbilityDSLLexer(this.dsl).tokenize();
    this.pos = 0;

    const policies: AbilityPolicy[] = [];

    // Keep parsing until we've consumed all tokens.
    while (!this.isAtEnd()) {
      this.consumeLeadingComments();
      // Every policy must start with an EFFECT token.
      if (!this.isStartOfPolicy()) {
        const token = this.peek();
        this.syntaxError(`Expected policy, got ${token.code}.`, token, [
          'EFFECT',
        ]);
      }
      policies.push(this.parsePolicy());
    }

    return policies;
  }

  // -------------------------------------------------------------------------
  // #region Policy parsing
  // -------------------------------------------------------------------------

  /**
   * Parses a single policy from the current token position.
   *
   * Grammar:
   *   policy = EFFECT ACTION IF (ALL | ANY) COLON ruleSets
   */
  private parsePolicy(): AbilityPolicy {
    this.consumeLeadingComments();
    const meta = this.takeAnnotations();

    // Effect: "permit" or "deny"
    const effectToken = this.consume(AbilityDSLToken.EFFECT, 'Expected effect');
    const effect = effectToken.value;

    // Action: e.g. "order.update"
    const actionToken = this.consume(AbilityDSLToken.ACTION, 'Expected action');
    const action = actionToken.value;

    // "if" keyword
    this.consume(AbilityDSLToken.IF, 'Expected "if"');

    // Group selector: "all" or "any" – determines how the top‑level rule sets are combined.
    const compareToken = this.consumeOneOf(
      [AbilityDSLToken.ALL, AbilityDSLToken.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.code === AbilityDSLToken.ALL
        ? AbilityCompare.and
        : AbilityCompare.or;

    // Colon after the group keyword
    this.consume(AbilityDSLToken.COLON, 'Expected ":"');

    // Parse the list of rule sets (each "all of:" or "any of:" block)
    const ruleSets = this.parseRuleSets(compareMethod);

    // Construct the policy instance.
    return new AbilityPolicy({
      id: `${effect}:${action}:${Math.random()}`,
      name: meta.name ?? `${effect} ${action}`,
      action,
      effect: effect === 'permit' ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny,
      compareMethod,
    }).addRuleSets(ruleSets);
  }

  // -------------------------------------------------------------------------
  // #region Rule set parsing (groups of rules)
  // -------------------------------------------------------------------------

  /**
   * Parses a sequence of rule sets (groups) until a new policy starts or EOF.
   */
  private parseRuleSets(policyCompareMethod: AbilityCompare): AbilityRuleSet[] {
    const sets: AbilityRuleSet[] = [];

    while (!this.isAtEnd() && !this.isStartOfPolicy()) {
      this.consumeLeadingComments();

      // Если начинается новая группа — парсим её
      if (this.isStartOfGroup()) {
        sets.push(this.parseGroup());
        continue;
      }

      // Иначе — implicit group (all-of по умолчанию)
      const meta = this.takeAnnotations();
      const group = new AbilityRuleSet({
        compareMethod: policyCompareMethod,
        name: meta.name,
      });

      // Читаем правила implicit-группы
      while (!this.isAtEnd()) {
        this.consumeLeadingComments();

        if (this.isStartOfGroup() || this.isStartOfPolicy()) {
          break;
        }

        if (this.check(AbilityDSLToken.IDENTIFIER)) {
          group.addRule(this.parseRule());
        } else {
          this.syntaxError(
            `Unexpected token in implicit group: ${this.peek().code}`,
            this.peek(),
          );
        }
      }

      sets.push(group);
    }

    return sets;
  }

  /**
   * Parses a single group, e.g. "all of:" or "any of:", and returns a RuleSet.
   */
  private parseGroup(): AbilityRuleSet {
    this.consumeLeadingComments();
    const meta = this.takeAnnotations();

    const compareToken = this.consumeOneOf(
      [AbilityDSLToken.ALL, AbilityDSLToken.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.code === AbilityDSLToken.ALL ? AbilityCompare.and : AbilityCompare.or;

    if (this.check(AbilityDSLToken.OF)) {
      this.advance();
    }

    this.consume(AbilityDSLToken.COLON, 'Expected ":"');

    const group = new AbilityRuleSet({ compareMethod, name: meta.name });

    while (!this.isAtEnd()) {
      this.consumeLeadingComments();

      if (this.isStartOfGroup() || this.isStartOfPolicy()) {
        break;
      }

      if (this.check(AbilityDSLToken.IDENTIFIER)) {
        group.addRule(this.parseRule());
      } else {
        this.syntaxError(`Unexpected token in group: ${this.peek().code}`, this.peek());
      }
    }

    return group;
  }

  // -------------------------------------------------------------------------
  // #region Rule parsing
  // -------------------------------------------------------------------------

  /**
   * Parses a single rule: subject operator value
   */
  private parseRule(): AbilityRule {
    this.consumeLeadingComments();
    const meta = this.takeAnnotations();
    if (!this.check(AbilityDSLToken.IDENTIFIER)) {
      this.syntaxError(`Expected identifier, got ${this.peek().code}`, this.peek());
    }

    // Subject (e.g., "user.roles")
    const subject = this.consume(AbilityDSLToken.IDENTIFIER, 'Expected field').value;

    // Operator (e.g., "contains", "equals", "is not null")
    const { condition, operator } = this.parseConditionOperator();

    let resource: AbilityRuleConfig['resource'];
    let beforePos = this.pos;

    // Special operators that don't consume a value token.
    if (
      operator === AbilityDSLToken.EQ_NULL ||
      operator === AbilityDSLToken.NOT_EQ_NULL ||
      operator === AbilityDSLToken.NULL
    ) {
      resource = null;
    } else {
      beforePos = this.pos;
      resource = this.parseValue();
    }

    // Checking that there are no extra tokens after the value
    // (skip comments)
    this.consumeLeadingComments();

    const resourceToken = this.tokens[beforePos];

    if (typeof resource === 'string' && resourceToken.code === AbilityDSLToken.IDENTIFIER && !resourceToken.value.includes('.')) {
          this.syntaxError(
            `Expected comparison operator or value, got \`${resource}\``,
            this.tokens[beforePos],
            [AbilityDSLToken.KEYWORD],
          );
    }

    return new AbilityRule({
      subject,
      resource,
      condition,
      name: meta.name,
    });
  }

  // -------------------------------------------------------------------------
  // #region Operator parsing
  // -------------------------------------------------------------------------

  /**
   * Parses the comparison operator part of a rule.
   * Returns both the resulting AbilityCondition and the token type that was consumed.
   */
  private parseConditionOperator(): { condition: AbilityCondition; operator: TokenType } {
    const savedPos = this.pos;

    // greater than or equal
    if (
      this.matchWord('greater') &&
      this.matchWord('than') &&
      this.matchWord('or') &&
      this.matchWord('equal')
    ) {
      return { condition: AbilityCondition.greater_or_equal, operator: AbilityDSLToken.GTE };
    }
    this.pos = savedPos;

    // greater than
    if (this.matchWord('greater') && this.matchWord('than')) {
      return { condition: AbilityCondition.greater_than, operator: AbilityDSLToken.GT };
    }
    this.pos = savedPos;

    // less than or equal
    if (
      this.matchWord('less') &&
      this.matchWord('than') &&
      this.matchWord('or') &&
      this.matchWord('equal')
    ) {
      return { condition: AbilityCondition.less_or_equal, operator: AbilityDSLToken.LTE };
    }
    this.pos = savedPos;

    // less than
    if (this.matchWord('less') && this.matchWord('than')) {
      return { condition: AbilityCondition.less_than, operator: AbilityDSLToken.LT };
    }
    this.pos = savedPos;

    // not contains
    if (this.matchWord('not') && this.matchWord('contains')) {
      return {
        condition: AbilityCondition.not_contains,
        operator: AbilityDSLToken.NOT_CONTAINS,
      };
    }
    this.pos = savedPos;

    // is equals
    if (this.matchWord('is') && this.matchWord('equals')) {
      return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
    }
    this.pos = savedPos;

    // not equal
    if (this.matchWord('not') && this.matchWord('equals')) {
      return { condition: AbilityCondition.not_equals, operator: AbilityDSLToken.NOT_EQ };
    }
    this.pos = savedPos;

    // is not equals
    if (this.matchWord('is') && this.matchWord('not') && this.matchWord('equals')) {
      return { condition: AbilityCondition.not_equals, operator: AbilityDSLToken.NOT_EQ };
    }
    this.pos = savedPos;

    // is in
    if (this.matchWord('is') && this.matchWord('in')) {
      return { condition: AbilityCondition.in, operator: AbilityDSLToken.IN };
    }
    this.pos = savedPos;

    // not in
    if (this.matchWord('not') && this.matchWord('in')) {
      return { condition: AbilityCondition.not_in, operator: AbilityDSLToken.NOT_IN };
    }
    this.pos = savedPos;

    // is not null
    if (this.matchWord('is') && this.matchWord('not')) {
      if (this.check(AbilityDSLToken.NULL)) {
        this.advance();
        return {
          condition: AbilityCondition.not_equals,
          operator: AbilityDSLToken.NOT_EQ_NULL,
        };
      }
    }
    this.pos = savedPos;

    // is null
    if (this.matchWord('is') && this.matchWord('null')) {
      if (this.check(AbilityDSLToken.NULL)) {
        this.advance();
        return {
          condition: AbilityCondition.equals,
          operator: AbilityDSLToken.EQ_NULL,
        };
      }
    }
    this.pos = savedPos;

    // Single token (symbol or keyword)

    const token = this.peek();

    if (
      token.code !== AbilityDSLToken.SYMBOL &&
      token.code !== AbilityDSLToken.KEYWORD &&
      token.code !== AbilityDSLToken.NULL
    ) {
      this.syntaxError(`Expected comparison operator, got \`${token.value}\``, token, [
        AbilityDSLToken.SYMBOL,
        AbilityDSLToken.KEYWORD,
        AbilityDSLToken.NULL,
      ]);
    }

    this.advance();

    switch (token.code) {
      case AbilityDSLToken.SYMBOL:
        if (token.value === '=')
          return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
        if (token.value === '!=')
          return { condition: AbilityCondition.not_equals, operator: AbilityDSLToken.NOT_EQ };
        if (token.value === '>')
          return { condition: AbilityCondition.greater_than, operator: AbilityDSLToken.GT };
        if (token.value === '<')
          return { condition: AbilityCondition.less_than, operator: AbilityDSLToken.LT };
        if (token.value === '>=')
          return { condition: AbilityCondition.greater_or_equal, operator: AbilityDSLToken.GTE };
        if (token.value === '<=')
          return { condition: AbilityCondition.less_or_equal, operator: AbilityDSLToken.LTE };
        break;

      case AbilityDSLToken.KEYWORD:
        if (token.value === 'contains')
          return { condition: AbilityCondition.contains, operator: AbilityDSLToken.CONTAINS };
        if (token.value === 'in')
          return { condition: AbilityCondition.in, operator: AbilityDSLToken.IN };
        if (token.value === 'equals')
          return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
        if (token.value === 'greater') {
          // If we come here, it means "greater" without "than" – treat as '>'
          return { condition: AbilityCondition.greater_than, operator: AbilityDSLToken.GT };
        }
        if (token.value === 'less') {
          return { condition: AbilityCondition.less_than, operator: AbilityDSLToken.LT };
        }
        if (token.value === 'is') {
          // "is" alone -> equals
          return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
        }
        break;

      default:
        break;
    }
    return this.syntaxError(`Unexpected operator token \`${token.value}\``, token, [
      AbilityDSLToken.SYMBOL,
      AbilityDSLToken.KEYWORD,
    ]);
  }

  /**
   * Helper to match and consume a specific word token (KEYWORD or IDENTIFIER).
   * @param word The exact string to look for.
   * @returns True if the next token has that value.
   */
  private matchWord(word: string): boolean {
    if (this.isAtEnd()) return false;
    const token = this.peek();
    if ((token.code === AbilityDSLToken.KEYWORD || token.code === AbilityDSLToken.IDENTIFIER) && token.value === word) {
      this.advance();
      return true;
    }
    return false;
  }

  // -------------------------------------------------------------------------
  // #region Value parsing (literals, arrays, identifiers)
  // -------------------------------------------------------------------------

  /**
   * Parses a resource value. Can be a string literal, number, boolean,
   * null, a path (identifier), or an array.
   */
  private parseValue(): AbilityRuleConfig['resource'] {
    // Arrays start with a left bracket
    if (this.check(AbilityDSLToken.LBRACKET)) {
      this.advance();
      return this.parseArray();
    }

    // Ensure we are not about to read a structural token as a value.
    const token = this.peek();
    if (
      token.code === AbilityDSLToken.ALL ||
      token.code === AbilityDSLToken.ANY ||
      token.code === AbilityDSLToken.EFFECT
    ) {
      this.syntaxError(`Unexpected ${token.code} in value position`, token);
    }

    this.advance();

    // CHECK THIS SWITCH COMPARE
    switch (token.code) {
      case AbilityDSLToken.STRING:
        return token.value;
      case AbilityDSLToken.NUMBER:
        return Number(token.value);
      case AbilityDSLToken.BOOLEAN:
        return token.value === 'true';
      case AbilityDSLToken.NULL:
        return null;
      case AbilityDSLToken.IDENTIFIER:
        return token.value;
      default:
      {
        this.syntaxError(
          `Unexpected value token "${token.value}"`,
          token ?? this.tokens[this.tokens.length - 1],
          [AbilityDSLToken.KEYWORD],
        );
      }
    }
  }

  /**
   * Parses an array literal: [ <value>, <value>, ... ]
   * The opening bracket has already been consumed.
   */
  private parseArray(): (string | number | boolean | null)[] {
    const arr: (string | number | boolean | null)[] = [];

    // Handle empty array
    if (this.check(AbilityDSLToken.RBRACKET)) {
      this.advance();
      return arr;
    }

    while (!this.isAtEnd() && !this.check(AbilityDSLToken.RBRACKET)) {
      const value = this.parseValue();

      // Flatten nested arrays if they appear (though grammar doesn't currently allow nesting).
      if (Array.isArray(value)) {
        arr.push(...value);
      } else if (
        typeof value === 'string' ||
        typeof value === 'number' ||
        typeof value === 'boolean'
      ) {
        arr.push(value);
      } else if (value === null) {
        // Null is allowed in arrays? Currently, we throw.
        this.syntaxError('Unexpected null in array', this.peek());
      }

      // Optional comma between elements
      if (this.check(AbilityDSLToken.COMMA)) {
        this.advance();
      }
    }

    this.consume(AbilityDSLToken.RBRACKET, 'Expected "]"');
    return arr;
  }

  // -------------------------------------------------------------------------
  // #region Annotations and comments
  // -------------------------------------------------------------------------
  private consumeLeadingComments() {
    while (this.check(AbilityDSLToken.COMMENT)) {
      const token = this.advance();
      this.processCommentToken(token);
    }
  }

  private processCommentToken(token: AbilityDSLToken) {
    const text = token.value.trim();

    if (text.startsWith('@name ')) {
      this.annotationBuffer.name = text.slice(6).trim();
    }

    if (text.startsWith('@description ')) {
      this.annotationBuffer.description = text.slice(13).trim();
    }
  }

  private takeAnnotations(): typeof this.annotationBuffer {
    const meta = { ...this.annotationBuffer };
    this.annotationBuffer = {
      name: null,
      description: null,
    };
    return meta;
  }

  // -------------------------------------------------------------------------
  // #region Errors
  // -------------------------------------------------------------------------

  private syntaxError(details: string, token: AbilityDSLToken, expected?: TokenType[]): never {
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
      const message = `Unexpected value token "${token.value}"`;
      const detailsMsg = `${message}\nDetails: Unexpected value token \`${actual}\``;
      finalDetails = suggestion ? `${detailsMsg} Did you mean \`${suggestion}\`?` : detailsMsg;
    }


    throw new AbilityDSLSyntaxError(token.line, token.column, context + '\n', finalDetails);
  }

  private getLine(lineNumber: number): string {
    return this.dsl.split(/\r?\n/)[lineNumber - 1] ?? '';
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
    const matrix = Array(b.length + 1)
      .fill(null)
      .map(() => Array(a.length + 1).fill(null));
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

  // -------------------------------------------------------------------------
  // #region Helper / lookahead methods
  // -------------------------------------------------------------------------
  private consumeOneOf(types: TokenType[], message: string): AbilityDSLToken {
    const token = this.peek();
    for (const t of types) {
      if (token && token.code === t) {
        return this.advance();
      }
    }
    const expected = types.map(t => t).join(', ');
    const actual = token ? token.value : AbilityDSLToken.EOF;
    const suggestion = this.suggest(actual, types);
    const details = `${message}\nDetails: Unexpected token \`${actual}\`, expected one of: ${expected}.`;
    const finalMsg = suggestion ? `${details} Did you mean \`${suggestion}\`?` : details;
    this.syntaxError(finalMsg, token ?? this.tokens[this.tokens.length - 1]);
  }

  private consume(type: TokenType, message: string): AbilityDSLToken {
    const token = this.peek();
    if (token && token.code === type) {
      return this.advance();
    }
    const expected = type;
    const actual = token ? token.value : AbilityDSLToken.EOF;
    const suggestion = this.suggest(actual, [type]);
    const details = `${message}\nDetails: Unexpected token \`${actual}\`, expected "${expected}".`;
    const finalMsg = suggestion ? `${details} Did you mean \`${suggestion}\`?` : details;
    this.syntaxError(finalMsg, token ?? this.tokens[this.tokens.length - 1]);
  }

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().code === type;
  }

  private isStartOfPolicy(): boolean {
    return this.check(AbilityDSLToken.EFFECT);
  }

  private isStartOfGroup(): boolean {
    return this.check(AbilityDSLToken.ALL) || this.check(AbilityDSLToken.ANY);
  }

  private advance(): AbilityDSLToken {
    return this.tokens[this.pos++];
  }

  private peek(): AbilityDSLToken {
    return this.tokens[this.pos];
  }

  private isAtEnd(): boolean {
    return this.peek().code === AbilityDSLToken.EOF;
  }
}
