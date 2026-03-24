import AbilityCompare from '~/core/AbilityCompare';
import AbilityCondition from '~/core/AbilityCondition';
import AbilityPolicy from '~/core/AbilityPolicy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import AbilityRule, { AbilityRuleConfig } from '~/core/AbilityRule';
import AbilityRuleSet from '~/core/AbilityRuleSet';
import { AbilityDSLLexer } from '~/parsers/dsl/AbilityDSLLexer';
import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

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
 * Operators can be simple (equals, contains, in, greater, less) or
 * composed (is null, is not null, greater than, less than, etc.).
 */
export class AbilityDSLParser {
  private tokens: AbilityDSLToken[] = [];
  private pos = 0;

  constructor(private readonly dsl: string) {}

  /**
   * Main entry point: tokenize the input and parse all policies.
   * @returns Array of AbilityPolicy instances.
   */
  public parse(): AbilityPolicy[] {
    // Tokenize the entire DSL string.
    this.tokens = new AbilityDSLLexer(this.dsl).tokenize();
    this.pos = 0;

    const policies: AbilityPolicy[] = [];

    // Keep parsing until we've consumed all tokens.
    while (!this.isAtEnd()) {
      // Every policy must start with an EFFECT token.
      if (!this.isStartOfPolicy()) {
        throw new Error(`Expected policy, got ${this.peek().type.code}`);
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
    // Effect: "permit" or "deny"
    const effectToken = this.consume(AbilityDSLTokenType.EFFECT, 'Expected effect');
    const effect = effectToken.value;

    // Action: e.g. "order.update"
    const actionToken = this.consume(AbilityDSLTokenType.ACTION, 'Expected action');
    const action = actionToken.value;

    // "if" keyword
    this.consume(AbilityDSLTokenType.IF, 'Expected "if"');

    // Group selector: "all" or "any" – determines how the top‑level rule sets are combined.
    const compareToken = this.consumeOneOf(
      [AbilityDSLTokenType.ALL, AbilityDSLTokenType.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.type === AbilityDSLTokenType.ALL ? AbilityCompare.and : AbilityCompare.or;

    // Colon after the group keyword
    this.consume(AbilityDSLTokenType.COLON, 'Expected ":"');

    // Parse the list of rule sets (each "all of:" or "any of:" block)
    const ruleSets = this.parseRuleSets();

    // Construct the policy instance.
    return new AbilityPolicy({
      id: `${effect}:${action}:${Math.random()}`,
      name: `${effect} ${action}`,
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
  private parseRuleSets(): AbilityRuleSet[] {
    const sets: AbilityRuleSet[] = [];

    // Continue while we haven't reached the end, and we're not at the start of a new policy.
    while (!this.isAtEnd() && !this.isStartOfPolicy()) {
      if (this.isStartOfGroup()) {
        sets.push(this.parseGroup());
      } else {
        // If we encounter something that isn't a group start, it's an error.
        break;
      }
    }

    return sets;
  }

  /**
   * Parses a single group, e.g. "all of:" or "any of:", and returns a RuleSet.
   */
  private parseGroup(): AbilityRuleSet {
    // Read the group keyword (ALL or ANY)
    const compareToken = this.consumeOneOf(
      [AbilityDSLTokenType.ALL, AbilityDSLTokenType.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.type === AbilityDSLTokenType.ALL ? AbilityCompare.and : AbilityCompare.or;

    // "of" keyword
    this.consume(AbilityDSLTokenType.OF, 'Expected "of"');

    // Colon after "of"
    this.consume(AbilityDSLTokenType.COLON, 'Expected ":"');

    // Create the rule set that will hold the rules inside this group.
    const group = new AbilityRuleSet({ compareMethod });

    // Read rules until we encounter either:
    //   - the start of another group (ALL or ANY)
    //   - the start of a new policy (EFFECT)
    //   - end of input
    while (!this.isAtEnd() && !this.isStartOfGroup() && !this.isStartOfPolicy()) {
      if (this.check(AbilityDSLTokenType.IDENTIFIER)) {
        group.addRule(this.parseRule());
      } else {
        // Any other token inside a group is unexpected.
        throw new Error(`Unexpected token in group: ${this.peek().type.code}`);
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
    if (!this.check(AbilityDSLTokenType.IDENTIFIER)) {
      throw new Error(`Expected identifier, got ${this.peek().type.code}`);
    }

    // Subject (e.g., "user.roles")
    const subject = this.consume(AbilityDSLTokenType.IDENTIFIER, 'Expected field').value;

    // Operator (e.g., "contains", "equals", "is not null")
    const { condition, operator } = this.parseConditionOperator();

    let resource: AbilityRuleConfig['resource'];

    // Special operators that don't consume a value token.
    if (
      operator === AbilityDSLTokenType.EQ_NULL ||
      operator === AbilityDSLTokenType.NOT_EQ_NULL ||
      operator === AbilityDSLTokenType.NULL
    ) {
      resource = null;
    } else {
      resource = this.parseValue();
    }

    return new AbilityRule({
      subject,
      resource,
      condition,
    });
  }

  // -------------------------------------------------------------------------
  // #region Operator parsing
  // -------------------------------------------------------------------------

  /**
   * Parses the comparison operator part of a rule.
   * Returns both the resulting AbilityCondition and the token type that was consumed.
   */
  private parseConditionOperator(): { condition: AbilityCondition; operator: AbilityDSLTokenType } {
    const token = this.advance();

    switch (token.type) {
      case AbilityDSLTokenType.EQ:
        // Simple "equals" or "is". If the next token is "null", treat as "is null".
        if (this.check(AbilityDSLTokenType.NULL)) {
          this.advance();
          return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.EQ_NULL };
        }
        return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.EQ };

      case AbilityDSLTokenType.EQ_NULL:
        // Already handled "is null"
        return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.EQ_NULL };

      case AbilityDSLTokenType.NOT_EQ_NULL:
        // Already handled "is not null"
        return { condition: AbilityCondition.not_equal, operator: AbilityDSLTokenType.NOT_EQ_NULL };

      case AbilityDSLTokenType.CONTAINS:
        return { condition: AbilityCondition.in, operator: AbilityDSLTokenType.CONTAINS };

      case AbilityDSLTokenType.IN:
        return { condition: AbilityCondition.in, operator: AbilityDSLTokenType.IN };

      case AbilityDSLTokenType.GT_WORD:
        // "greater" optionally followed by "equal" → "greater or equal"
        if (this.matchWord('equal')) {
          return {
            condition: AbilityCondition.more_or_equal,
            operator: AbilityDSLTokenType.GT_WORD,
          };
        }
        return { condition: AbilityCondition.more_than, operator: AbilityDSLTokenType.GT_WORD };

      case AbilityDSLTokenType.LT_WORD:
        // "less" optionally followed by "equal" → "less or equal"
        if (this.matchWord('equal')) {
          return {
            condition: AbilityCondition.less_or_equal,
            operator: AbilityDSLTokenType.LT_WORD,
          };
        }
        return { condition: AbilityCondition.less_than, operator: AbilityDSLTokenType.LT_WORD };

      case AbilityDSLTokenType.NULL:
        return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.NULL };

      default:
        throw new Error(`Unexpected operator token: ${token.type}`);
    }
  }

  /**
   * Helper to match and consume a specific word token.
   * @param word The exact string to look for.
   * @returns True if the next token is a word with that value.
   */
  private matchWord(word: string): boolean {
    if (this.peek()?.value === word) {
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
    if (this.check(AbilityDSLTokenType.LBRACKET)) {
      this.advance();
      return this.parseArray();
    }

    // Ensure we are not about to read a structural token as a value.
    const token = this.peek();
    if (
      token.type.code === AbilityDSLTokenType.ALL.code ||
      token.type.code === AbilityDSLTokenType.ANY.code ||
      token.type.code === AbilityDSLTokenType.EFFECT.code
    ) {
      throw new Error(`Unexpected ${token.type} in value position`);
    }

    this.advance();

    switch (token.type) {
      case AbilityDSLTokenType.STRING:
        return token.value;
      case AbilityDSLTokenType.NUMBER:
        return Number(token.value);
      case AbilityDSLTokenType.BOOLEAN:
        return token.value === 'true';
      case AbilityDSLTokenType.NULL:
        return null;
      case AbilityDSLTokenType.IDENTIFIER:
        return token.value;
      default:
        throw new Error(`Unexpected value token: ${token.type}`);
    }
  }

  /**
   * Parses an array literal: [ <value>, <value>, ... ]
   * The opening bracket has already been consumed.
   */
  private parseArray(): (string | number | boolean)[] {
    const arr: (string | number | boolean)[] = [];

    // Handle empty array
    if (this.check(AbilityDSLTokenType.RBRACKET)) {
      this.advance();
      return arr;
    }

    while (!this.isAtEnd() && !this.check(AbilityDSLTokenType.RBRACKET)) {
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
        throw new Error('Unexpected null in array');
        // arr.push(null);
      }

      // Optional comma between elements
      if (this.check(AbilityDSLTokenType.COMMA)) {
        this.advance();
      }
    }

    this.consume(AbilityDSLTokenType.RBRACKET, 'Expected "]"');
    return arr;
  }

  // -------------------------------------------------------------------------
  // #region Helper / lookahead methods
  // -------------------------------------------------------------------------

  /**
   * Consumes one token from the list, ensuring it matches any of the given types.
   */
  private consumeOneOf(types: AbilityDSLTokenType[], msg: string): AbilityDSLToken {
    for (const t of types) {
      if (this.check(t)) {
        return this.advance();
      }
    }
    throw new Error(msg);
  }

  /**
   * Checks if the current token matches the given type and advances if so.
   */
  private match(type: AbilityDSLTokenType): boolean {
    if (this.check(type)) {
      this.advance();
      return true;
    }
    return false;
  }

  /**
   * Consumes the current token if it matches the given type; otherwise throws.
   */
  private consume(type: AbilityDSLTokenType, msg: string): AbilityDSLToken {
    if (this.check(type)) {
      return this.advance();
    }
    throw new Error(msg + ` at token ${this.peek()?.value}`);
  }

  /**
   * Checks whether the current token has the given type.
   */
  private check(type: AbilityDSLTokenType): boolean {
    if (this.isAtEnd()) {
      return false;
    }
    return this.peek().type.code === type.code;
  }

  /**
   * Looks one token ahead.
   */
  private checkNext(type: AbilityDSLTokenType): boolean {
    if (this.pos + 1 >= this.tokens.length) {
      return false;
    }
    return this.tokens[this.pos + 1].type.code === type.code;
  }

  /**
   * Returns true if the current token is the start of a new policy (EFFECT).
   */
  private isStartOfPolicy(): boolean {
    return this.check(AbilityDSLTokenType.EFFECT);
  }

  /**
   * Returns true if the current token is the start of a new group (ALL or ANY).
   */
  private isStartOfGroup(): boolean {
    return this.check(AbilityDSLTokenType.ALL) || this.check(AbilityDSLTokenType.ANY);
  }

  /**
   * Advances the token position and returns the consumed token.
   */
  private advance(): AbilityDSLToken {
    return this.tokens[this.pos++];
  }

  /**
   * Returns the current token without consuming it.
   */
  private peek(): AbilityDSLToken {
    return this.tokens[this.pos];
  }

  /**
   * Checks if the parser has reached the end of the token stream.
   */
  private isAtEnd(): boolean {
    return this.peek().type.code === AbilityDSLTokenType.EOF.code;
  }
}
