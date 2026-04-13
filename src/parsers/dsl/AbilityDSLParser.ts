import { AbilityCompare, AbilityCompareType } from '../../core/AbilityCompare';
import { AbilityCondition, AbilityConditionType } from '../../core/AbilityCondition';
import AbilityPolicy from '../../core/AbilityPolicy';
import { AbilityPolicyEffect } from '../../core/AbilityPolicyEffect';
import AbilityRule, { AbilityRuleConfig } from '../../core/AbilityRule';
import AbilityRuleSet from '../../core/AbilityRuleSet';
import { AbilityDSLLexer } from './AbilityDSLLexer';
import { AbilityDSLToken, TokenType, TokenTypes } from './AbilityDSLToken';
import { EnvironmentObject, ResourceObject } from '../../core/AbilityTypeGenerator';
import { AbilityDSLTokenStream } from './AbilityDSLTokenStream';
import { AbilityDSLAnnotations, AnnotationName } from '~/parsers/dsl/AbilityDSLAnnotations';
import { AnnotationAllowed } from './AbilityDSLAnnotationMatrix';
import { AbilityDSLAliases } from '~/parsers/dsl/AbilityDSLAliases';

/**
 * Parser for the Ability DSL.
 *
 * Converts a DSL string into one or more AbilityPolicy instances.
 * The grammar follows the structure:
 *
 *   <effect> <permission> if <group> [ <group> ... ]
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
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
  T extends string = string,
> {
  private readonly dsl: string;
  private stream!: AbilityDSLTokenStream;
  private annBuffer: AbilityDSLAnnotations = new AbilityDSLAnnotations();
  private aliasBuffer: AbilityDSLAliases = new AbilityDSLAliases();

  constructor(dsl: string) {
    this.dsl = dsl;
  }

  /**
   * Main entry point: tokenize the input and parse all policies.
   * @returns Array of AbilityPolicy instances.
   */
  public parse(): readonly AbilityPolicy<R, E, T>[] {
    this.annBuffer.clear();
    // 1. Лексер → токены
    const tokens = new AbilityDSLLexer(this.dsl).tokenize();

    // 2. Создаём TokenStream
    this.stream = new AbilityDSLTokenStream(tokens, this.dsl);

    const policies: AbilityPolicy<R, E, T>[] = [];

    while (!this.stream.eof()) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();
      this.consumeLeadingAliases();

      if (!this.isStartOfPolicy()) {
        const token = this.stream.peek();
        this.stream.syntaxError(`Expected policy, got ${token.type}.`, token, [TokenTypes.EFFECT]);
      }

      policies.push(this.parsePolicy() as AbilityPolicy<R, E, T>);
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
   *   policy = EFFECT PERMISSION IF (ALL | ANY) COLON ruleSets
   */
  private parsePolicy(): AbilityPolicy {
    this.consumeLeadingComments();
    this.consumeLeadingAnnotations();
    this.consumeLeadingAliases();
    const annotations = this.takeAnnotations('policy');

    // Effect: "permit" or "deny"
    const effectToken = this.stream.expect(TokenTypes.EFFECT, 'Expected effect');
    const effect = effectToken.value;

    // Permission: e.g. "order.update"
    const permissionToken = this.stream.expect(TokenTypes.PERMISSION, 'Expected permission');
    const permission = permissionToken.value;
    if (!permission.startsWith('permission.')) {
      return this.stream.syntaxError(
        `Unexpected token. The permission key, must be starts with prefix \`permission.\`, but got \`${permission}\`.\nDid you mean \`permission.${permission}\`?`,
        permissionToken,
      );
    }

    // "if" keyword
    this.stream.expect(TokenTypes.IF, 'Expected "if"');

    // Group selector: "all" or "any" – determines how the top‑level rule sets are combined.
    const compareToken = this.stream.expectOneOf(
      [TokenTypes.ALL, TokenTypes.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.type === TokenTypes.ALL ? AbilityCompare.and : AbilityCompare.or;

    // Colon after the group keyword
    this.stream.expect(TokenTypes.COLON, 'Expected ":"');

    // Parse the list of rule sets (each "all of:" or "any of:" block)
    const ruleSets = this.parseRuleSets(compareMethod);

    // Construct the policy instance.
    return new AbilityPolicy<R, E, T>({
      id: annotations.id?.value || null,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      priority: annotations.priority?.value || null,
      permission: permission.replace(/^permission\./, ''),
      effect: effect === 'permit' ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny,
      disabled: annotations.disabled?.value ?? undefined,
      tags: annotations.tags?.value ?? undefined,
      compareMethod,
    }).addRuleSets(ruleSets);
  }

  // -------------------------------------------------------------------------
  // #region Rule set parsing (groups of rules)
  // -------------------------------------------------------------------------

  /**
   * Parses a sequence of rule sets (groups) until a new policy starts or EOF.
   */
  private parseRuleSets(policyCompareMethod: AbilityCompareType): AbilityRuleSet[] {
    const sets: AbilityRuleSet[] = [];

    while (!this.stream.eof() && !this.isStartOfPolicy()) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      // Если начинается новая except группа — парсим её
      if (this.isStartOfExcept()) {
        sets.push(this.parseExceptGroup(policyCompareMethod));
        continue;
      }

      // Если начинается новая группа — парсим её
      if (this.isStartOfGroup()) {
        sets.push(this.parseGroup());
        continue;
      }

      const annotation = this.takeAnnotations('ruleSet');

      const group = new AbilityRuleSet({
        id: annotation.id?.value || null,
        compareMethod: policyCompareMethod,
        name: annotation.name?.value ?? null,
        description: annotation.description?.value || null,
        disabled: annotation.disabled?.value ?? undefined,
      });

      // Читаем правила implicit-группы
      while (!this.stream.eof()) {
        this.consumeLeadingComments();
        this.consumeLeadingAnnotations();

        if (this.isStartOfGroup() || this.isStartOfPolicy() || this.isStartOfExcept()) {
          break;
        }

        if (
          this.stream.check(TokenTypes.IDENTIFIER) ||
          this.stream.check(TokenTypes.ALWAYS) ||
          this.stream.check(TokenTypes.NEVER)
        ) {
          group.addRule(this.parseRule());
        } else {
          this.stream.syntaxError(
            `Unexpected token in implicit group: ${this.stream.peek().type}`,
            this.stream.peek(),
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
    this.consumeLeadingAnnotations();
    const annotations = this.takeAnnotations('ruleSet');

    const compareToken = this.stream.expectOneOf(
      [TokenTypes.ALL, TokenTypes.ANY, TokenTypes.ALWAYS, TokenTypes.NEVER],
      'Expected "all" or "any" or "always" or "never"',
    );

    const compareMethod =
      compareToken.type === TokenTypes.ALL ? AbilityCompare.and : AbilityCompare.or;

    if (this.stream.check(TokenTypes.OF)) {
      this.stream.next();
    }

    this.stream.expect(TokenTypes.COLON, 'Expected ":"');

    const group = new AbilityRuleSet({
      id: annotations.id?.value || null,
      compareMethod,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      disabled: annotations.disabled?.value ?? undefined,
    });

    while (!this.stream.eof()) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      if (this.isStartOfExcept()) {
        break;
      }

      if (this.isStartOfGroup() || this.isStartOfPolicy()) {
        break;
      }

      if (this.stream.check(TokenTypes.IDENTIFIER)) {
        group.addRule(this.parseRule());
      } else {
        this.stream.syntaxError(
          `Unexpected token in group: ${this.stream.peek().type}`,
          this.stream.peek(),
        );
      }
    }

    return group;
  }

  // -------------------------------------------------------------------------
  // #region Except RuleSet parsing
  // -------------------------------------------------------------------------
  private parseExceptGroup(policyCompareMethod: AbilityCompareType): AbilityRuleSet {
    this.consumeLeadingComments();
    this.consumeLeadingAnnotations();
    const annotations = this.takeAnnotations('ruleSet');

    // consume "except"
    this.stream.expect(TokenTypes.EXCEPT, 'Expected "except"');

    let compareMethod = policyCompareMethod;

    // optional: "all" / "any"
    if (this.stream.check(TokenTypes.ALL) || this.stream.check(TokenTypes.ANY)) {
      const compareToken = this.stream.next();
      compareMethod = compareToken.type === TokenTypes.ALL ? AbilityCompare.and : AbilityCompare.or;

      if (this.stream.check(TokenTypes.OF)) {
        this.stream.next();
      }

      this.stream.expect(TokenTypes.COLON, 'Expected ":" after except group');
    } else {
      // implicit except group — no "all/any of:"
      // but still must end with colon
      this.stream.expect(TokenTypes.COLON, 'Expected ":" after "except"');
    }

    const group = new AbilityRuleSet({
      id: annotations.id?.value || null,
      compareMethod,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      isExcept: true,
      disabled: annotations.disabled?.value ?? undefined,
    });

    // read rules
    while (!this.stream.eof()) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      if (this.isStartOfGroup() || this.isStartOfPolicy() || this.isStartOfExcept()) {
        break;
      }

      if (this.stream.check(TokenTypes.IDENTIFIER)) {
        group.addRule(this.parseRule());
      } else {
        this.stream.syntaxError(
          `Unexpected token in except group: ${this.stream.peek().type}`,
          this.stream.peek(),
        );
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
    this.consumeLeadingAnnotations();
    const annotations = this.takeAnnotations('rule');

    const isNeverAlways =
      this.stream.check(TokenTypes.ALWAYS) || this.stream.check(TokenTypes.NEVER);





    if (!isNeverAlways && !this.stream.check(TokenTypes.IDENTIFIER)) {
      this.stream.syntaxError(
        `Expected identifier, but got ${this.stream.peek().type}`,
        this.stream.peek(),
      );
    }

    // subject
    const subject = isNeverAlways
      ? ''
      : this.stream.expect(TokenTypes.IDENTIFIER, 'Expected field').value;


    // check alias
    if (this.aliasBuffer.has(subject)) {
      return this.aliasBuffer.get(subject)!;
    }
      // operator
      const { condition, operator } = this.parseConditionOperator();

    // value
    let resource: AbilityRuleConfig['resource'] = null;
    let valueToken: AbilityDSLToken | null = null;

    const operatorConsumesValue =
      operator !== TokenTypes.EQ_NULL &&
      operator !== TokenTypes.NOT_EQ_NULL &&
      operator !== TokenTypes.NULL &&
      operator !== TokenTypes.ALWAYS &&
      operator !== TokenTypes.NEVER;

    if (operatorConsumesValue) {
      this.stream.mark();
      resource = this.parseValue();
      valueToken = this.stream.lookPrev();
      this.stream.commit();
    }

    this.consumeLeadingComments();
    this.consumeLeadingAnnotations();
    this.consumeLeadingAliases();

    // validation: identifier without dot → error
    if (
      typeof resource === 'string' &&
      valueToken &&
      valueToken.type === TokenTypes.IDENTIFIER &&
      !valueToken.value.includes('.')
    ) {
      this.stream.syntaxError(
        `Expected comparison operator or value, got \`${resource}\``,
        valueToken,
        [TokenTypes.KEYWORD],
      );
    }

    return new AbilityRule({
      id: annotations.id?.value || null,
      subject,
      resource,
      condition,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      disabled: annotations.disabled?.value ?? undefined,
    });
  }

  // -------------------------------------------------------------------------
  // #region Operator parsing
  // -------------------------------------------------------------------------

  /**
   * Parses the comparison operator part of a rule.
   * Returns both the resulting AbilityCondition and the token type that was consumed.
   */
  private parseConditionOperator(): { condition: AbilityConditionType; operator: TokenType } {
    // "always"
    this.stream.mark();
    if (this.matchWord('always')) {
      this.stream.commit();
      return { condition: AbilityCondition.always, operator: TokenTypes.ALWAYS };
    }
    this.stream.reset();

    // "never"
    this.stream.mark();
    if (this.matchWord('never')) {
      this.stream.commit();
      return { condition: AbilityCondition.never, operator: TokenTypes.NEVER };
    }
    this.stream.reset();

    // "length equals"
    this.stream.mark();
    if (this.matchWord('length') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_equals, operator: TokenTypes.LEN_EQ };
    }
    this.stream.reset();

    // "length ="
    this.stream.mark();
    if (this.matchWord('length') && this.matchSymbol('=')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_equals, operator: TokenTypes.LEN_EQ };
    }
    this.stream.reset();

    // "length greater than"
    this.stream.mark();
    if (this.matchWord('length') && this.matchWord('greater') && this.matchWord('than')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_greater_than, operator: TokenTypes.LEN_GT };
    }
    this.stream.reset();

    // "length >"
    this.stream.mark();
    if (this.matchWord('length') && this.matchSymbol('>')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_greater_than, operator: TokenTypes.LEN_GT };
    }
    this.stream.reset();

    // "length less than"
    this.stream.mark();
    if (this.matchWord('length') && this.matchWord('less') && this.matchWord('than')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_less_than, operator: TokenTypes.LEN_LT };
    }
    this.stream.reset();

    // "length <"
    this.stream.mark();
    if (this.matchWord('length') && this.matchSymbol('<')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_less_than, operator: TokenTypes.LEN_LT };
    }
    this.stream.reset();

    // "greater than or equal"
    this.stream.mark();
    if (
      this.matchWord('greater') &&
      this.matchWord('than') &&
      this.matchWord('or') &&
      this.matchWord('equal')
    ) {
      this.stream.commit();
      return { condition: AbilityCondition.greater_or_equal, operator: TokenTypes.GTE };
    }
    this.stream.reset();

    // greater than
    this.stream.mark();
    if (this.matchWord('greater') && this.matchWord('than')) {
      this.stream.commit();
      return { condition: AbilityCondition.greater_than, operator: TokenTypes.GT };
    }
    this.stream.reset();

    // less than or equal
    this.stream.mark();
    if (
      this.matchWord('less') &&
      this.matchWord('than') &&
      this.matchWord('or') &&
      this.matchWord('equal')
    ) {
      this.stream.commit();
      return { condition: AbilityCondition.less_or_equal, operator: TokenTypes.LTE };
    }
    this.stream.reset();

    // less than
    if (this.matchWord('less') && this.matchWord('than')) {
      return { condition: AbilityCondition.less_than, operator: TokenTypes.LT };
    }
    this.stream.reset();

    // "not contains"
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('contains')) {
      this.stream.commit();
      return {
        condition: AbilityCondition.not_contains,
        operator: TokenTypes.NOT_CONTAINS,
      };
    }
    this.stream.reset();

    // "not includes"
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('includes')) {
      this.stream.commit();
      return {
        condition: AbilityCondition.not_contains,
        operator: TokenTypes.NOT_CONTAINS,
      };
    }
    this.stream.reset();

    // "not includes"
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('has')) {
      this.stream.commit();
      return {
        condition: AbilityCondition.not_contains,
        operator: TokenTypes.NOT_CONTAINS,
      };
    }
    this.stream.reset();

    // "is equals"
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.equals, operator: TokenTypes.EQ };
    }
    this.stream.reset();

    // not equal
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.not_equals, operator: TokenTypes.NOT_EQ };
    }
    this.stream.reset();

    // is not equals
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('not') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.not_equals, operator: TokenTypes.NOT_EQ };
    }
    this.stream.reset();

    // is in
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('in')) {
      this.stream.commit();
      return { condition: AbilityCondition.in, operator: TokenTypes.IN };
    }
    this.stream.reset();

    // not in
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('in')) {
      this.stream.commit();
      return { condition: AbilityCondition.not_in, operator: TokenTypes.NOT_IN };
    }
    this.stream.reset();

    // is not null
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('not')) {
      if (this.stream.check(TokenTypes.NULL)) {
        this.stream.next();
        this.stream.commit();
        return {
          condition: AbilityCondition.not_equals,
          operator: TokenTypes.NOT_EQ_NULL,
        };
      }
    }
    this.stream.reset();

    // is null
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('null')) {
      if (this.stream.check(TokenTypes.NULL)) {
        this.stream.commit();
        this.stream.next();
        return {
          condition: AbilityCondition.equals,
          operator: TokenTypes.EQ_NULL,
        };
      }
    }
    this.stream.reset();

    // Single token (symbol or keyword)

    const token = this.stream.peek();

    if (
      token.type !== TokenTypes.SYMBOL &&
      token.type !== TokenTypes.KEYWORD &&
      token.type !== TokenTypes.NULL
    ) {
      this.stream.syntaxError(`Expected comparison operator, got \`${token.value}\``, token, [
        TokenTypes.SYMBOL,
        TokenTypes.KEYWORD,
        TokenTypes.NULL,
      ]);
    }

    this.stream.next();

    switch (token.type) {
      case TokenTypes.SYMBOL:
        if (token.value === '=' || token.value === '==')
          return { condition: AbilityCondition.equals, operator: TokenTypes.EQ };
        if (token.value === '!=' || token.value === '<>')
          return { condition: AbilityCondition.not_equals, operator: TokenTypes.NOT_EQ };
        if (token.value === '>')
          return { condition: AbilityCondition.greater_than, operator: TokenTypes.GT };
        if (token.value === '<')
          return { condition: AbilityCondition.less_than, operator: TokenTypes.LT };
        if (token.value === '>=')
          return { condition: AbilityCondition.greater_or_equal, operator: TokenTypes.GTE };
        if (token.value === '<=')
          return { condition: AbilityCondition.less_or_equal, operator: TokenTypes.LTE };
        break;

      case TokenTypes.KEYWORD:
        if (token.value === 'contains' || token.value === 'includes' || token.value === 'has')
          return { condition: AbilityCondition.contains, operator: TokenTypes.CONTAINS };
        if (token.value === 'in')
          return { condition: AbilityCondition.in, operator: TokenTypes.IN };
        if (token.value === 'equals')
          return { condition: AbilityCondition.equals, operator: TokenTypes.EQ };
        if (token.value === 'gte') {
          return { condition: AbilityCondition.greater_or_equal, operator: TokenTypes.GTE };
        }
        if (token.value === 'greater' || token.value === 'gt') {
          // If we come here, it means "greater" without "than" – treat as '>'
          return { condition: AbilityCondition.greater_than, operator: TokenTypes.GT };
        }
        if (token.value === 'less' || token.value === 'lt') {
          return { condition: AbilityCondition.less_than, operator: TokenTypes.LT };
        }
        if (token.value === 'lte') {
          return { condition: AbilityCondition.less_or_equal, operator: TokenTypes.LTE };
        }
        if (token.value === 'is') {
          // "is" alone -> equals
          return { condition: AbilityCondition.equals, operator: TokenTypes.EQ };
        }
        break;

      default:
        break;
    }
    return this.stream.syntaxError(`Unexpected operator token \`${token.value}\``, token, [
      TokenTypes.SYMBOL,
      TokenTypes.KEYWORD,
    ]);
  }

  /**
   * Helper to match and consume a specific word token (KEYWORD or IDENTIFIER).
   * @param word The exact string to look for.
   * @returns True if the next token has that value.
   */
  private matchWord(word: string): boolean {
    if (this.stream.eof()) {
      return false;
    }

    const token = this.stream.peek();
    if (
      (token.type === TokenTypes.KEYWORD ||
        token.type === TokenTypes.IDENTIFIER ||
        token.type === TokenTypes.ALWAYS ||
        token.type === TokenTypes.NEVER) &&
      token.value === word
    ) {
      this.stream.next();
      return true;
    }
    return false;
  }

  private matchSymbol(symbol: string): boolean {
    if (this.stream.eof()) return false;
    const token = this.stream.peek();
    if (token.type === TokenTypes.SYMBOL && token.value === symbol) {
      this.stream.next();
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
    if (this.stream.check(TokenTypes.LBRACKET)) {
      this.stream.next();
      return this.parseArray();
    }

    // Ensure we are not about to read a structural token as a value.
    const token = this.stream.peek();
    if (
      token.type === TokenTypes.ALL ||
      token.type === TokenTypes.ANY ||
      token.type === TokenTypes.EFFECT
    ) {
      this.stream.syntaxError(`Unexpected ${token.type} in value position`, token);
    }

    this.stream.next();

    // CHECK THIS SWITCH COMPARE
    switch (token.type) {
      case TokenTypes.STRING:
        return token.value;
      case TokenTypes.NUMBER:
        return Number(token.value);
      case TokenTypes.BOOLEAN:
        return token.value === 'true';
      case TokenTypes.NULL:
        return null;
      case TokenTypes.IDENTIFIER:
        return token.value;
      default: {
        this.stream.syntaxError(`Unexpected value token "${token.value}"`, token, [
          TokenTypes.KEYWORD,
        ]);
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
    if (this.stream.check(TokenTypes.RBRACKET)) {
      this.stream.next();
      return arr;
    }

    while (!this.stream.eof() && !this.stream.check(TokenTypes.RBRACKET)) {
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
        this.stream.syntaxError('Unexpected null in array', this.stream.peek());
      }

      // Optional comma between elements
      if (this.stream.check(TokenTypes.COMMA)) {
        this.stream.next();
      }
    }

    this.stream.expect(TokenTypes.RBRACKET, 'Expected "]"');
    return arr;
  }

  // -------------------------------------------------------------------------
  // #region comments
  // -------------------------------------------------------------------------
  private consumeLeadingComments() {
    while (this.stream.check(TokenTypes.COMMENT)) {
      this.stream.next();
      // this.processCommentToken(token);
    }
  }

  // private _consumeLeadingAnnotations() {
  //   while (this.stream.check(TokenTypes.ANNOTATION)) {
  //     const token = this.stream.next();
  //     this.processAnnotationToken(token);
  //   }
  // }

  private consumeLeadingAliases() {
    while (this.stream.check(TokenTypes.ALIAS)) {
      this.stream.next(); // consume "alias"

      const nameToken = this.stream.expect(TokenTypes.IDENTIFIER, `Expected alias name`);
      const aliasKey = nameToken.value;

      this.stream.expect(TokenTypes.COLON, `Expected colon after an alias`);


      const annotations = this.takeAnnotations('alias');

      while (!this.stream.eof() && !this.isStartOfAlias() && !this.isStartOfPolicy()) {
        const rule = this.parseRule();
        rule.name = annotations.get('name')?.value || aliasKey;
        rule.description = annotations.get('description')?.value;
        if (annotations.get('disabled')?.value === true) {
          rule.disabled = true;
        }

        this.aliasBuffer.set(aliasKey, rule);
      }

    }
  }

  private consumeLeadingAnnotations() {
    while (this.stream.check(TokenTypes.ANNOTATION)) {
      const token = this.stream.next();
      const text = token.value.trim();

      if (text.startsWith('@id ')) {
        this.annBuffer.setID(text.slice(4).trim(), token);
      }

      if (text.startsWith('@name ')) {
        this.annBuffer.setName(text.slice(6).trim(), token);
      }

      if (text.startsWith('@description ')) {
        this.annBuffer.setDescription(text.slice(13).trim(), token);
      }

      if (text.startsWith('@priority ')) {
        this.annBuffer.setPriority(parseInt(text.slice(10).trim(), 10), token);
      }

      if (text.startsWith('@disabled')) {
        const value = text.slice(9).trim();

        this.annBuffer.setDisabled(
          value.length === 0 ? true : text.slice(9).trim() === 'true',
          token,
        );
      }

      if (text.startsWith('@tags ')) {
        const value = text
          .slice(6)
          .trim()
          .split(',')
          .map(tag => tag.trim());

        this.annBuffer.setTags(value, token);
      }
    }
  }

  private takeAnnotations(owner: 'policy' | 'ruleSet' | 'rule' | 'alias'): AbilityDSLAnnotations {
    const ann = this.annBuffer.clone();
    this.annBuffer.clear();

    const allowed = AnnotationAllowed[owner];
    for (const key of Object.keys(ann['store']) as AnnotationName[]) {
      const entry = ann.get(key);
      if (!entry) continue;

      if (!allowed.has(key)) {
        this.stream.syntaxError(
          `Annotation @${key} is not allowed on ${owner}. Allowed: ${[...allowed]
            .map(a => '@' + a)
            .join(', ')}`,
          entry.token ?? this.stream.peek(),
        );
      }
    }

    return ann;
  }
  // -------------------------------------------------------------------------
  // #region Helpers
  // -------------------------------------------------------------------------

  private isStartOfPolicy(): boolean {
    return this.stream.check(TokenTypes.EFFECT);
  }

  private isStartOfGroup(): boolean {
    return this.stream.check(TokenTypes.ALL) || this.stream.check(TokenTypes.ANY);
  }

  private isStartOfExcept(): boolean {
    return this.stream.check(TokenTypes.EXCEPT);
  }

  private isStartOfAlias(): boolean {
    return this.stream.check(TokenTypes.ALIAS);
  }
}
