import AbilityCompare from '../../core/AbilityCompare';
import AbilityCondition from '../../core/AbilityCondition';
import AbilityPolicy from '../../core/AbilityPolicy';
import AbilityPolicyEffect from '../../core/AbilityPolicyEffect';
import AbilityRule, { AbilityRuleConfig } from '../../core/AbilityRule';
import AbilityRuleSet from '../../core/AbilityRuleSet';
import { AbilityDSLLexer } from './AbilityDSLLexer';
import { AbilityDSLToken, TokenType } from './AbilityDSLToken';
import { EnvironmentObject, ResourceObject } from '../../core/AbilityTypeGenerator';
import { AbilityDSLTokenStream } from './AbilityDSLTokenStream';
import {
  AbilityDSLAnnotation,
  AbilityDSLAnnotationName,
  AbilityDSLAnnotationOwner,
  AbilityDSLAnnotations,
} from '~/parsers/dsl/AbilityDSLAnnotations';


type AnnotationBuffer = {
  id: string | null;
  name: string | null;
  description: string | null;
  priority: number | null;
  disabled: boolean | null;
  tags: readonly string[];
};

type AnnotationTokens = {
  id?: AbilityDSLToken;
  name?: AbilityDSLToken;
  description?: AbilityDSLToken;
  priority?: AbilityDSLToken;
  disabled?: AbilityDSLToken;
  tags?: AbilityDSLToken;
};

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
  private annotationBuffer: AnnotationBuffer = {
    id: null,
    name: null,
    description: null,
    priority: null,
    disabled: null,
    tags: [],
  };

  private annotationMatrix2 = {
    [AbilityDSLAnnotationOwner.policy.code]: new Set([
      AbilityDSLAnnotationName.id,
      AbilityDSLAnnotationName.name,
      AbilityDSLAnnotationName.disabled,
      AbilityDSLAnnotationName.description,
      AbilityDSLAnnotationName.priority,
      AbilityDSLAnnotationName.tags,
    ]),
    [AbilityDSLAnnotationOwner.ruleSet.code]: new Set([
      AbilityDSLAnnotationName.id,
      AbilityDSLAnnotationName.name,
      AbilityDSLAnnotationName.disabled,
      AbilityDSLAnnotationName.description,
    ]),
    [AbilityDSLAnnotationOwner.rule.code]: new Set([
      AbilityDSLAnnotationName.id,
      AbilityDSLAnnotationName.name,
      AbilityDSLAnnotationName.disabled,
      AbilityDSLAnnotationName.description,
    ]),
  } as const;
  private annBuffer : AbilityDSLAnnotations = new AbilityDSLAnnotations();
  private annotationTokens: AnnotationTokens = {};
  private annotationMatrix = {
    policy: new Set(['id', 'name', 'disabled', 'description', 'priority', 'tags']),
    ruleSet: new Set(['id', 'name', 'disabled', 'description']),
    rule: new Set(['id', 'name', 'disabled', 'description']),
  } as const;

  constructor(dsl: string) {
    this.dsl = dsl;
  }

  /**
   * Main entry point: tokenize the input and parse all policies.
   * @returns Array of AbilityPolicy instances.
   */
  public parse(): readonly AbilityPolicy<R, E, T>[] {
    // 1. Лексер → токены
    const tokens = new AbilityDSLLexer(this.dsl).tokenize();

    // 2. Создаём TokenStream
    this.stream = new AbilityDSLTokenStream(tokens, this.dsl);

    const policies: AbilityPolicy<R, E, T>[] = [];

    // 3. Парсим политики
    while (!this.stream.eof()) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      if (!this.isStartOfPolicy()) {
        const token = this.stream.peek();
        this.stream.syntaxError(`Expected policy, got ${token.code}.`, token, ['EFFECT']);
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
    const annotations = this.takeAnnotations(AbilityDSLAnnotationOwner.policy);
    // this.validateAnnotations('policy', meta, tokens);

    // Effect: "permit" or "deny"
    const effectToken = this.stream.expect(AbilityDSLToken.EFFECT, 'Expected effect');
    const effect = effectToken.value;

    // Permission: e.g. "order.update"
    const permissionToken = this.stream.expect(AbilityDSLToken.PERMISSION, 'Expected permission');
    const permission = permissionToken.value;
    if (!permission.startsWith('permission.')) {
      return this.stream.syntaxError(
        `Unexpected token. The permission key, must be starts with prefix \`permission.\`, but got \`${permission}\`.\nDid you mean \`permission.${permission}\`?`,
        permissionToken,
      );
    }

    // "if" keyword
    this.stream.expect(AbilityDSLToken.IF, 'Expected "if"');

    // Group selector: "all" or "any" – determines how the top‑level rule sets are combined.
    const compareToken = this.stream.expectOneOf(
      [AbilityDSLToken.ALL, AbilityDSLToken.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.code === AbilityDSLToken.ALL ? AbilityCompare.and : AbilityCompare.or;

    // Colon after the group keyword
    this.stream.expect(AbilityDSLToken.COLON, 'Expected ":"');

    // Parse the list of rule sets (each "all of:" or "any of:" block)
    const ruleSets = this.parseRuleSets(compareMethod);

    // Construct the policy instance.
    return new AbilityPolicy<R, E, T>({
      id: annotations.id?.value || null,
      name: annotations.name?.value || null,
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
  private parseRuleSets(policyCompareMethod: AbilityCompare): AbilityRuleSet[] {
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

      const annotation = this.takeAnnotations(AbilityDSLAnnotationOwner.ruleSet);
      // this.validateAnnotations('ruleSet', meta, tokens);
      // this.validateAnnotations('ruleSet', meta);

      const group = new AbilityRuleSet({
        id: annotation.id?.value || null,
        compareMethod: policyCompareMethod,
        name: annotation.name?.value ?? null,
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
          this.stream.check(AbilityDSLToken.IDENTIFIER) ||
          this.stream.check(AbilityDSLToken.ALWAYS) ||
          this.stream.check(AbilityDSLToken.NEVER)
        ) {
          group.addRule(this.parseRule());
        } else {
          this.stream.syntaxError(
            `Unexpected token in implicit group: ${this.stream.peek().code}`,
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
    const annotations = this.takeAnnotations(AbilityDSLAnnotationOwner.ruleSet);
    // this.validateAnnotations('ruleSet', meta, tokens);

    const compareToken = this.stream.expectOneOf(
      [AbilityDSLToken.ALL, AbilityDSLToken.ANY, AbilityDSLToken.ALWAYS, AbilityDSLToken.NEVER],
      'Expected "all" or "any" or "always" or "never"',
    );

    const compareMethod =
      compareToken.code === AbilityDSLToken.ALL ? AbilityCompare.and : AbilityCompare.or;

    if (this.stream.check(AbilityDSLToken.OF)) {
      this.stream.next();
    }

    this.stream.expect(AbilityDSLToken.COLON, 'Expected ":"');

    const group = new AbilityRuleSet({
      id: annotations.id?.value || null,
      compareMethod,
      name: annotations.name?.value || null,
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

      if (this.stream.check(AbilityDSLToken.IDENTIFIER)) {
        group.addRule(this.parseRule());
      } else {
        this.stream.syntaxError(
          `Unexpected token in group: ${this.stream.peek().code}`,
          this.stream.peek(),
        );
      }
    }

    return group;
  }

  // -------------------------------------------------------------------------
  // #region Except RuleSet parsing
  // -------------------------------------------------------------------------
  private parseExceptGroup(policyCompareMethod: AbilityCompare): AbilityRuleSet {
    this.consumeLeadingComments();
    this.consumeLeadingAnnotations();
    const annotations = this.takeAnnotations(AbilityDSLAnnotationOwner.ruleSet);
    // this.validateAnnotations('ruleSet', meta, tokens);

    // consume "except"
    this.stream.expect(AbilityDSLToken.EXCEPT, 'Expected "except"');

    let compareMethod = policyCompareMethod;

    // optional: "all" / "any"
    if (this.stream.check(AbilityDSLToken.ALL) || this.stream.check(AbilityDSLToken.ANY)) {
      const compareToken = this.stream.next();
      compareMethod =
        compareToken.code === AbilityDSLToken.ALL ? AbilityCompare.and : AbilityCompare.or;

      if (this.stream.check(AbilityDSLToken.OF)) {
        this.stream.next();
      }

      this.stream.expect(AbilityDSLToken.COLON, 'Expected ":" after except group');
    } else {
      // implicit except group — no "all/any of:"
      // but still must end with colon
      this.stream.expect(AbilityDSLToken.COLON, 'Expected ":" after "except"');
    }

    const group = new AbilityRuleSet({
      id: annotations.id?.value || null,
      compareMethod,
      name: annotations.name?.value || null,
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

      if (this.stream.check(AbilityDSLToken.IDENTIFIER)) {
        group.addRule(this.parseRule());
      } else {
        this.stream.syntaxError(
          `Unexpected token in except group: ${this.stream.peek().code}`,
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
    const annotations = this.takeAnnotations(AbilityDSLAnnotationOwner.rule);
    // this.validateAnnotations('rule', meta, tokens);

    const isNeverAlways =
      this.stream.check(AbilityDSLToken.ALWAYS) || this.stream.check(AbilityDSLToken.NEVER);

    if (!isNeverAlways && !this.stream.check(AbilityDSLToken.IDENTIFIER)) {
      this.stream.syntaxError(
        `Expected identifier, but got ${this.stream.peek().code}`,
        this.stream.peek(),
      );
    }

    // subject
    const subject = isNeverAlways
      ? ''
      : this.stream.expect(AbilityDSLToken.IDENTIFIER, 'Expected field').value;

    // operator
    const { condition, operator } = this.parseConditionOperator();

    // value
    let resource: AbilityRuleConfig['resource'] = null;
    let valueToken: AbilityDSLToken | null = null;

    const operatorConsumesValue =
      operator !== AbilityDSLToken.EQ_NULL &&
      operator !== AbilityDSLToken.NOT_EQ_NULL &&
      operator !== AbilityDSLToken.NULL &&
      operator !== AbilityDSLToken.ALWAYS &&
      operator !== AbilityDSLToken.NEVER;

    if (operatorConsumesValue) {
      this.stream.mark();
      resource = this.parseValue();
      valueToken = this.stream.lookPrev();
      this.stream.commit();
    }

    this.consumeLeadingComments();
    this.consumeLeadingAnnotations();

    // validation: identifier without dot → error
    if (
      typeof resource === 'string' &&
      valueToken &&
      valueToken.code === AbilityDSLToken.IDENTIFIER &&
      !valueToken.value.includes('.')
    ) {
      this.stream.syntaxError(
        `Expected comparison operator or value, got \`${resource}\``,
        valueToken,
        [AbilityDSLToken.KEYWORD],
      );
    }

    return new AbilityRule({
      id: annotations.id?.value || null,
      subject,
      resource,
      condition,
      name: annotations.name?.value || null,
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
  private parseConditionOperator(): { condition: AbilityCondition; operator: TokenType } {
    // "always"
    this.stream.mark();
    if (this.matchWord('always')) {
      this.stream.commit();
      return { condition: AbilityCondition.always, operator: AbilityDSLToken.ALWAYS };
    }
    this.stream.reset();

    // "never"
    this.stream.mark();
    if (this.matchWord('never')) {
      this.stream.commit();
      return { condition: AbilityCondition.never, operator: AbilityDSLToken.NEVER };
    }
    this.stream.reset();

    // "length equals"
    this.stream.mark();
    if (this.matchWord('length') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_equals, operator: AbilityDSLToken.LEN_EQ };
    }
    this.stream.reset();

    // "length ="
    this.stream.mark();
    if (this.matchWord('length') && this.matchSymbol('=')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_equals, operator: AbilityDSLToken.LEN_EQ };
    }
    this.stream.reset();

    // "length greater than"
    this.stream.mark();
    if (this.matchWord('length') && this.matchWord('greater') && this.matchWord('than')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_greater_than, operator: AbilityDSLToken.LEN_GT };
    }
    this.stream.reset();

    // "length >"
    this.stream.mark();
    if (this.matchWord('length') && this.matchSymbol('>')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_greater_than, operator: AbilityDSLToken.LEN_GT };
    }
    this.stream.reset();

    // "length less than"
    this.stream.mark();
    if (this.matchWord('length') && this.matchWord('less') && this.matchWord('than')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_less_than, operator: AbilityDSLToken.LEN_LT };
    }
    this.stream.reset();

    // "length <"
    this.stream.mark();
    if (this.matchWord('length') && this.matchSymbol('<')) {
      this.stream.commit();
      return { condition: AbilityCondition.length_less_than, operator: AbilityDSLToken.LEN_LT };
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
      return { condition: AbilityCondition.greater_or_equal, operator: AbilityDSLToken.GTE };
    }
    this.stream.reset();

    // greater than
    this.stream.mark();
    if (this.matchWord('greater') && this.matchWord('than')) {
      this.stream.commit();
      return { condition: AbilityCondition.greater_than, operator: AbilityDSLToken.GT };
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
      return { condition: AbilityCondition.less_or_equal, operator: AbilityDSLToken.LTE };
    }
    this.stream.reset();

    // less than
    if (this.matchWord('less') && this.matchWord('than')) {
      return { condition: AbilityCondition.less_than, operator: AbilityDSLToken.LT };
    }
    this.stream.reset();

    // "not contains"
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('contains')) {
      this.stream.commit();
      return {
        condition: AbilityCondition.not_contains,
        operator: AbilityDSLToken.NOT_CONTAINS,
      };
    }
    this.stream.reset();

    // "not includes"
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('includes')) {
      this.stream.commit();
      return {
        condition: AbilityCondition.not_contains,
        operator: AbilityDSLToken.NOT_CONTAINS,
      };
    }
    this.stream.reset();

    // "not includes"
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('has')) {
      this.stream.commit();
      return {
        condition: AbilityCondition.not_contains,
        operator: AbilityDSLToken.NOT_CONTAINS,
      };
    }
    this.stream.reset();

    // "is equals"
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
    }
    this.stream.reset();

    // not equal
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.not_equals, operator: AbilityDSLToken.NOT_EQ };
    }
    this.stream.reset();

    // is not equals
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('not') && this.matchWord('equals')) {
      this.stream.commit();
      return { condition: AbilityCondition.not_equals, operator: AbilityDSLToken.NOT_EQ };
    }
    this.stream.reset();

    // is in
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('in')) {
      this.stream.commit();
      return { condition: AbilityCondition.in, operator: AbilityDSLToken.IN };
    }
    this.stream.reset();

    // not in
    this.stream.mark();
    if (this.matchWord('not') && this.matchWord('in')) {
      this.stream.commit();
      return { condition: AbilityCondition.not_in, operator: AbilityDSLToken.NOT_IN };
    }
    this.stream.reset();

    // is not null
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('not')) {
      if (this.stream.check(AbilityDSLToken.NULL)) {
        this.stream.next();
        this.stream.commit();
        return {
          condition: AbilityCondition.not_equals,
          operator: AbilityDSLToken.NOT_EQ_NULL,
        };
      }
    }
    this.stream.reset();

    // is null
    this.stream.mark();
    if (this.matchWord('is') && this.matchWord('null')) {
      if (this.stream.check(AbilityDSLToken.NULL)) {
        this.stream.commit();
        this.stream.next();
        return {
          condition: AbilityCondition.equals,
          operator: AbilityDSLToken.EQ_NULL,
        };
      }
    }
    this.stream.reset();

    // Single token (symbol or keyword)

    const token = this.stream.peek();

    if (
      token.code !== AbilityDSLToken.SYMBOL &&
      token.code !== AbilityDSLToken.KEYWORD &&
      token.code !== AbilityDSLToken.NULL
    ) {
      this.stream.syntaxError(`Expected comparison operator, got \`${token.value}\``, token, [
        AbilityDSLToken.SYMBOL,
        AbilityDSLToken.KEYWORD,
        AbilityDSLToken.NULL,
      ]);
    }

    this.stream.next();

    switch (token.code) {
      case AbilityDSLToken.SYMBOL:
        if (token.value === '=' || token.value === '==')
          return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
        if (token.value === '!=' || token.value === '<>')
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
        if (token.value === 'contains' || token.value === 'includes' || token.value === 'has')
          return { condition: AbilityCondition.contains, operator: AbilityDSLToken.CONTAINS };
        if (token.value === 'in')
          return { condition: AbilityCondition.in, operator: AbilityDSLToken.IN };
        if (token.value === 'equals')
          return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
        if (token.value === 'gte') {
          return { condition: AbilityCondition.greater_or_equal, operator: AbilityDSLToken.GTE };
        }
        if (token.value === 'greater' || token.value === 'gt') {
          // If we come here, it means "greater" without "than" – treat as '>'
          return { condition: AbilityCondition.greater_than, operator: AbilityDSLToken.GT };
        }
        if (token.value === 'less' || token.value === 'lt') {
          return { condition: AbilityCondition.less_than, operator: AbilityDSLToken.LT };
        }
        if (token.value === 'lte') {
          return { condition: AbilityCondition.less_or_equal, operator: AbilityDSLToken.LTE };
        }
        if (token.value === 'is') {
          // "is" alone -> equals
          return { condition: AbilityCondition.equals, operator: AbilityDSLToken.EQ };
        }
        break;

      default:
        break;
    }
    return this.stream.syntaxError(`Unexpected operator token \`${token.value}\``, token, [
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
    if (this.stream.eof()) {
      return false;
    }

    const token = this.stream.peek();
    if (
      (token.code === AbilityDSLToken.KEYWORD ||
        token.code === AbilityDSLToken.IDENTIFIER ||
        token.code === AbilityDSLToken.ALWAYS ||
        token.code === AbilityDSLToken.NEVER) &&
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
    if (token.code === AbilityDSLToken.SYMBOL && token.value === symbol) {
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
    if (this.stream.check(AbilityDSLToken.LBRACKET)) {
      this.stream.next();
      return this.parseArray();
    }

    // Ensure we are not about to read a structural token as a value.
    const token = this.stream.peek();
    if (
      token.code === AbilityDSLToken.ALL ||
      token.code === AbilityDSLToken.ANY ||
      token.code === AbilityDSLToken.EFFECT
    ) {
      this.stream.syntaxError(`Unexpected ${token.code} in value position`, token);
    }

    this.stream.next();

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
      default: {
        this.stream.syntaxError(`Unexpected value token "${token.value}"`, token, [
          AbilityDSLToken.KEYWORD,
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
    if (this.stream.check(AbilityDSLToken.RBRACKET)) {
      this.stream.next();
      return arr;
    }

    while (!this.stream.eof() && !this.stream.check(AbilityDSLToken.RBRACKET)) {
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
      if (this.stream.check(AbilityDSLToken.COMMA)) {
        this.stream.next();
      }
    }

    this.stream.expect(AbilityDSLToken.RBRACKET, 'Expected "]"');
    return arr;
  }

  // -------------------------------------------------------------------------
  // #region comments
  // -------------------------------------------------------------------------
  private consumeLeadingComments() {
    while (this.stream.check(AbilityDSLToken.COMMENT)) {
      this.stream.next();
      // this.processCommentToken(token);
    }
  }

  // private _consumeLeadingAnnotations() {
  //   while (this.stream.check(AbilityDSLToken.ANNOTATION)) {
  //     const token = this.stream.next();
  //     this.processAnnotationToken(token);
  //   }
  // }

  private consumeLeadingAnnotations() {
    // const annotationNames = Object.keys(this.annotationBuffer);
    // console.log(`annotationNames: ${annotationNames}`);

    while (this.stream.check(AbilityDSLToken.ANNOTATION)) {
      const token = this.stream.next();
      const text = token.value.trim();

      if (text.startsWith('@id ')) {
        this.annotationBuffer.id = text.slice(4).trim();
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

  // private processAnnotationToken(token: AbilityDSLToken) {
  //   const text = token.value.trim();
  //
  //   if (text.startsWith('@id ')) {
  //     this.annotationBuffer.id = text.slice(4).trim();
  //   }
  //
  //   if (text.startsWith('@name ')) {
  //     this.annotationBuffer.name = text.slice(6).trim();
  //   }
  //
  //   if (text.startsWith('@description ')) {
  //     this.annotationBuffer.description = text.slice(13).trim();
  //   }
  //
  //   if (text.startsWith('@priority ')) {
  //     this.annotationBuffer.priority = parseInt(text.slice(10).trim(), 10);
  //   }
  //
  //   if (text.startsWith('@disabled')) {
  //     const value = text.slice(9).trim();
  //     this.annotationBuffer.disabled = value.length === 0 ? true : text.slice(9).trim() === 'true';
  //   }
  //
  //   if (text.startsWith('@tags ')) {
  //     const value = text
  //       .slice(6)
  //       .trim()
  //       .split(',')
  //       .map(tag => tag.trim());
  //     this.annotationBuffer.tags = this.annotationBuffer.tags.concat(value);
  //   }
  // }

  private takeAnnotations(owner: AbilityDSLAnnotationOwner): AbilityDSLAnnotations {
    // const meta = { ...this.annotationBuffer };
    // const tokens = { ...this.annotationTokens };
    const annBuffer = this.annBuffer.clone();

    const allowed = this.annotationMatrix2[owner.code];
    //
    // const checkField = (field: AbilityDSLAnnotationName) => {
    //   if (!allowed.has(field)) {
    //     const msg = `Annotation @${field} is not allowed on ${owner.code}. Allowed: ${Array.from(allowed)
    //       .map(a => `@${a}`)
    //       .join(', ')}`;
    //     this.stream.syntaxError(msg, allo);
    //   }
    // };
    // checkField(AbilityDSLAnnotationName.id, tokens.id);
    // checkField('name', tokens.name);
    // checkField('description', tokens.description);
    // checkField('priority', tokens.priority);
    // checkField('disabled', tokens.disabled);
    // if (tokens.tags && meta.tags.length > 0) {
    //   checkField('tags', tokens.tags);
    // }

    this.annBuffer.clear();

    this.annotationBuffer = {
      id: null,
      name: null,
      description: null,
      priority: null,
      disabled: null,
      tags: [],
    };
    this.annotationTokens = {};
    return annBuffer;
  }

  // Метод validateAnnotations
  private validateAnnotations(
    type: keyof typeof this.annotationMatrix,
    meta: typeof this.annotationBuffer,
    tokens: typeof this.annotationTokens,
  ): void {
    const allowed = this.annotationMatrix[type];
    const checkField = (field: keyof typeof meta, token?: AbilityDSLToken) => {
      if (token && !allowed.has(field)) {
        const msg = `Annotation @${field} is not allowed on ${type}. Allowed: ${Array.from(allowed)
          .map(a => `@${a}`)
          .join(', ')}`;
        this.stream.syntaxError(msg, token);
      }
    };
    checkField('id', tokens.id);
    checkField('name', tokens.name);
    checkField('description', tokens.description);
    checkField('priority', tokens.priority);
    checkField('disabled', tokens.disabled);
    if (tokens.tags && meta.tags.length > 0) {
      checkField('tags', tokens.tags);
    }
  }

  // -------------------------------------------------------------------------
  // #region Helpers
  // -------------------------------------------------------------------------

  private isStartOfPolicy(): boolean {
    return this.stream.check(AbilityDSLToken.EFFECT);
  }

  private isStartOfGroup(): boolean {
    return this.stream.check(AbilityDSLToken.ALL) || this.stream.check(AbilityDSLToken.ANY);
  }

  private isStartOfExcept(): boolean {
    return this.stream.check(AbilityDSLToken.EXCEPT);
  }
}
