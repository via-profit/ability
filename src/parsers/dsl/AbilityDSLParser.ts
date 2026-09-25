import { AbilityCompare, AbilityCompareType } from '../../core/AbilityCompare';
import { AbilityCondition, AbilityConditionType } from '../../core/AbilityCondition';
import AbilityPolicy from '../../core/AbilityPolicy';
import { AbilityPolicyEffect } from '../../core/AbilityPolicyEffect';
import AbilityRule, { AbilityRuleConfig, AbilityRuleResourceType } from '../../core/AbilityRule';
import AbilityRuleSet from '../../core/AbilityRuleSet';
import { AbilityDSLLexer } from './AbilityDSLLexer';
import { AbilityDSLToken, TokenType, TokenTypes } from './AbilityDSLToken';
import { EnvironmentObject, ResourceObject } from '../../core/AbilityTypeGenerator';
import { AbilityDSLTokenStream } from './AbilityDSLTokenStream';
import { AbilityDSLAnnotations, AnnotationName } from '~/parsers/dsl/AbilityDSLAnnotations';
import { AnnotationAllowed } from './AbilityDSLAnnotationMatrix';
import { AbilityDSLAliases } from '~/parsers/dsl/AbilityDSLAliases';

type ArrayValue = (string | number | boolean | null)[];

type ParsedValue = {
  readonly resource: AbilityRuleConfig['resource'];
  readonly resourceType: AbilityRuleResourceType;
  readonly token: AbilityDSLToken;
};

type ParsedOperator = {
  readonly condition: AbilityConditionType;
  readonly operator: TokenType;
  readonly token: AbilityDSLToken;
};

/**
 * Part of the multi-word operator: word (keyword or identifier), symbol or token type
 */
type OperatorPart = string | { readonly symbol: string } | { readonly type: TokenType };

type OperatorDefinition = {
  readonly parts: readonly OperatorPart[];
  readonly condition: AbilityConditionType;
  readonly operator: TokenType;
  /**
   * Additional check, which is performed after all the parts have been matched
   */
  readonly guard?: () => boolean;
};

const KNOWN_ANNOTATIONS: readonly AnnotationName[] = [
  'id',
  'name',
  'description',
  'priority',
  'disabled',
  'tags',
];

/**
 * Operators, which do not require a value
 */
const VALUELESS_OPERATORS: readonly TokenType[] = [
  TokenTypes.EQ_NULL,
  TokenTypes.NOT_EQ_NULL,
  TokenTypes.ALWAYS,
  TokenTypes.NEVER,
  TokenTypes.DEFINED,
  TokenTypes.EMPTY,
  TokenTypes.NOT_EMPTY,
];

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
 * Each rule is: <path> <operator> <value>
 *
 * The value is a literal (quoted string, number, boolean, null, array)
 * or a path (unquoted dot-notation identifier, e.g. `user.id` or `env.ip`).
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
  private readonly ids = {
    policy: new Set<string>(),
    ruleSet: new Set<string>(),
    rule: new Set<string>(),
  };

  constructor(dsl: string) {
    this.dsl = dsl;
  }

  /**
   * Main entry point: tokenize the input and parse all policies.
   * @returns Array of AbilityPolicy instances.
   */
  public parse(): readonly AbilityPolicy<R, E, T>[] {
    this.annBuffer.clear();
    this.aliasBuffer.clear();
    this.ids.policy.clear();
    this.ids.ruleSet.clear();
    this.ids.rule.clear();

    const tokens = new AbilityDSLLexer(this.dsl).tokenize();
    this.stream = new AbilityDSLTokenStream(tokens, this.dsl);

    const policies: AbilityPolicy<R, E, T>[] = [];

    while (true) {
      this.consumeLeading();

      if (this.stream.eof()) {
        this.assertNoPendingAnnotations();
        break;
      }

      if (!this.isStartOfPolicy()) {
        const token = this.stream.peek();
        this.stream.syntaxError(`Expected policy, got ${token.type}.`, token, [TokenTypes.EFFECT]);
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
   *   policy = EFFECT PERMISSION IF (ALL | ANY) COLON ruleSets
   */
  private parsePolicy(): AbilityPolicy<R, E, T> {
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

    if (!ruleSets.length) {
      this.stream.syntaxError(
        `Policy \`${permission}\` has no rules. Add at least one rule (use \`always\` or \`never\` explicitly, if needed)`,
        permissionToken,
      );
    }

    if (!ruleSets.some(ruleSet => !ruleSet.isExcept)) {
      this.stream.syntaxError(
        `Policy \`${permission}\` must contain at least one rule outside the \`except\` block`,
        permissionToken,
      );
    }

    this.registerID('policy', annotations);

    return new AbilityPolicy<R, E, T>({
      id: annotations.id?.value || null,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      priority: annotations.priority?.value ?? null,
      permission: permission.replace(/^permission\./, ''),
      effect: effect === 'permit' ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny,
      disabled: annotations.disabled?.value ?? undefined,
      tags: (annotations.tags?.value as readonly T[] | undefined) ?? undefined,
      compareMethod,
    }).addRuleSets(ruleSets as unknown as AbilityRuleSet<R, E>[]);
  }

  // -------------------------------------------------------------------------
  // #region Rule set parsing (groups of rules)
  // -------------------------------------------------------------------------

  /**
   * Parses a sequence of rule sets (groups) until a new policy (alias) starts or EOF.
   */
  private parseRuleSets(policyCompareMethod: AbilityCompareType): AbilityRuleSet[] {
    const sets: AbilityRuleSet[] = [];

    while (true) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      if (this.stream.eof() || this.isStartOfPolicy() || this.isStartOfAlias()) {
        break;
      }

      if (this.isStartOfExcept()) {
        sets.push(this.parseExceptGroup(policyCompareMethod));
        continue;
      }

      if (this.isStartOfGroup()) {
        sets.push(this.parseGroup());
        continue;
      }

      if (!this.isStartOfRule()) {
        this.stream.syntaxError(
          `Unexpected token in policy: ${this.stream.peek().type}`,
          this.stream.peek(),
        );
      }

      // implicit group. Annotations before the first rule belong to this rule
      const group = new AbilityRuleSet({
        compareMethod: policyCompareMethod,
      });

      this.parseGroupRules(group, 'implicit group');
      sets.push(group);
    }

    return sets;
  }

  /**
   * Parses a single group, e.g. "all of:" or "any of:", and returns a RuleSet.
   */
  private parseGroup(): AbilityRuleSet {
    const annotations = this.takeAnnotations('ruleSet');

    const compareToken = this.stream.expectOneOf(
      [TokenTypes.ALL, TokenTypes.ANY],
      'Expected "all" or "any"',
    );

    const compareMethod =
      compareToken.type === TokenTypes.ALL ? AbilityCompare.and : AbilityCompare.or;

    if (this.stream.check(TokenTypes.OF)) {
      this.stream.next();
    }

    this.stream.expect(TokenTypes.COLON, 'Expected ":"');

    this.registerID('ruleSet', annotations);

    const group = new AbilityRuleSet({
      id: annotations.id?.value || null,
      compareMethod,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      disabled: annotations.disabled?.value ?? undefined,
    });

    this.parseGroupRules(group, 'group');

    if (!group.rules.length) {
      this.stream.syntaxError('The group has no rules', compareToken);
    }

    return group;
  }

  // -------------------------------------------------------------------------
  // #region Except RuleSet parsing
  // -------------------------------------------------------------------------
  private parseExceptGroup(policyCompareMethod: AbilityCompareType): AbilityRuleSet {
    const annotations = this.takeAnnotations('ruleSet');

    // consume "except"
    const exceptToken = this.stream.expect(TokenTypes.EXCEPT, 'Expected "except"');

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

    this.registerID('ruleSet', annotations);

    const group = new AbilityRuleSet({
      id: annotations.id?.value || null,
      compareMethod,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      isExcept: true,
      disabled: annotations.disabled?.value ?? undefined,
    });

    this.parseGroupRules(group, 'except group');

    if (!group.rules.length) {
      this.stream.syntaxError('The except group has no rules', exceptToken);
    }

    return group;
  }

  /**
   * Reads the rules of the group until the next group, except block, policy, alias or EOF
   */
  private parseGroupRules(group: AbilityRuleSet, owner: string): void {
    while (true) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      if (
        this.stream.eof() ||
        this.isStartOfGroup() ||
        this.isStartOfPolicy() ||
        this.isStartOfExcept() ||
        this.isStartOfAlias()
      ) {
        break;
      }

      if (!this.isStartOfRule()) {
        this.stream.syntaxError(
          `Unexpected token in ${owner}: ${this.stream.peek().type}`,
          this.stream.peek(),
        );
      }

      group.addRule(this.parseRule());
    }
  }

  // -------------------------------------------------------------------------
  // #region Rule parsing
  // -------------------------------------------------------------------------

  /**
   * Parses a single rule: subject operator value
   */
  private parseRule(): AbilityRule {
    const annotations = this.takeAnnotations('rule');

    // `always` / `never` without subject
    if (this.stream.check(TokenTypes.ALWAYS) || this.stream.check(TokenTypes.NEVER)) {
      const token = this.stream.next();
      this.registerID('rule', annotations);

      return new AbilityRule({
        id: annotations.id?.value || null,
        name: annotations.name?.value || null,
        description: annotations.description?.value || null,
        disabled: annotations.disabled?.value ?? undefined,
        subject: '',
        resource: null,
        resourceType: 'value',
        condition:
          token.type === TokenTypes.ALWAYS ? AbilityCondition.always : AbilityCondition.never,
      });
    }

    // subject
    const subjectToken = this.stream.expect(TokenTypes.IDENTIFIER, 'Expected field');
    const subject = subjectToken.value;

    // alias
    const alias = this.aliasBuffer.get(subject);
    if (alias) {
      this.registerID('rule', annotations);

      // Each usage of the alias gets its own rule instance
      // so the evaluation state is not shared between policies
      return alias.copyWith({
        id: annotations.id?.value,
        name: annotations.name?.value,
        description: annotations.description?.value,
        disabled: annotations.disabled?.value,
      });
    }

    // single word on the line — it looks like an unknown alias
    if (!subject.includes('.')) {
      const next = this.stream.peek();
      if (next.type === TokenTypes.EOF || next.line !== subjectToken.line) {
        this.stream.syntaxError(
          `Unknown alias \`${subject}\`. Aliases must be defined before they are used`,
          subjectToken,
        );
      }
    }

    // operator
    const operator = this.parseConditionOperator();

    // value
    let value: ParsedValue | null = null;

    if (!VALUELESS_OPERATORS.includes(operator.operator)) {
      value = this.parseValue();
      this.validateValue(operator, value);
    }

    this.registerID('rule', annotations);

    return new AbilityRule({
      id: annotations.id?.value || null,
      subject,
      resource: value ? value.resource : null,
      resourceType: value ? value.resourceType : 'value',
      condition: operator.condition,
      name: annotations.name?.value || null,
      description: annotations.description?.value || null,
      disabled: annotations.disabled?.value ?? undefined,
    });
  }

  /**
   * Checks that the value is compatible with the operator
   */
  private validateValue(operator: ParsedOperator, value: ParsedValue): void {
    if (value.resourceType === 'path') {
      return;
    }

    const { resource } = value;
    const op = `\`${operator.token.value}\``;

    switch (operator.condition) {
      case AbilityCondition.greater_than:
      case AbilityCondition.greater_or_equal:
      case AbilityCondition.less_than:
      case AbilityCondition.less_or_equal:
      case AbilityCondition.length_equals:
      case AbilityCondition.length_greater_than:
      case AbilityCondition.length_less_than:
        if (typeof resource !== 'number') {
          this.stream.syntaxError(
            `Operator ${op} expects a number or a path, got \`${value.token.value}\``,
            value.token,
          );
        }
        break;

      case AbilityCondition.in:
      case AbilityCondition.not_in:
        if (!Array.isArray(resource)) {
          this.stream.syntaxError(
            `Operator ${op} expects an array or a path, got \`${value.token.value}\``,
            value.token,
          );
        }
        break;

      case AbilityCondition.starts_with:
      case AbilityCondition.ends_with:
        if (typeof resource !== 'string') {
          this.stream.syntaxError(
            `Operator ${op} expects a quoted string or a path, got \`${value.token.value}\``,
            value.token,
          );
        }
        break;

      case AbilityCondition.contains_all:
      case AbilityCondition.contains_any:
        if (Array.isArray(resource) && resource.length === 0) {
          this.stream.syntaxError(`Operator ${op} expects a non-empty array`, value.token);
        }
        break;

      case AbilityCondition.equals:
      case AbilityCondition.not_equals:
        if (Array.isArray(resource)) {
          this.stream.syntaxError(
            `Operator ${op} can not be used with an array. Use \`in\` / \`not in\` instead`,
            value.token,
          );
        }
        break;

      default:
        break;
    }
  }

  // -------------------------------------------------------------------------
  // #region Operator parsing
  // -------------------------------------------------------------------------

  /**
   * Multi-word operators. The order matters: longer sequences go first
   */
  private readonly operatorDefinitions: readonly OperatorDefinition[] = [
    { parts: ['always'], condition: AbilityCondition.always, operator: TokenTypes.ALWAYS },
    { parts: ['never'], condition: AbilityCondition.never, operator: TokenTypes.NEVER },

    ...(['length', 'len'] as const).flatMap(len => [
      {
        parts: [len, 'equals'],
        condition: AbilityCondition.length_equals,
        operator: TokenTypes.LEN_EQ,
      },
      {
        parts: [len, { symbol: '=' }],
        condition: AbilityCondition.length_equals,
        operator: TokenTypes.LEN_EQ,
      },
      {
        parts: [len, 'greater', 'than'],
        condition: AbilityCondition.length_greater_than,
        operator: TokenTypes.LEN_GT,
      },
      {
        parts: [len, { symbol: '>' }],
        condition: AbilityCondition.length_greater_than,
        operator: TokenTypes.LEN_GT,
      },
      {
        parts: [len, 'less', 'than'],
        condition: AbilityCondition.length_less_than,
        operator: TokenTypes.LEN_LT,
      },
      {
        parts: [len, { symbol: '<' }],
        condition: AbilityCondition.length_less_than,
        operator: TokenTypes.LEN_LT,
      },
    ]),

    {
      parts: ['greater', 'than', 'or', 'equal'],
      condition: AbilityCondition.greater_or_equal,
      operator: TokenTypes.GTE,
    },
    {
      parts: ['greater', 'than'],
      condition: AbilityCondition.greater_than,
      operator: TokenTypes.GT,
    },
    {
      parts: ['less', 'than', 'or', 'equal'],
      condition: AbilityCondition.less_or_equal,
      operator: TokenTypes.LTE,
    },
    { parts: ['less', 'than'], condition: AbilityCondition.less_than, operator: TokenTypes.LT },

    ...(['contains', 'includes', 'has'] as const).map(word => ({
      parts: ['not', word],
      condition: AbilityCondition.not_contains,
      operator: TokenTypes.NOT_CONTAINS,
    })),

    // `contains all` / `contains any` must not be confused with the `all of:` group on the next line
    {
      parts: ['contains', 'all'],
      condition: AbilityCondition.contains_all,
      operator: TokenTypes.CONTAINS_ALL,
      guard: () => this.isNotGroupContinuation(),
    },
    {
      parts: ['contains', 'any'],
      condition: AbilityCondition.contains_any,
      operator: TokenTypes.CONTAINS_ANY,
      guard: () => this.isNotGroupContinuation(),
    },

    {
      parts: ['starts', 'with'],
      condition: AbilityCondition.starts_with,
      operator: TokenTypes.STARTS_WITH,
    },
    {
      parts: ['ends', 'with'],
      condition: AbilityCondition.ends_with,
      operator: TokenTypes.ENDS_WITH,
    },

    {
      parts: ['is', 'not', 'empty'],
      condition: AbilityCondition.not_empty,
      operator: TokenTypes.NOT_EMPTY,
    },
    { parts: ['is', 'empty'], condition: AbilityCondition.empty, operator: TokenTypes.EMPTY },

    {
      parts: ['is', 'not', 'equals'],
      condition: AbilityCondition.not_equals,
      operator: TokenTypes.NOT_EQ,
    },
    { parts: ['is', 'equals'], condition: AbilityCondition.equals, operator: TokenTypes.EQ },
    {
      parts: ['not', 'equals'],
      condition: AbilityCondition.not_equals,
      operator: TokenTypes.NOT_EQ,
    },

    { parts: ['is', 'in'], condition: AbilityCondition.in, operator: TokenTypes.IN },
    { parts: ['not', 'in'], condition: AbilityCondition.not_in, operator: TokenTypes.NOT_IN },

    {
      parts: ['is', 'not', { type: TokenTypes.NULL }],
      condition: AbilityCondition.not_equals,
      operator: TokenTypes.NOT_EQ_NULL,
    },
    {
      parts: ['is', { type: TokenTypes.NULL }],
      condition: AbilityCondition.equals,
      operator: TokenTypes.EQ_NULL,
    },

    {
      parts: ['is', 'not', 'defined'],
      condition: AbilityCondition.not_defined,
      operator: TokenTypes.DEFINED,
    },
    { parts: ['is', 'defined'], condition: AbilityCondition.defined, operator: TokenTypes.DEFINED },
  ];

  /**
   * Parses the comparison operator part of a rule.
   * Returns both the resulting AbilityCondition and the token type that was consumed.
   */
  private parseConditionOperator(): ParsedOperator {
    const token = this.stream.peek();

    for (const definition of this.operatorDefinitions) {
      if (this.tryMatch(definition.parts, definition.guard)) {
        return { condition: definition.condition, operator: definition.operator, token };
      }
    }

    // Single token (symbol or keyword)
    if (token.type !== TokenTypes.SYMBOL && token.type !== TokenTypes.KEYWORD) {
      this.stream.syntaxError(`Expected comparison operator, got \`${token.value}\``, token, [
        TokenTypes.SYMBOL,
        TokenTypes.KEYWORD,
      ]);
    }

    this.stream.next();

    const found = ((): Omit<ParsedOperator, 'token'> | null => {
      switch (token.value) {
        case '=':
        case '==':
        case 'equals':
        case 'is':
          return { condition: AbilityCondition.equals, operator: TokenTypes.EQ };
        case '!=':
        case '<>':
          return { condition: AbilityCondition.not_equals, operator: TokenTypes.NOT_EQ };
        case '>':
        case 'gt':
        case 'greater':
          return { condition: AbilityCondition.greater_than, operator: TokenTypes.GT };
        case '<':
        case 'lt':
        case 'less':
          return { condition: AbilityCondition.less_than, operator: TokenTypes.LT };
        case '>=':
        case 'gte':
          return { condition: AbilityCondition.greater_or_equal, operator: TokenTypes.GTE };
        case '<=':
        case 'lte':
          return { condition: AbilityCondition.less_or_equal, operator: TokenTypes.LTE };
        case 'contains':
        case 'includes':
        case 'has':
          return { condition: AbilityCondition.contains, operator: TokenTypes.CONTAINS };
        case 'in':
          return { condition: AbilityCondition.in, operator: TokenTypes.IN };
        default:
          return null;
      }
    })();

    if (!found) {
      return this.stream.syntaxError(`Unexpected operator token \`${token.value}\``, token, [
        TokenTypes.SYMBOL,
        TokenTypes.KEYWORD,
      ]);
    }

    return { ...found, token };
  }

  /**
   * Tries to match the sequence of the operator parts.
   * If the sequence does not match, the stream position is restored
   */
  private tryMatch(parts: readonly OperatorPart[], guard?: () => boolean): boolean {
    this.stream.mark();

    for (const part of parts) {
      let matched: boolean;

      if (typeof part === 'string') {
        matched = this.matchWord(part);
      } else if ('symbol' in part) {
        matched = this.matchSymbol(part.symbol);
      } else {
        matched = this.stream.match(part.type) !== null;
      }

      if (!matched) {
        this.stream.reset();
        return false;
      }
    }

    if (guard && !guard()) {
      this.stream.reset();
      return false;
    }

    this.stream.commit();
    return true;
  }

  /**
   * `contains all` / `contains any` is an operator only if it is followed by a value,
   * but not by `of` or `:` (group definition)
   */
  private isNotGroupContinuation(): boolean {
    return !this.stream.check(TokenTypes.OF) && !this.stream.check(TokenTypes.COLON);
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
        token.type === TokenTypes.NEVER ||
        token.type === TokenTypes.DEFINED ||
        token.type === TokenTypes.ALL ||
        token.type === TokenTypes.ANY) &&
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
  // #region Value parsing (literals, arrays, paths)
  // -------------------------------------------------------------------------

  /**
   * Parses a resource value:
   * - quoted string, number, boolean, null, array — literal value
   * - unquoted dot-notation identifier — path to the resource or environment
   */
  private parseValue(): ParsedValue {
    const token = this.stream.peek();

    // Arrays start with a left bracket
    if (token.type === TokenTypes.LBRACKET) {
      this.stream.next();
      return { resource: this.parseArray(), resourceType: 'value', token };
    }

    if (
      token.type === TokenTypes.ALL ||
      token.type === TokenTypes.ANY ||
      token.type === TokenTypes.EFFECT ||
      token.type === TokenTypes.EOF
    ) {
      this.stream.syntaxError(`Unexpected ${token.type} in value position`, token);
    }

    this.stream.next();

    switch (token.type) {
      case TokenTypes.STRING:
        return { resource: token.value, resourceType: 'value', token };
      case TokenTypes.NUMBER:
        return { resource: Number(token.value), resourceType: 'value', token };
      case TokenTypes.BOOLEAN:
        return { resource: token.value === 'true', resourceType: 'value', token };
      case TokenTypes.NULL:
        return { resource: null, resourceType: 'value', token };
      case TokenTypes.IDENTIFIER:
        if (!token.value.includes('.')) {
          this.stream.syntaxError(
            `Expected value or path, got \`${token.value}\`. If it is a string, wrap it in quotes: '${token.value}'`,
            token,
          );
        }
        return { resource: token.value, resourceType: 'path', token };
      default:
        return this.stream.syntaxError(`Unexpected value token "${token.value}"`, token, [
          TokenTypes.KEYWORD,
        ]);
    }
  }

  /**
   * Parses an array literal: [ <value>, <value>, ... ]
   * The opening bracket has already been consumed.
   * Array items may be literals only.
   */
  private parseArray(): ArrayValue {
    const arr: ArrayValue = [];

    while (!this.stream.eof() && !this.stream.check(TokenTypes.RBRACKET)) {
      const token = this.stream.next();

      switch (token.type) {
        case TokenTypes.STRING:
          arr.push(token.value);
          break;
        case TokenTypes.NUMBER:
          arr.push(Number(token.value));
          break;
        case TokenTypes.BOOLEAN:
          arr.push(token.value === 'true');
          break;
        case TokenTypes.NULL:
          arr.push(null);
          break;
        case TokenTypes.IDENTIFIER:
          this.stream.syntaxError(
            `Paths are not allowed inside arrays, got \`${token.value}\`. If it is a string, wrap it in quotes: '${token.value}'`,
            token,
          );
          break;
        case TokenTypes.LBRACKET:
          this.stream.syntaxError('Nested arrays are not supported', token);
          break;
        default:
          this.stream.syntaxError(`Unexpected token in array: \`${token.value}\``, token);
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
  // #region comments, annotations, aliases
  // -------------------------------------------------------------------------

  /**
   * Consumes comments, annotations and aliases before the policy
   */
  private consumeLeading() {
    while (
      this.stream.check(TokenTypes.COMMENT) ||
      this.stream.check(TokenTypes.ANNOTATION) ||
      this.stream.check(TokenTypes.ALIAS)
    ) {
      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();
      this.consumeLeadingAliases();
    }
  }

  private consumeLeadingComments() {
    while (this.stream.check(TokenTypes.COMMENT)) {
      this.stream.next();
    }
  }

  private consumeLeadingAliases() {
    while (this.stream.check(TokenTypes.ALIAS)) {
      const aliasToken = this.stream.next(); // consume "alias"

      const nameToken = this.stream.expect(TokenTypes.IDENTIFIER, `Expected alias name`);
      const aliasKey = nameToken.value;

      if (aliasKey.includes('.')) {
        this.stream.syntaxError(`Alias name must not contain dots, got \`${aliasKey}\``, nameToken);
      }

      if (this.aliasBuffer.has(aliasKey)) {
        this.stream.syntaxError(`Alias \`${aliasKey}\` is already defined`, nameToken);
      }

      this.stream.expect(TokenTypes.COLON, `Expected colon after an alias`);

      const annotations = this.takeAnnotations('alias');

      this.consumeLeadingComments();
      this.consumeLeadingAnnotations();

      if (!this.isStartOfRule()) {
        this.stream.syntaxError(`Alias \`${aliasKey}\` has no rule`, aliasToken);
      }

      const rule = this.parseRule();
      rule.name = annotations.name?.value || aliasKey;
      rule.description = annotations.description?.value;
      if (annotations.disabled?.value === true) {
        rule.disabled = true;
        rule.reset();
      }

      this.consumeLeadingComments();

      if (this.isStartOfRule()) {
        this.stream.syntaxError(
          `Alias \`${aliasKey}\` must contain exactly one rule`,
          this.stream.peek(),
        );
      }

      this.aliasBuffer.set(aliasKey, rule);
    }
  }

  private consumeLeadingAnnotations() {
    while (this.stream.check(TokenTypes.ANNOTATION)) {
      const token = this.stream.next();
      const match = token.value.trim().match(/^@(\S+)\s*([\s\S]*)$/);
      const key = match ? match[1] : '';
      const value = match ? match[2].trim() : '';

      if (!KNOWN_ANNOTATIONS.includes(key as AnnotationName)) {
        const suggestion = this.stream.suggest(key, KNOWN_ANNOTATIONS);
        this.stream.syntaxError(
          `Unknown annotation \`@${key}\`.${suggestion ? ` Did you mean \`@${suggestion}\`?` : ''} Allowed: ${KNOWN_ANNOTATIONS.map(a => '@' + a).join(', ')}`,
          token,
        );
      }

      const name = key as AnnotationName;

      if (this.annBuffer.has(name)) {
        this.stream.syntaxError(`Duplicate annotation \`@${name}\``, token);
      }

      if (name !== 'disabled' && value.length === 0) {
        this.stream.syntaxError(`Annotation \`@${name}\` requires a value`, token);
      }

      switch (name) {
        case 'id':
          this.annBuffer.setID(value, token);
          break;
        case 'name':
          this.annBuffer.setName(value, token);
          break;
        case 'description':
          this.annBuffer.setDescription(value, token);
          break;
        case 'priority':
          if (!/^-?\d+$/.test(value)) {
            this.stream.syntaxError(
              `Annotation \`@priority\` expects an integer, got \`${value}\``,
              token,
            );
          }
          this.annBuffer.setPriority(parseInt(value, 10), token);
          break;
        case 'disabled':
          if (value !== '' && value !== 'true' && value !== 'false') {
            this.stream.syntaxError(
              `Annotation \`@disabled\` expects \`true\` or \`false\`, got \`${value}\``,
              token,
            );
          }
          this.annBuffer.setDisabled(value !== 'false', token);
          break;
        case 'tags': {
          const tags = value.split(',').map(tag => tag.trim());
          if (tags.some(tag => tag.length === 0)) {
            this.stream.syntaxError('Annotation `@tags` contains an empty tag', token);
          }
          const invalidTag = tags.find(tag => /[\s[\]]/.test(tag));
          if (invalidTag) {
            this.stream.syntaxError(
              `Invalid tag \`${invalidTag}\`. Tags are separated by commas and must not contain spaces or brackets: @tags admin, user`,
              token,
            );
          }
          this.annBuffer.setTags(tags, token);
          break;
        }
      }
    }
  }

  private takeAnnotations(owner: 'policy' | 'ruleSet' | 'rule' | 'alias'): AbilityDSLAnnotations {
    const ann = this.annBuffer.clone();
    this.annBuffer.clear();

    const allowed = AnnotationAllowed[owner];
    for (const entry of ann.entries()) {
      if (!allowed.has(entry.key)) {
        this.stream.syntaxError(
          `Annotation @${entry.key} is not allowed on ${owner}. Allowed: ${[...allowed]
            .map(a => '@' + a)
            .join(', ')}`,
          entry.token ?? this.stream.peek(),
        );
      }
    }

    return ann;
  }

  /**
   * Annotations must be followed by a policy, group, rule or alias
   */
  private assertNoPendingAnnotations() {
    const [entry] = this.annBuffer.entries();
    if (entry) {
      this.stream.syntaxError(
        `Annotation \`@${entry.key}\` is not attached to any policy, group, rule or alias`,
        entry.token ?? this.stream.peek(),
      );
    }
  }

  /**
   * Checks the uniqueness of the explicitly specified ID
   */
  private registerID(kind: 'policy' | 'ruleSet' | 'rule', annotations: AbilityDSLAnnotations) {
    const entry = annotations.id;
    if (!entry) {
      return;
    }

    if (this.ids[kind].has(entry.value)) {
      this.stream.syntaxError(
        `Duplicate @id \`${entry.value}\`. The ID must be unique`,
        entry.token ?? this.stream.peek(),
      );
    }

    this.ids[kind].add(entry.value);
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

  private isStartOfRule(): boolean {
    return (
      this.stream.check(TokenTypes.IDENTIFIER) ||
      this.stream.check(TokenTypes.ALWAYS) ||
      this.stream.check(TokenTypes.NEVER)
    );
  }

  private isStartOfExcept(): boolean {
    return this.stream.check(TokenTypes.EXCEPT);
  }

  private isStartOfAlias(): boolean {
    return this.stream.check(TokenTypes.ALIAS);
  }
}
