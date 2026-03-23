import AbilityCompare from '~/core/AbilityCompare';
import AbilityCondition from '~/core/AbilityCondition';
import AbilityPolicy from '~/core/AbilityPolicy';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import AbilityRule, { AbilityRuleConfig } from '~/core/AbilityRule';
import AbilityRuleSet from '~/core/AbilityRuleSet';
import { AbilityDSLLexer } from '~/parsers/dsl/AbilityDSLLexer';
import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';

export class AbilityDSLParser {
  private tokens: AbilityDSLToken[] = [];
  private pos = 0;

  constructor(private readonly dsl: string) {}

public parse(): AbilityPolicy[] {
  this.tokens = new AbilityDSLLexer(this.dsl).tokenize();
  this.pos = 0;

  const policies: AbilityPolicy[] = [];

  while (!this.isAtEnd()) {
    if (!this.isStartOfPolicy()) {
      throw new Error(`Expected policy, got ${this.peek().type.code}`);
    }
    policies.push(this.parsePolicy());
  }

  return policies;
}

  // ───────────────────────────────────────────────
  // ПОЛИТИКА
  // ───────────────────────────────────────────────

  private parsePolicy(): AbilityPolicy {
  const effectToken = this.consume(AbilityDSLTokenType.EFFECT, 'Expected effect');
  const effect = effectToken.value;

  const actionToken = this.consume(AbilityDSLTokenType.ACTION, 'Expected action');
  const action = actionToken.value;

  this.consume(AbilityDSLTokenType.IF, 'Expected "if"');

  const compareToken = this.consumeOneOf(
    [AbilityDSLTokenType.ALL, AbilityDSLTokenType.ANY],
    'Expected "all" or "any"',
  );

  const compareMethod =
    compareToken.type === AbilityDSLTokenType.ALL
      ? AbilityCompare.and
      : AbilityCompare.or;

  this.consume(AbilityDSLTokenType.COLON, 'Expected ":"');

  const ruleSets = this.parseRuleSets();

  const policy = new AbilityPolicy({
    id: `${effect}:${action}:${Math.random()}`,
    name: `${effect} ${action}`,
    action,
    effect: effect === 'permit' ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny,
    compareMethod,
  }).addRuleSets(ruleSets);

  return policy;
}

  // ───────────────────────────────────────────────
  // ГРУППЫ ПРАВИЛ
  // ───────────────────────────────────────────────

 private parseRuleSets(): AbilityRuleSet[] {
  const sets: AbilityRuleSet[] = [];

  // Собираем группы, пока не встретим конец или начало новой политики
  while (!this.isAtEnd() && !this.isStartOfPolicy()) {
    if (this.isStartOfGroup()) {
      sets.push(this.parseGroup());
    } else {
      // Если не группа и не политика — ошибка
      break;
    }
  }

  return sets;
}

  // parseGroup читает только правила
  private parseGroup(): AbilityRuleSet {
  const compareToken = this.consumeOneOf(
    [AbilityDSLTokenType.ALL, AbilityDSLTokenType.ANY],
    'Expected "all" or "any"',
  );

  const compareMethod =
    compareToken.type === AbilityDSLTokenType.ALL 
      ? AbilityCompare.and 
      : AbilityCompare.or;

  this.consume(AbilityDSLTokenType.OF, 'Expected "of"');
  this.consume(AbilityDSLTokenType.COLON, 'Expected ":"');

  const group = new AbilityRuleSet({ compareMethod });

  // Читаем правила, пока не встретим:
  // - начало новой группы (ALL/ANY)
  // - начало новой политики (EFFECT)
  // - конец файла
  while (!this.isAtEnd() && !this.isStartOfGroup() && !this.isStartOfPolicy()) {
    if (this.check(AbilityDSLTokenType.IDENTIFIER)) {
      group.addRule(this.parseRule());
    } else {
      // Если не IDENTIFIER и не начало группы/политики — ошибка
      throw new Error(`Unexpected token in group: ${this.peek().type.code}`);
    }
  }

  return group;
}

private isStartOfGroup(): boolean {
  return this.check(AbilityDSLTokenType.ALL) || this.check(AbilityDSLTokenType.ANY);
}

private isStartOfPolicy(): boolean {
  return this.check(AbilityDSLTokenType.EFFECT);
}
  // ───────────────────────────────────────────────
  // ПРАВИЛО
  // ───────────────────────────────────────────────

  private parseRule(): AbilityRule {
  if (!this.check(AbilityDSLTokenType.IDENTIFIER)) {
    throw new Error(`Expected identifier, got ${this.peek().type.code}`);
  }

  const subject = this.consume(AbilityDSLTokenType.IDENTIFIER, 'Expected field').value;
  const { condition, operator } = this.parseConditionOperator();

  let resource: AbilityRuleConfig['resource'];

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

  // ───────────────────────────────────────────────
  // ОПЕРАТОР УСЛОВИЯ
  // ───────────────────────────────────────────────

  private parseConditionOperator(): { condition: AbilityCondition; operator: AbilityDSLTokenType } {
    const token = this.advance();

    switch (token.type) {
      case AbilityDSLTokenType.EQ:
        if (this.check(AbilityDSLTokenType.NULL)) {
          this.advance();
          return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.EQ_NULL };
        }
        return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.EQ };

      case AbilityDSLTokenType.EQ_NULL:
        return { condition: AbilityCondition.equal, operator: AbilityDSLTokenType.EQ_NULL };

      case AbilityDSLTokenType.NOT_EQ_NULL:
        return { condition: AbilityCondition.not_equal, operator: AbilityDSLTokenType.NOT_EQ_NULL };

      case AbilityDSLTokenType.CONTAINS:
        return { condition: AbilityCondition.in, operator: AbilityDSLTokenType.CONTAINS };

      case AbilityDSLTokenType.IN:
        return { condition: AbilityCondition.in, operator: AbilityDSLTokenType.IN };

      case AbilityDSLTokenType.GT_WORD:
        if (this.matchWord('equal')) {
          return {
            condition: AbilityCondition.more_or_equal,
            operator: AbilityDSLTokenType.GT_WORD,
          };
        }
        return { condition: AbilityCondition.more_than, operator: AbilityDSLTokenType.GT_WORD };

      case AbilityDSLTokenType.LT_WORD:
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

  private matchWord(word: string): boolean {
    if (this.peek()?.value === word) {
      this.advance();
      return true;
    }
    return false;
  }

  // ───────────────────────────────────────────────
  // ЗНАЧЕНИЕ
  // ───────────────────────────────────────────────

  private parseValue(): AbilityRuleConfig['resource'] {
    // Массив
    if (this.check(AbilityDSLTokenType.LBRACKET)) {
      this.advance();
      return this.parseArray();
    }

    // Проверяем, что следующий токен — значение, а не служебный токен
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

  private parseArray(): (string | number | boolean)[] {
    const arr: (string | number | boolean)[] = [];

    // Пропускаем LBRACKET (он уже съеден в parseValue)
    // Теперь читаем элементы, пока не встретим RBRACKET
    while (!this.isAtEnd() && !this.check(AbilityDSLTokenType.RBRACKET)) {
      const value = this.parseValue();

      // parseValue может вернуть массив (если вложенный массив), строку, число, булево
      if (Array.isArray(value)) {
        arr.push(...value);
      } else if (
        typeof value === 'string' ||
        typeof value === 'number' ||
        typeof value === 'boolean'
      ) {
        arr.push(value);
      } else if (value === null) {
        throw new Error('Unexpected null in array');
        // arr.push(null);
      }

      // Пропускаем запятую, если есть
      if (this.check(AbilityDSLTokenType.COMMA)) {
        this.advance();
      }
    }

    this.consume(AbilityDSLTokenType.RBRACKET, 'Expected "]"');
    return arr;
  }

  // ───────────────────────────────────────────────
  // HELPERS
  // ───────────────────────────────────────────────

  private consumeOneOf(types: AbilityDSLTokenType[], msg: string): AbilityDSLToken {
    for (const t of types) {
      if (this.check(t)) {
        return this.advance();
      }
    }
    throw new Error(msg);
  }

  private match(type: AbilityDSLTokenType): boolean {
    if (this.check(type)) {
      this.advance();
      return true;
    }
    return false;
  }

  private consume(type: AbilityDSLTokenType, msg: string): AbilityDSLToken {
    if (this.check(type)) {
      return this.advance();
    }

    throw new Error(msg + ` at token ${this.peek()?.value}`);
  }

  private check(type: AbilityDSLTokenType): boolean {
    if (this.isAtEnd()) {
      return false;
    }
    return this.peek().type.code === type.code;
  }

  private checkNext(type: AbilityDSLTokenType): boolean {
    if (this.pos + 1 >= this.tokens.length) {
      return false;
    }
    return this.tokens[this.pos + 1].type.code === type.code;
  }

  private advance(): AbilityDSLToken {
    return this.tokens[this.pos++];
  }

  private peek(): AbilityDSLToken {
    return this.tokens[this.pos];
  }

  private isAtEnd(): boolean {
    //  return this.pos >= this.tokens.length - 1;
    return this.peek().type.code === AbilityDSLTokenType.EOF.code;
  }
}
