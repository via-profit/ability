import AbilityPolicy from '~/core/AbilityPolicy';
import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLLexer } from '~/parsers/dsl/AbilityDSLLexer';
import { AbilityDSLTokenType } from '~/parsers/dsl/AbilityDSLTokenType';
import AbilityPolicyEffect from '~/core/AbilityPolicyEffect';
import AbilityCompare from '~/core/AbilityCompare';
import AbilityRuleSet from '~/core/AbilityRuleSet';
import AbilityRule from '~/core/AbilityRule';

export class AbilityDSLParser {
  private tokens: AbilityDSLToken[] = [];
  private pos = 0;

  public constructor(private readonly dsl: string) {}

  public parse(): AbilityPolicy[] {
    this.tokens = new AbilityDSLLexer(this.dsl).tokenize();
    this.pos = 0;

    const policies: AbilityPolicy[] = [];

    while (!this.isAtEnd()) {
      policies.push(this.parsePolicy());
    }

    return policies;
  }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ПОЛИТИКИ
  // ───────────────────────────────────────────────

  private parsePolicy(): AbilityPolicy {
    // 1. effect
    const effectToken = this.consume(AbilityDSLTokenType.EFFECT, 'Expected effect (allow/deny)');
    const effect = effectToken.value; // permit / deny

    // 2. action
    const actionToken = this.consume(AbilityDSLTokenType.ACTION, 'Expected action name');
    const action = actionToken.value;

    // 3. IF
    this.consume(AbilityDSLTokenType.IF, 'Expected "if"');

    // 4. colon
    this.consume(AbilityDSLTokenType.COLON, 'Expected ":"');

    // 5. ANY OF / ALL OF
    let compareMethod: AbilityCompare;
    if (this.match(AbilityDSLTokenType.ALL)) {
      compareMethod = AbilityCompare.and;
      this.advance();
    } else if (this.match(AbilityDSLTokenType.ANY)) {
      compareMethod = AbilityCompare.or;
      this.advance();
    } else {
      throw new Error('Expected "all of" or "any of"');
    }

    this.consume(AbilityDSLTokenType.COLON, 'Expected ":"');


    // 5. Создаём AbilityPolicy
    return new AbilityPolicy({
      id: `${effect}:${action}:${Math.random()}`,
      name: `${effect} ${action}`,
      action,
      effect: effect === 'permit' ? AbilityPolicyEffect.permit : AbilityPolicyEffect.deny,
      compareMethod,
    })
      // .addRuleSets(this.parseRuleSets());
  }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ГРУПП ПРАВИЛ (разделённых OR)
  // ───────────────────────────────────────────────

  private parseRuleSets(): AbilityRuleSet[] {
    const ruleSetList: AbilityRuleSet[] = [];


      // if (this.check(AbilityDSLTokenType.ALL)) {
      //   this.advance();
      // }

      // ruleSetList.push(this.parseRuleGroup());

      // while (this.match(AbilityDSLTokenType.OR)) {
      //   ruleSetList.push(this.parseRuleGroup());
      // }

      return ruleSetList;
  }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ОДНОЙ ГРУППЫ ПРАВИЛ (A and B and C)
  // ───────────────────────────────────────────────

  // private parseRuleGroup() {
  //   const rules: AbilityRule[] = [];
  //
  //   rules.push(this.parseCondition());
  //
  //   while (this.match(AbilityDSLTokenType.AND)) {
  //     rules.push(this.parseCondition());
  //   }
  //
  //   return {
  //     compareMethod: 'and',
  //     rules,
  //   };
  // }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ОДНОГО УСЛОВИЯ
  // ───────────────────────────────────────────────

  // private parseCondition() {
  //   // path
  //   const left = this.parsePath();
  //
  //   // operator
  //   const operator = this.parseOperator();
  //
  //   // right side: path | string | number | boolean
  //   const right = this.parseValue();
  //
  //   return {
  //     field: left,
  //     op: operator,
  //     value: right,
  //   };
  // }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ПУТИ (user.role, order.items[0].id)
  // ───────────────────────────────────────────────

  private parsePath(): string {
    let path = this.consume(AbilityDSLTokenType.IDENTIFIER, 'Expected identifier').value;

    while (true) {
      if (this.match(AbilityDSLTokenType.DOT)) {
        const part = this.consume(AbilityDSLTokenType.IDENTIFIER, 'Expected identifier after dot');
        path += '.' + part.value;
        continue;
      }

      if (this.match(AbilityDSLTokenType.LBRACKET)) {
        const index = this.consume(AbilityDSLTokenType.NUMBER, 'Expected array index');
        this.consume(AbilityDSLTokenType.RBRACKET, 'Expected closing bracket');
        path += `[${index.value}]`;
        continue;
      }

      break;
    }

    return path;
  }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ОПЕРАТОРА
  // ───────────────────────────────────────────────

  private parseOperator(): string {
    const token = this.advance();

    switch (token.type) {
      case AbilityDSLTokenType.EQ:
        return 'eq';
      case AbilityDSLTokenType.NEQ:
        return 'neq';
      case AbilityDSLTokenType.GT:
        return 'gt';
      case AbilityDSLTokenType.GTE:
        return 'gte';
      case AbilityDSLTokenType.LT:
        return 'lt';
      case AbilityDSLTokenType.LTE:
        return 'lte';
      case AbilityDSLTokenType.CONTAINS:
        return 'contains';
      case AbilityDSLTokenType.NOT_CONTAINS:
        return 'not_contains';
      case AbilityDSLTokenType.IN:
        return 'in';
      case AbilityDSLTokenType.NOT_IN:
        return 'not_in';
    }

    throw new Error(`Unexpected operator: ${token.value}`);
  }

  // ───────────────────────────────────────────────
  // ПАРСИНГ ЗНАЧЕНИЯ
  // ───────────────────────────────────────────────

  private parseValue(): unknown {
    const token = this.advance();

    switch (token.type) {
      case AbilityDSLTokenType.STRING:
        return token.value;
      case AbilityDSLTokenType.NUMBER:
        return Number(token.value);
      case AbilityDSLTokenType.BOOLEAN:
        return token.value === 'true';
      case AbilityDSLTokenType.IDENTIFIER:
        // Это ссылка на поле: order.owner
        return { ref: token.value };
    }

    throw new Error(`Unexpected value: ${token.value}`);
  }

  // ───────────────────────────────────────────────
  // ВСПОМОГАТЕЛЬНЫЕ МЕТОДЫ
  // ───────────────────────────────────────────────

  private match(type: AbilityDSLTokenType): boolean {
    if (this.check(type)) {
      this.advance();
      return true;
    }
    return false;
  }

  private consume(type: AbilityDSLTokenType, message: string): AbilityDSLToken {
    if (this.check(type)) {
      return this.advance();
    }

    throw new Error(message + ` at token ${this.peek()?.value}`);
  }

  private check(type: AbilityDSLTokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().type === type;
  }

  private advance(): AbilityDSLToken {
    return this.tokens[this.pos++];
  }

  private peek(): AbilityDSLToken {
    return this.tokens[this.pos];
  }

  private isAtEnd(): boolean {
    return this.pos >= this.tokens.length;
  }
}
