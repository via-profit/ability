import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';
import AbilityRule from './AbilityRule';

export type AbilityRuleCompareMethod = 'or' | 'and';
export type AbilityEnforceResult = {
  readonly permission: AbilityStatementStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedStatements: readonly AbilityStatement[];
};

class AbilityPolicy {
  #rules: AbilityRule[] = [];
  #compareMethod: AbilityRuleCompareMethod = 'and';
  #target: string = '<unknown-target>';
  #name: string;

  public constructor(ruleName: string) {
    this.#name = ruleName;
  }

  public addRule(rule: AbilityRule, compareMethod: AbilityRuleCompareMethod = 'and'): this {
    this.#rules.push(rule);
    this.#compareMethod = compareMethod;

    return this;
  }

  public addRules(rules: AbilityRule[], compareMethod: AbilityRuleCompareMethod = 'and'): this {
    rules.forEach(rule => {
      this.addRule(rule, compareMethod);
    });

    return this;
  }

  public setTarget(target: string): this {
    this.#target = target;

    return this;
  }

  public getTarget() {
    return this.#target;
  }

  public getName() {
    return this.#name;
  }

  public enforce(subject: unknown, obj?: unknown | undefined): AbilityEnforceResult {
    const deniedRules: AbilityRule[] = [];
    const deniedStatements: AbilityStatement[] = [];
    const statuses: AbilityStatementStatus[] = [];

    if (this.#rules.length === 0) {
      deniedStatements.push(new AbilityStatement('Missing rules', ['', '=', '']));
      deniedRules.push(new AbilityRule(deniedStatements));

      return {
        permission: 'deny',
        deniedRules,
        deniedStatements,
      };
    }

    this.#rules.forEach(rule => {
      const { permission, deniedStatements: st } = rule.enforce(subject, obj);

      statuses.push(permission);

      if (permission === 'deny') {
        deniedRules.push(rule);
        st.forEach(statement => {
          deniedStatements.push(statement);
        });
      }
    });

    const res = statuses[this.#compareMethod === 'and' ? 'every' : 'some'](
      status => status === 'permit',
    )
      ? 'permit'
      : 'deny';

    return {
      permission: res,
      deniedRules,
      deniedStatements,
    };
  }
}

export default AbilityPolicy;
