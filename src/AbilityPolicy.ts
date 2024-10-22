import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';
import AbilityRule from './AbilityRule';

export type AbilityRuleCompareMethod = 'or' | 'and';
export type AbilityEnforceResult = {
  readonly permission: AbilityStatementStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedStatements: readonly AbilityStatement[];
};

class AbilityPolicy<Subject = unknown, Resource = unknown, Environment = unknown> {
  public rules: AbilityRule[] = [];
  public compareMethod: AbilityRuleCompareMethod = 'and';
  public target: string = '<unknown-target>';
  public name: string;

  public constructor(policyName: string) {
    this.name = policyName;
  }

  public addRule(rule: AbilityRule, compareMethod: AbilityRuleCompareMethod = 'and'): this {
    this.rules.push(rule);
    this.compareMethod = compareMethod;

    return this;
  }

  public addRules(rules: AbilityRule[], compareMethod: AbilityRuleCompareMethod = 'and'): this {
    rules.forEach(rule => {
      this.addRule(rule, compareMethod);
    });

    return this;
  }

  public setTarget(target: string): this {
    this.target = target;

    return this;
  }

  public getTarget() {
    return this.target;
  }

  public getName() {
    return this.name;
  }

  public throwCheck(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ): void | never {
    const { permission, deniedStatements } = this.check(subject, resource, environment);

    if (permission === 'deny') {
      throw new Error(
        `Permission denied.\n${deniedStatements.map(st => st.getName()).join('.\n')}`,
      );
    }
  }

  public isPermit(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ): boolean {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'permit';
  }

  public isDeny(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ): boolean {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'deny';
  }

  public check(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ): AbilityEnforceResult {
    const deniedRules: AbilityRule[] = [];
    const deniedStatements: AbilityStatement[] = [];
    const statuses: AbilityStatementStatus[] = [];

    if (this.rules.length === 0) {
      deniedStatements.push(new AbilityStatement('Missing rules', ['subject.', '=', '']));
      deniedRules.push(new AbilityRule(deniedStatements));

      return {
        permission: 'deny',
        deniedRules,
        deniedStatements,
      };
    }

    this.rules.forEach(rule => {
      const { permission, deniedStatements: st } = rule.check(subject, resource, environment);

      statuses.push(permission);

      if (permission === 'deny') {
        deniedRules.push(rule);
        st.forEach(statement => {
          deniedStatements.push(statement);
        });
      }
    });

    const res = statuses[this.compareMethod === 'and' ? 'every' : 'some'](
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
