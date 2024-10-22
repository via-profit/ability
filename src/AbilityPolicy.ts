import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';
import AbilityRule from './AbilityRule';

export type AbilityCompareMethod = 'or' | 'and';
export type AbilityPolicyResult = {
  readonly permission: AbilityStatementStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedStatements: readonly AbilityStatement[];
};

class AbilityPolicy<Subject = unknown, Resource = unknown, Environment = unknown> {
  public rules: AbilityRule[] = [];
  public compareMethod: AbilityCompareMethod = 'and';
  public target: string = '<unknown-target>';
  public name: string;

  public constructor(policyName: string) {
    this.name = policyName;
  }

  public addRule(rule: AbilityRule, compareMethod: AbilityCompareMethod = 'and'): this {
    this.rules.push(rule);
    this.compareMethod = compareMethod;

    return this;
  }

  public addRules(rules: AbilityRule[], compareMethod: AbilityCompareMethod = 'and'): this {
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

  public throwEnforce(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ): void | never {
    const { permission, deniedStatements } = this.enforce(subject, resource, environment);

    if (permission === 'deny') {
      throw new Error(
        `Permission denied. ${deniedStatements.map(st => st.getName()).join('. ')}`,
      );
    }
  }

  public isPermit(subject: Subject, resource?: Resource, environment?: Environment): boolean {
    const { permission } = this.enforce(subject, resource, environment);

    return permission === 'permit';
  }

  public isDeny(subject: Subject, resource?: Resource, environment?: Environment): boolean {
    const { permission } = this.enforce(subject, resource, environment);

    return permission === 'deny';
  }

  public enforce(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ): AbilityPolicyResult {
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
      const { permission, deniedStatements: st } = rule.enforce(subject, resource, environment);

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
