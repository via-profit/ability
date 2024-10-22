import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';

class AbilityRule {
  public statements: AbilityStatement[];
  public constructor(statements: AbilityStatement[]) {
    this.statements = statements;
  }

  public getStatements() {
    return this.statements;
  }

  public check(...args: Parameters<AbilityRule['enforce']>) {
    return this.enforce(...args);
  }

  public isPermit(...args: Parameters<AbilityRule['enforce']>) {
    const { permission } = this.enforce(...args);

    return permission === 'permit';
  }

  public isDeny(...args: Parameters<AbilityRule['enforce']>) {
    const { permission } = this.enforce(...args);

    return permission === 'deny';
  }

  private enforce(subject: unknown, resource?: unknown, environment?: unknown | undefined) {
    const affected = this.statements.map(statement => {
      const status = statement.check(subject, resource, environment);

      return { statement, status };
    });

    const res: AbilityStatementStatus = affected.every(statement => statement.status === 'permit')
      ? 'permit'
      : 'deny';

    return {
      permission: res,
      deniedStatements: affected
        .filter(({ status }) => status === 'deny')
        .map(({ statement }) => statement),
    };
  }
}

export default AbilityRule;
