import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';

class AbilityRule {
  #statements: AbilityStatement[];
  public constructor(statements: AbilityStatement[]) {
    this.#statements = statements;
  }

  public getStatements() {
    return this.#statements;
  }

  public enforce(subject: unknown, obj?: unknown, env?: unknown | undefined) {
    const affected = this.#statements.map(statement => {
      const status = statement.enforce(subject, obj, env);

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
