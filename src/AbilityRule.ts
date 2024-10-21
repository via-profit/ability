import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';

class AbilityRule {
  #statements: AbilityStatement[];
  public constructor(statements: AbilityStatement[]) {
    this.#statements = statements;
  }

  public getStatements() {
    return this.#statements;
  }

  /**
   * Запустить проверку условий
   * Все условия в одном правиле должны вернуть «permit»,
   * тогда правило считается разрешательным, в противном случае - запрещает
   */
  public enforce(subject: unknown, obj?: unknown) {
    const affected = this.#statements.map(statement => {
      const status = statement.enforce(subject, obj);

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
