import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';

class AbilityRule<Subject = unknown, Resource = unknown, Environment = unknown> {
  public statements: AbilityStatement[];
  public constructor(statements: AbilityStatement[]) {
    this.statements = statements;
  }

  public getStatements() {
    return this.statements;
  }

  public isPermit(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ) {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'permit';
  }

  public isDeny(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ) {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'deny';
  }

  public check(
    subject: Subject | null | undefined,
    resource?: Resource | null | undefined,
    environment?: Environment | null | undefined,
  ) {
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
