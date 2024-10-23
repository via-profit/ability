import { AbilityCompareMethod } from './AbilityPolicy';
import AbilityStatement, {
  AbilityStatementConfig,
  AbilityStatementStatus,
} from './AbilityStatement';

export type AbilityRuleResult = {
  readonly permission: AbilityStatementStatus;
  readonly deniedStatements: readonly AbilityStatement[];
};

export type AbilityRuleConfig = {
  readonly name: string;
  readonly statements: {
    readonly compareMethod: AbilityCompareMethod;
    readonly list: AbilityStatementConfig[];
  };
};

class AbilityRule<Subject = unknown, Resource = unknown, Environment = unknown> {
  /**
   * Statements list
   */
  public statements: AbilityStatement[] = [];

  /**
   * Statements compare method.\
   * For the «and» mrthod the statement will be permitted if all\
   * statements will be returns «permit» status and for the «or» - if\
   * one of the statements returns as «permit»
   */
  public statementsCompareMethod: AbilityCompareMethod = 'and';

  /**
   * Rule name
   */
  public name: string | symbol;

  public constructor(name: string | symbol) {
    this.name = name;
  }

  public getName() {
    return this.name;
  }

  public addStatement(
    statement: AbilityStatement,
    compareMethod: AbilityCompareMethod = 'and',
  ): this {
    this.statements.push(statement);
    this.statementsCompareMethod = compareMethod;

    return this;
  }

  public addStatements(
    statements: AbilityStatement[],
    compareMethod: AbilityCompareMethod = 'and',
  ): this {
    statements.forEach(st => this.addStatement(st, compareMethod));

    return this;
  }

  public getStatements() {
    return this.statements;
  }

  public isPermit(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ) {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'permit';
  }

  public isDeny(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ) {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'deny';
  }

  public check(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ): AbilityRuleResult {
    const affected = this.statements.map(statement => {
      const status = statement.check(subject, resource, environment);

      return { statement, status };
    });

    const res: AbilityStatementStatus = affected[
      this.statementsCompareMethod === 'and' ? 'every' : 'some'
    ](statement => statement.status === 'permit')
      ? 'permit'
      : 'deny';

    return {
      permission: res,
      deniedStatements: affected
        .filter(({ status }) => status === 'deny')
        .map(({ statement }) => statement),
    };
  }

  /**
   * Parsing the rule config object or JSON string\
   * of config and returns the AbilityRule class instance
   */
  public static parse(configOrJson: AbilityRuleConfig | string): AbilityRule {
    const { name, statements } =
      typeof configOrJson === 'string'
        ? (JSON.parse(configOrJson) as AbilityRuleConfig)
        : configOrJson;

    const statementInstances = statements.list.map(statementConfig => {
      return AbilityStatement.parse(statementConfig);
    });

    return new AbilityRule(name).addStatements(statementInstances, statements.compareMethod);
  }
}

export default AbilityRule;
