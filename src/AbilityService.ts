import AbilityStatement, {
  AbilityStatementMatches,
  AbilityStatementStatus,
} from './AbilityStatement';
import AbilityPolicy, { AbilityCompareMethod, AbilityPolicyResult } from './AbilityPolicy';
import AbilityRule from './AbilityRule';

export type AbilityStatementConfig = {
  readonly name: string;
  readonly effect: AbilityStatementStatus;
  readonly matches: AbilityStatementMatches;
};

export type AbilityPolicyConfig = {
  readonly id: string;
  readonly name: string;
  readonly description?: string;
  readonly target: string;
  readonly ruleCompareMethod: AbilityCompareMethod;
  readonly rules: AbilityStatementConfig[][];
};

export type AbilityEnforceResult = {
  readonly permission: AbilityStatementStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedStatements: readonly AbilityStatement[];
  readonly deniedPolicies: readonly AbilityPolicy[];
};

class AbilityService {
  public createRule(...args: ConstructorParameters<typeof AbilityRule>): AbilityRule {
    return new AbilityRule(...args);
  }

  /**
   * Create the statement to compare
   *
   * @param statementName {string} - The statement name
   * @param effect {AbilityStatementStatus} - Return value
   * @param matches {AbilityStatementMatches} - The matching rule he matching rule can be on of the format:
   * \
   * For example, be compared two's data\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The resource_
   * ```json
   * {"departamentID": "154", "departamentName": "NBC"}
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "resource.departamentName"]
   * ```
   *
   * \
   * **Example 2.**\
   * In this case will be compared resource and string:
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The resource_ will be «undefined».\
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "NBC"]
   * ```
   * \
   * **Example 3.**\
   * In this case will be compared resource and array of string:\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The resource_
   * ```json
   * ["FOX", "NBC", "AONE"]
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "resource"]
   * ```
   * **Note: In this rule whe set the resource field as the «resource» string.\
   * This means that we will compare the entire resource as a whole,\
   * and not search for it by field name.**
   */
  public createStatement(
    ...args: ConstructorParameters<typeof AbilityStatement>
  ): AbilityStatement {
    return new AbilityStatement(...args);
  }

  /**
   * Create the Policy class instance
   */
  public createPolicy<Subject = unknown, Resource = unknown, Environment = unknown>(
    ...args: ConstructorParameters<typeof AbilityPolicy>
  ): AbilityPolicy {
    return new AbilityPolicy<Subject, Resource, Environment>(...args);
  }

  /**
   * Parse the config JSON format to Policy class instance
   */
  public parsePolicyConfig<Subject = unknown, Resource = unknown, Environment = unknown>(
    config: AbilityPolicyConfig,
  ): AbilityPolicy<Subject, Resource, Environment> {
    const { name, rules, ruleCompareMethod, target } = config;

    const ruless = rules.map(statements => {
      return new AbilityRule(
        statements.map(statementConfig => {
          const { name, effect, matches } = statementConfig;

          return new AbilityStatement(name, matches, effect);
        }),
      );
    });

    return this.createPolicy<Subject, Resource, Environment>(name)
      .addRules(ruless, ruleCompareMethod)
      .setTarget(target);
  }

  public enforcePolicies(
    policiesResult: readonly AbilityPolicyResult[],
    compareMethod?: AbilityCompareMethod | undefined,
  ): AbilityEnforceResult {
    const deniedRules: AbilityRule[] = [];
    const deniedStatements: AbilityStatement[] = [];
    const deniedPolicies: AbilityPolicy[] = [];
    const statuses: AbilityStatementStatus[] = [];

    policiesResult.forEach(policyResult => {
      statuses.push(policyResult.permission);

      if (policyResult.permission === 'deny') {
        policyResult.deniedRules.forEach(rule => {
          deniedRules.push(rule);
        });
        policyResult.deniedStatements.forEach(st => {
          deniedStatements.push(st);
        });
      }
    });

    const permission = statuses[compareMethod === 'and' ? 'every' : 'some'](
      status => status === 'permit',
    )
      ? 'permit'
      : 'deny';

    return {
      permission,
      deniedRules,
      deniedStatements,
      deniedPolicies,
    };
  }

  public throwEnforcePolicies(
    policiesResult: readonly AbilityPolicyResult[],
    compareMethod?: AbilityCompareMethod | undefined,
  ): void | never {
    const { permission, deniedStatements } = this.enforcePolicies(policiesResult, compareMethod);

    if (permission === 'deny') {
      throw new Error(
        `Permission denied. ${deniedStatements.map(st => st.getName()).join('. ')}`,
      );
    }
  }
}

export default AbilityService;
