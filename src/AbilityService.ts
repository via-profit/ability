import AbilityStatement, {
  AbilityStatementMatches,
  AbilityStatementStatus,
} from './AbilityStatement';
import AbilityPolicy, { AbilityRuleCompareMethod } from './AbilityPolicy';
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
  readonly ruleCompareMethod: AbilityRuleCompareMethod;
  readonly rules: AbilityStatementConfig[][];
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
}

export default AbilityService;
