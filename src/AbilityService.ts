import AbilityRule, { AbilityRuleStatus } from './AbilityRule';
import AbilityPolicy, { AbilityCompareMethod, AbilityPolicyResult } from './AbilityPolicy';

export type AbilityEnforceResult = {
  readonly permission: AbilityRuleStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedPolicies: readonly AbilityPolicy[];
};

class AbilityService {
  /**
   * Create the rule to compare
   *
   * @param ruleName {string} - The rule name
   * @param effect {AbilityRuleStatus} - Return value
   * @param matches {AbilityRuleMatches} - The matching rule he matching rule can be on of the format:
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
  public createRule(...args: ConstructorParameters<typeof AbilityRule>): AbilityRule {
    return new AbilityRule(...args);
  }

  /**
   * Create the Policy class instance
   */
  public createPolicy(...args: ConstructorParameters<typeof AbilityPolicy>): AbilityPolicy {
    return new AbilityPolicy(...args);
  }

  public checkPolicies(
    policiesResult: readonly AbilityPolicyResult[],
    compareMethod?: AbilityCompareMethod | undefined,
  ): AbilityEnforceResult {
    const deniedRules: AbilityRule[] = [];
    const deniedPolicies: AbilityPolicy[] = [];
    const statuses: AbilityRuleStatus[] = [];

    policiesResult.forEach(policyResult => {
      statuses.push(policyResult.permission);

      if (policyResult.permission === 'deny') {
        policyResult.deniedRules.forEach(rule => {
          deniedRules.push(rule);
        });
        policyResult.deniedRules.forEach(st => {
          deniedRules.push(st);
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
      deniedPolicies,
    };
  }

  public enforcePolicies(
    policiesResult: readonly AbilityPolicyResult[],
    compareMethod?: AbilityCompareMethod | undefined,
  ): void | never {
    const { permission, deniedRules } = this.checkPolicies(policiesResult, compareMethod);

    if (permission === 'deny') {
      throw new Error(`Permission denied. ${deniedRules[0].getName().toString()}`);
    }
  }
}

export default AbilityService;
