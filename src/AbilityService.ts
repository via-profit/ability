import AbilityStatement, {
  AbilityStatementMatches,
  AbilityStatementStatus,
} from './AbilityStatement';
import AbilityPolicy, { AbilityRuleCompareMethod } from './AbilityPolicy';
import AbilityRule from './AbilityRule';

export type PolicyID =
  | 'USER_MUST_BE_A_CREATOR_OR_RESPONSIBLE_PROGRAMMER'
  | 'BUGTRACKER_CAN_CHANGE_TO_ACCEPTED_STATUS'
  | 'BUGTRACKER_CAN_CHANGE_TO_ACCEPTED_STATUS_FROM_UNKNOWN';

export type AbilityStatementConfig = {
  readonly name: string;
  readonly effect: AbilityStatementStatus;
  readonly matches: AbilityStatementMatches;
};

export type AbilityPolicyConfig = {
  readonly policyID: PolicyID;
  readonly policyName: string;
  readonly policyDescription?: string;
  readonly target: string;
  readonly ruleCompareMethod: AbilityRuleCompareMethod;
  readonly rules: AbilityStatementConfig[][];
};

class AbilityService {
  public static isPermissionDeny(permission: AbilityStatementStatus) {
    return permission === 'deny';
  }

  public static isPermissionPermit(permission: AbilityStatementStatus) {
    return permission === 'permit';
  }

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
  public createPolicy(...args: ConstructorParameters<typeof AbilityPolicy>): AbilityPolicy {
    return new AbilityPolicy(...args);
  }

  /**
   * Parse the config JSON format to Policy class instance
   */
  public parsePolicyConfig(config: AbilityPolicyConfig): AbilityPolicy {
    const { policyName, rules, ruleCompareMethod, target } = config;

    const ruless = rules.map(statements => {
      return new AbilityRule(
        statements.map(statementConfig => {
          const { name, effect, matches } = statementConfig;

          return new AbilityStatement(name, matches, effect);
        }),
      );
    });

    return this.createPolicy(policyName).addRules(ruless, ruleCompareMethod).setTarget(target);
  }

  public async loadPolicies() {
    const policyConfig: AbilityPolicyConfig[] = [
      {
        policyID: 'BUGTRACKER_CAN_CHANGE_TO_ACCEPTED_STATUS',
        policyName: 'Bugtracker. Возможность сменить статус на «accepted»',
        target: 'BugtrackerTaskStatus',
        ruleCompareMethod: 'or',
        rules: [
          [
            {
              name: 'Разрешено, если пользователь назначен как ответственный',
              effect: 'permit',
              matches: ['subject.user.id', '=', 'resource.task.responsible'],
            },
            {
              name: 'Разрешено, если предыдущий статус - «unknown»',
              effect: 'permit',
              matches: ['subject.user.id', '=', 'resource.task.responsible'],
            },
          ],
          [
            {
              name: 'Разрешено, если пользователь является создателем задачи',
              effect: 'permit',
              matches: ['subject.user.id', '=', 'resource.task.creator'],
            },
          ],
          [
            {
              name: 'Разрешено, если пользователь имеет роль администратора',
              effect: 'permit',
              matches: ['subject.account.roles', 'in', 'administrator'],
            },
          ],
        ],
      },
    ];

    const policyRecords: Record<PolicyID, AbilityPolicy> = {} as Record<PolicyID, AbilityPolicy>;

    Object.entries(policyConfig).map(([_key, config]) => {
      policyRecords[config.policyID] = this.parsePolicyConfig(config);
    });

    return policyRecords;
  }
}

export default AbilityService;
