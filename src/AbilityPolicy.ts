import AbilityRule, { AbilityRuleStatus, AbilityRuleConfig } from './AbilityRule';

/**
 * Compare method.\
 * For the «and» method the entity (policy, rule or rule) will be permitted if all\
 * entities will be returns «permit» status and for the «or» - if\
 * one of the entities returns as «permit»
 */
export type AbilityCompareMethod = 'or' | 'and';

export type AbilityPolicyResult = {
  readonly permission: AbilityRuleStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedPolicies: readonly AbilityPolicy[];
};

export type AbilityPolicyConfig = {
  readonly id?: string;
  readonly name?: string;
  readonly description?: string;
  readonly rulesCompareMethod?: AbilityCompareMethod;
  readonly policiesCompareMethod?: AbilityCompareMethod;
  readonly rules?: AbilityRuleConfig[] | null;
  readonly policies?: AbilityPolicyConfig[] | null;
};

export class AbilityPolicy<Subject = unknown, Resource = unknown, Environment = unknown> {
  /**
   * List of rules
   */
  public rules: AbilityRule[] = [];

  /**
   * Nested policies
   */
  public policies: AbilityPolicy[] = [];

  /**
   * Rules compare method.\
   * For the «and» method the rule will be permitted if all\
   * rules will be returns «permit» status and for the «or» - if\
   * one of the rules returns as «permit»
   */
  public rulesCompareMethod: AbilityCompareMethod = 'and';

  /**
   * Policies compare method.\
   * For the «and» method the policy will be permitted if all\
   * policies will be returns «permit» status and for the «or» - if\
   * one of the policies returns as «permit»
   */
  public policiesCompareMethod: AbilityCompareMethod = 'and';

  /**
   * Policy name
   */
  public name: string | symbol;

  /**
   * Policy description
   */
  public description: string | null = null;

  /**
   * Policy ID
   */
  public id: string | symbol;

  public constructor(
    policyName: string | symbol | undefined,
    policyID: string | symbol | undefined,
    description?: string,
  ) {
    this.name = policyName || Symbol('name');
    this.id = policyID || Symbol('id');
    this.description = typeof description === 'string' ? description : null;
  }

  public addRule(rule: AbilityRule, compareMethod: AbilityCompareMethod = 'and'): this {
    this.rules.push(rule);
    this.rulesCompareMethod = compareMethod;

    return this;
  }

  public addRules(rules: AbilityRule[], compareMethod: AbilityCompareMethod = 'and'): this {
    rules.forEach(rule => this.addRule(rule, compareMethod));

    return this;
  }

  public addPolicy(policy: AbilityPolicy, compareMethod: AbilityCompareMethod = 'and'): this {
    this.policies.push(policy);
    this.policiesCompareMethod = compareMethod;

    return this;
  }

  public addPolicies(policies: AbilityPolicy[], compareMethod: AbilityCompareMethod = 'and'): this {
    policies.forEach(policy => this.addPolicy(policy, compareMethod));

    return this;
  }

  public getName() {
    return this.name;
  }

  public getID() {
    return this.id;
  }

  public getPolicies() {
    return this.policies;
  }

  public getRules() {
    return this.rules;
  }

  public setDescription(description: string): this {
    this.description = description;

    return this;
  }

  public enforce(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ): void | never {
    const { permission, deniedPolicies } = this.check(subject, resource, environment);

    if (permission === 'deny') {
      throw new Error(`Permission denied. ${deniedPolicies[0].getName().toString()}`);
    }
  }

  public isPermit(subject: Subject, resource?: Resource, environment?: Environment): boolean {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'permit';
  }

  public isDeny(subject: Subject, resource?: Resource, environment?: Environment): boolean {
    const { permission } = this.check(subject, resource, environment);

    return permission === 'deny';
  }

  public check(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ): AbilityPolicyResult {
    const deniedRules: AbilityRule[] = [];
    const deniedPolicies: AbilityPolicy[] = [];
    const ruleStatuses: AbilityRuleStatus[] = [];
    const policyStatuses: AbilityRuleStatus[] = [];

    AbilityPolicy.validatePolicy(this);

    this.policies.forEach(policy => {
      const policyResult = policy.check(subject, resource, environment);

      policyStatuses.push(policyResult.permission);

      if (policyResult.permission === 'deny') {
        deniedPolicies.push(policy);
      }

      policyResult.deniedRules.forEach(rule => {
        deniedRules.push(rule);
      });
    });

    this.rules.forEach(rule => {
      const permission = rule.check(subject, resource, environment);

      ruleStatuses.push(permission);

      if (permission === 'deny') {
        deniedRules.push(rule);
        deniedPolicies.push(this);
      }
    });

    let res: AbilityRuleStatus = 'deny';

    if (policyStatuses.length) {
      res = policyStatuses[this.policiesCompareMethod === 'and' ? 'every' : 'some'](
        status => status === 'permit',
      )
        ? 'permit'
        : 'deny';
    }

    if (ruleStatuses.length) {
      res = ruleStatuses[this.rulesCompareMethod === 'and' ? 'every' : 'some'](
        status => status === 'permit',
      )
        ? 'permit'
        : 'deny';
    }

    return {
      permission: res,
      deniedRules,
      deniedPolicies,
    };
  }

  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse<Subject = unknown, Resource = unknown, Environment = unknown>(
    configOrJson: AbilityPolicyConfig | string,
  ): AbilityPolicy<Subject, Resource, Environment> {
    const { id, name, description, rules, policies, rulesCompareMethod, policiesCompareMethod } =
      typeof configOrJson === 'string'
        ? (JSON.parse(configOrJson) as AbilityPolicyConfig)
        : configOrJson;

    // Create the empty policy
    const policy = new AbilityPolicy<Subject, Resource, Environment>(name, id, description);

    if (description) {
      policy.setDescription(description);
    }

    // Adding rules if exists
    if (rules && rules.length > 0) {
      const abilityRules = rules.map(ruleConfig => AbilityRule.parse(ruleConfig));

      policy.addRules(abilityRules, rulesCompareMethod);
    }

    // Adding policies if exixts
    if (policies && policies.length > 0) {
      const nestedPolicies = policies.map(nestedConfig => AbilityPolicy.parse(nestedConfig));

      policy.addPolicies(nestedPolicies, policiesCompareMethod);
    }

    return policy;
  }

  public static validatePolicy(policy: AbilityPolicy): void | never {
    if (policy.policies.length > 0 && policy.rules.length > 0) {
      throw new Error("The policy can't have a policies and rules at the same time");
    }

    if (policy.policies.length === 0 && policy.rules.length === 0) {
      throw new Error('The policy must have a nested policies or rules');
    }
  }
}

export default AbilityPolicy;
