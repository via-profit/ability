import AbilityStatement, { AbilityStatementStatus } from './AbilityStatement';
import AbilityRule, { AbilityRuleConfig } from './AbilityRule';

/**
 * Compare method.\
 * For the «and» method the entity (policy, rule or statement) will be permitted if all\
 * entities will be returns «permit» status and for the «or» - if\
 * one of the entities returns as «permit»
 */
export type AbilityCompareMethod = 'or' | 'and';

export type AbilityPolicyResult = {
  readonly permission: AbilityStatementStatus;
  readonly deniedRules: readonly AbilityRule[];
  readonly deniedStatements: readonly AbilityStatement[];
  readonly deniedPolicies: readonly AbilityPolicy[];
};

export type AbilityPolicyConfig = {
  readonly id: string;
  readonly name: string;
  readonly description?: string;
  readonly rules?: {
    readonly compareMethod: AbilityCompareMethod;
    readonly list: AbilityRuleConfig[];
  } | null;
  readonly policies?: {
    readonly compareMethod: AbilityCompareMethod;
    readonly list: AbilityPolicyConfig[];
  } | null;
};

class AbilityPolicy<Subject = unknown, Resource = unknown, Environment = unknown> {
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
  public id: string;

  public constructor(policyName: string | symbol, policyID: string, description?: string) {
    this.name = policyName;
    this.id = policyID;
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
      throw new Error(
        `Permission denied. ${deniedPolicies.map(policy => policy.getName()).join('. ')}`,
      );
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
    const deniedStatements: AbilityStatement[] = [];
    const deniedPolicies: AbilityPolicy[] = [];
    const ruleStatuses: AbilityStatementStatus[] = [];
    const policyStatuses: AbilityStatementStatus[] = [];

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

      policyResult.deniedStatements.forEach(st => {
        deniedStatements.push(st);
      });
    });

    this.rules.forEach(rule => {
      const { permission, deniedStatements: st } = rule.check(subject, resource, environment);

      ruleStatuses.push(permission);

      if (permission === 'deny') {
        deniedRules.push(rule);
        deniedPolicies.push(this);
      }

      st.forEach(statement => {
        deniedStatements.push(statement);
      });
    });

    let res: AbilityStatementStatus = 'deny';

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
      deniedStatements,
    };
  }

  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse(configOrJson: AbilityPolicyConfig | string): AbilityPolicy {
    const { id, name, description, rules, policies } =
      typeof configOrJson === 'string'
        ? (JSON.parse(configOrJson) as AbilityPolicyConfig)
        : configOrJson;

    // Create the empty policy
    const policy = new AbilityPolicy(name, id, description);

    if (description) {
      policy.setDescription(description);
    }

    // Adding rules if exists
    if (rules?.list && rules.list.length > 0) {
      const abilityRules = [...(rules?.list || [])].map(ruleConfig =>
        AbilityRule.parse(ruleConfig),
      );

      policy.addRules(abilityRules, rules.compareMethod);
    }

    // Adding policies if exixts
    if (policies?.list && policies.list.length > 0) {
      const nestedPolicies = [...(policies?.list || [])].map(nestedConfig =>
        AbilityPolicy.parse(nestedConfig),
      );

      policy.addPolicies(nestedPolicies, policies.compareMethod);
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
