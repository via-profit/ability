import AbilityRule, { AbilityRuleConfig, AbilityRuleState } from './AbilityRule';

/**
 * Compare method.\
 * For the «and» method the entity (policy, rule or rule) will be permitted if all\
 * entities will be returns «permit» status and for the «or» - if\
 * one of the entities returns as «permit»
 */
export type AbilityCompareMethod = 'or' | 'and';
export type AbilityPolicyStatus = 'permit' | 'deny';

export type AbilityPolicyResult = {
  readonly permission: AbilityPolicyStatus;
  readonly matchingRules: readonly AbilityRule[];
  // readonly affectedPolicies: readonly AbilityPolicy[];
};

export type AbilityPolicyConfig = {
  readonly influence?: string;
  readonly action?: AbilityPolicyAction;
  readonly effect: AbilityPolicyStatus;
  readonly id?: string;
  readonly name?: string;
  readonly description?: string;
  readonly rulesCompareMethod?: AbilityCompareMethod;
  readonly policiesCompareMethod?: AbilityCompareMethod;
  readonly rules?: AbilityRuleConfig[] | null;
  readonly policies?: AbilityPolicyConfig[] | null;
};

export type AbilityPolicyAction =
  | 'create'
  | 'read'
  | 'update'
  | 'delete'
  | '*';

export class AbilityPolicy<Subject = unknown, Resource = unknown, Environment = unknown> {
  /**
   * List of rules
   */
  public rules: AbilityRule[] = [];

  /**
   * Policy effect
   */
  public effect: AbilityPolicyStatus;

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

  /**
   * Soon
   */
  public influence: string;

  /**
   * Soon
   */
  public action: AbilityPolicyAction;


  public constructor(
    params: {
      influence?: string,
      action?: AbilityPolicyAction,
      effect: AbilityPolicyStatus,
      name?: string | symbol,
      id?: string | symbol,
      description?: string,
    },
  ) {
    const { name, id, description, action, influence, effect } = params;
    this.name = name || Symbol('name');
    this.id = id || Symbol('id');
    this.description = typeof description === 'string' ? description : null;
    this.action = action || '*';
    this.influence = influence || '*';
    this.effect = effect;
  }

  public addRule(rule: AbilityRule, compareMethod: AbilityCompareMethod): this {
    this.rules.push(rule);
    this.rulesCompareMethod = compareMethod;

    return this;
  }

  public addRules(rules: AbilityRule[], compareMethod: AbilityCompareMethod): this {
    rules.forEach(rule => this.addRule(rule, compareMethod));

    return this;
  }

  public addPolicy(policy: AbilityPolicy, compareMethod: AbilityCompareMethod): this {
    this.policies.push(policy);
    this.policiesCompareMethod = compareMethod;

    return this;
  }

  public addPolicies(policies: AbilityPolicy[], compareMethod: AbilityCompareMethod): this {
    policies.forEach(policy => this.addPolicy(policy, compareMethod));

    return this;
  }

  public enforce(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
  ): void | never {
    const { permission } = this.check(subject, resource, environment);

    if (permission === 'deny') {
      throw new Error(`Permission denied. ${this.name.toString()}`);
    }
  }

  // public isPermit(subject: Subject, resource?: Resource, environment?: Environment): boolean {
  //   const { permission } = this.check(subject, resource, environment);
  //
  //   return permission === 'permit';
  // }
  //
  // public isDeny(subject: Subject, resource?: Resource, environment?: Environment): boolean {
  //   const { permission } = this.check(subject, resource, environment);
  //
  //   return permission === 'deny';
  // }

  public check(
    subject: Subject | null,
    resource?: Resource | null,
    environment?: Environment | null,
    options?: {
      influence?: string;
      action?: AbilityPolicyAction | readonly AbilityPolicyAction[];
    },
  ): AbilityPolicyResult {
    const matchingRules: AbilityRule[] = [];
    // const affectedPolicies: AbilityPolicy[] = [];
    // const ruleStates: AbilityRuleState[] = [];
    const influence = options?.influence || '*';
    const action: readonly AbilityPolicyAction[] = options?.action && Array.isArray(options?.action) ? options?.action : options?.action ? [options.action] : ['*'];

    AbilityPolicy.validatePolicy(this);


    this.policies
      // filter by influence and action
      .filter(policy => {
        return (policy.influence === influence || policy.influence === '*') && (action.includes(policy.action) || policy.action === '*');
      })
      // just iterate
      .forEach(policy => {
        const policyResult = policy.check(subject, resource, environment,
          { influence, action },
        );


        // if (policyResult.permission === 'deny') {
        //   affectedPolicies.push(policy);
        // }

        policyResult.matchingRules.forEach(rule => {
          matchingRules.push(rule);
        });
      });

    this.rules.forEach(rule => {
      const ruleCheckResult = rule.check(subject, resource, environment);

      if (ruleCheckResult === 'match') {
        matchingRules.push(rule);
      }
    });


    let permission: AbilityPolicyStatus = this.effect;

    if (matchingRules.length) {
      permission = matchingRules[this.rulesCompareMethod === 'and' ? 'every' : 'some'](
        ({ state }) => state === 'match',
      ) ? this.effect : AbilityPolicy.reverseEffect(this.effect);
    }


    return {
      permission,
      matchingRules,
    };
  }

  public static createRootPolicy(policiesOrRules?: (AbilityPolicy | AbilityRule)[] | undefined) {
    const root = new AbilityPolicy({
      name: 'Root',
      effect: 'permit',
      influence: '*',
      action: '*',
    });

    if (policiesOrRules) {
      if (policiesOrRules[0] instanceof AbilityPolicy) {
        root.addPolicies(policiesOrRules as AbilityPolicy[], 'and');
      }
      if (policiesOrRules[0] instanceof AbilityRule) {
        root.addRules(policiesOrRules as AbilityRule[], 'and');
      }
    }


    return root;
  }

  public static reverseEffect(effect: AbilityPolicyStatus) {
    return effect === 'permit' ? 'deny' : 'permit';
  };

  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse<Subject = unknown, Resource = unknown, Environment = unknown>(
    configOrJson: AbilityPolicyConfig | string,
  ): AbilityPolicy<Subject, Resource, Environment> {
    const {
      id,
      name,
      description,
      rules,
      policies,
      rulesCompareMethod,
      policiesCompareMethod,
      action,
      influence,
      effect,
    } =
      typeof configOrJson === 'string'
        ? (JSON.parse(configOrJson) as AbilityPolicyConfig)
        : configOrJson;

    // Create the empty policy
    const policy = new AbilityPolicy<Subject, Resource, Environment>({
      name,
      id,
      description,
      action,
      influence,
      effect,
    });

    if (policiesCompareMethod) {
      policy.policiesCompareMethod = policiesCompareMethod;
    }

    if (rulesCompareMethod) {
      policy.rulesCompareMethod = rulesCompareMethod;
    }

    if (description) {
      policy.description = description;
    }

    // Adding rules if exists
    if (rules && rules.length > 0) {
      const abilityRules = rules.map(ruleConfig => AbilityRule.parse(ruleConfig));

      policy.addRules(abilityRules, policy.rulesCompareMethod);
    }

    // Adding policies if exist
    if (policies && policies.length > 0) {
      const nestedPolicies = policies.map(nestedConfig => AbilityPolicy.parse(nestedConfig));

      policy.addPolicies(nestedPolicies, policy.policiesCompareMethod);
    }

    return policy;
  }

  public static export(policy: AbilityPolicy): AbilityPolicyConfig {
    return {
      id: policy.id.toString(),
      name: policy.name.toString(),
      rulesCompareMethod: policy.rulesCompareMethod,
      policiesCompareMethod: policy.policiesCompareMethod,
      policies: policy.policies ? policy.policies.map(p => AbilityPolicy.export(p)) : undefined,
      rules: policy.rules ? policy.rules.map(rule => AbilityRule.export(rule)) : undefined,
      action: policy.action,
      influence: policy.influence,
      effect: policy.effect,
    };

  }

  public static validatePolicy(policy: AbilityPolicy): void | never {
    if (policy.policies.length > 0 && policy.rules.length > 0) {
      throw new Error('The policy can\'t have a policies and rules at the same time');
    }

    if (policy.policies.length === 0 && policy.rules.length === 0) {
      throw new Error('The policy must have a nested policies or rules');
    }
  }
}

export default AbilityPolicy;
