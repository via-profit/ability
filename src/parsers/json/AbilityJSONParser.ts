import { AbilityRule, AbilityRuleConfig } from '../../core/AbilityRule';
import { AbilityRuleSet, AbilityRuleSetConfig } from '../../core/AbilityRuleSet';
import { EnvironmentObject, ResourceObject } from '../../core/AbilityTypeGenerator';
import { AbilityPolicy, AbilityPolicyConfig } from '../../core/AbilityPolicy';

export class AbilityJSONParser {
  /**
   * Parses an array of policy configurations into an array of AbilityPolicy instances.
   * @param configs - Array of policy configurations
   * @returns Array of AbilityPolicy instances
   */
  public static parse<R extends ResourceObject, E extends  EnvironmentObject, T extends string = string>(
    configs: readonly AbilityPolicyConfig[],
  ): AbilityPolicy<R, E, T>[] {
    return configs.map(config => AbilityJSONParser.parsePolicy<R, E, T>(config));
  }

  public static parsePolicy<R extends ResourceObject, E extends  EnvironmentObject, T extends string = string>(
    config: AbilityPolicyConfig,
  ): AbilityPolicy<R, E, T> {
    const { id, name, ruleSet, compareMethod, permission, effect, priority, disabled, tags } =
      config;

    // Create the empty policy
    const policy = new AbilityPolicy<R, E, T>({
      name,
      id,
      permission: permission,
      priority: priority,
      effect: effect,
      disabled,
      tags,
    });

    policy.compareMethod = compareMethod;

    ruleSet.forEach(ruleSetConfig => {
      policy.addRuleSet(AbilityJSONParser.parseRuleSet<R, E>(ruleSetConfig));
    });

    return policy;
  }

  public static parseRule<R extends ResourceObject, E extends  EnvironmentObject>(
    config: AbilityRuleConfig,
  ): AbilityRule<R, E> {
    const { id, name, subject, resource, condition, disabled } = config;

    return new AbilityRule<R, E>({
      id,
      name,
      subject,
      resource,
      disabled,
      condition,
    });
  }

  /**
   * Parse the config JSON format to Group class instance
   */
  public static parseRuleSet<R extends ResourceObject, E extends  EnvironmentObject>(
    config: AbilityRuleSetConfig,
  ): AbilityRuleSet<R, E> {
    const { id, name, rules, compareMethod, disabled } = config;

    const ruleSet = new AbilityRuleSet<R, E>({
      disabled,
      compareMethod: compareMethod,
      name,
      id,
    });

    // Adding rules if exists
    if (rules && rules.length > 0) {
      const abilityRules = rules.map(ruleConfig => AbilityJSONParser.parseRule(ruleConfig));

      ruleSet.addRules(abilityRules);
    }

    return ruleSet;
  }

  public static ruleToJSON(rule: AbilityRule): AbilityRuleConfig {
    return {
      id: rule.id,
      name: rule.name,
      disabled: rule.disabled,
      subject: rule.subject,
      resource: rule.resource,
      condition: rule.condition,
    };
  }

  public static ruleSetToJSON(ruleSet: AbilityRuleSet): AbilityRuleSetConfig {
    return {
      id: ruleSet.id.toString(),
      name: ruleSet.name.toString(),
      disabled: ruleSet.disabled,
      compareMethod: ruleSet.compareMethod,
      rules: ruleSet.rules.map(rule => AbilityJSONParser.ruleToJSON(rule)),
    };
  }

  public static policyToJSON(policy: AbilityPolicy): AbilityPolicyConfig {
    return {
      id: policy.id.toString(),
      name: policy.name.toString(),
      disabled: policy.disabled,
      priority: policy.priority,
      permission: policy.permission,
      effect: policy.effect,
      compareMethod: policy.compareMethod,
      tags: policy.tags,
      ruleSet: policy.ruleSet.map(ruleSet => AbilityJSONParser.ruleSetToJSON(ruleSet)),
    };
  }

  public static toJSON(policies: readonly AbilityPolicy[]): AbilityPolicyConfig[] {
    return policies.map(policy => AbilityJSONParser.policyToJSON(policy));
  }
}
