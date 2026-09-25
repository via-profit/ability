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
    const {
      id,
      name,
      description,
      ruleSet,
      compareMethod,
      permission,
      effect,
      priority,
      disabled,
      tags,
    } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<R, E, T>({
      name,
      id,
      description,
      permission: permission,
      priority: priority,
      effect: effect,
      compareMethod,
      disabled,
      tags: tags as readonly T[] | undefined,
    });

    ruleSet.forEach(ruleSetConfig => {
      policy.addRuleSet(AbilityJSONParser.parseRuleSet<R, E>(ruleSetConfig));
    });

    return policy;
  }

  public static parseRule<R extends ResourceObject, E extends  EnvironmentObject>(
    config: AbilityRuleConfig,
  ): AbilityRule<R, E> {
    const { id, name, description, subject, resource, resourceType, condition, disabled } = config;

    return new AbilityRule<R, E>({
      id,
      name,
      description,
      subject,
      resource,
      resourceType,
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
    const { id, name, description, rules, compareMethod, disabled, isExcept } = config;

    const ruleSet = new AbilityRuleSet<R, E>({
      disabled,
      compareMethod: compareMethod,
      name,
      id,
      description,
      isExcept,
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
      ...(rule.description ? { description: rule.description } : {}),
      disabled: rule.disabled,
      subject: rule.subject,
      resource: rule.resource,
      resourceType: rule.resourceType,
      condition: rule.condition,
    };
  }

  public static ruleSetToJSON(ruleSet: AbilityRuleSet): AbilityRuleSetConfig {
    return {
      id: ruleSet.id.toString(),
      name: ruleSet.name.toString(),
      ...(ruleSet.description ? { description: ruleSet.description } : {}),
      disabled: ruleSet.disabled,
      isExcept: ruleSet.isExcept,
      compareMethod: ruleSet.compareMethod,
      rules: ruleSet.rules.map(rule => AbilityJSONParser.ruleToJSON(rule)),
    };
  }

  public static policyToJSON(policy: AbilityPolicy): AbilityPolicyConfig {
    return {
      id: policy.id.toString(),
      name: policy.name.toString(),
      ...(policy.description ? { description: policy.description } : {}),
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
