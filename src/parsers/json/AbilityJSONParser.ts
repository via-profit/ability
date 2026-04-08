import { AbilityCondition } from '../../core/AbilityCondition';
import { AbilityRule, AbilityRuleConfig } from '../../core/AbilityRule';
import { AbilityRuleSet, AbilityRuleSetConfig } from '../../core/AbilityRuleSet';
import { ResourceObject } from '../../core/AbilityTypeGenerator';
import { AbilityPolicyEffect  } from '../../core/AbilityPolicyEffect';
import { AbilityPolicy, AbilityPolicyConfig } from '../../core/AbilityPolicy';

export class AbilityJSONParser {
  /**
   * Parses an array of policy configurations into an array of AbilityPolicy instances.
   * @param configs - Array of policy configurations
   * @returns Array of AbilityPolicy instances
   */
  public static parse<Resource extends ResourceObject>(
    configs: readonly AbilityPolicyConfig[],
  ): AbilityPolicy<Resource>[] {
    return configs.map(config => AbilityJSONParser.parsePolicy<Resource>(config));
  }

  public static parsePolicy<
    Resource extends ResourceObject = Record<string, unknown>,
  >(config: AbilityPolicyConfig): AbilityPolicy<Resource> {
    const { id, name, ruleSet, compareMethod, permission, effect, priority, disabled, tags } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<Resource>({
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
      policy.addRuleSet(AbilityJSONParser.parseRuleSet<Resource>(ruleSetConfig));
    });

    return policy;
  }

  public static parseRule<Resources extends object>(
    config: AbilityRuleConfig,
  ): AbilityRule<Resources> {
    const { id, name, subject, resource, condition, disabled } = config;

    return new AbilityRule<Resources>({
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
  public static parseRuleSet<
    Resource extends ResourceObject = Record<string, unknown>
  >(config: AbilityRuleSetConfig): AbilityRuleSet<Resource> {
    const { id, name, rules, compareMethod, disabled } = config;

    const ruleSet = new AbilityRuleSet<Resource>({
      disabled,
      compareMethod: compareMethod,
      name,
      id,
    });

    // Adding rules if exists
    if (rules && rules.length > 0) {
      const abilityRules = rules.map(ruleConfig =>
       AbilityJSONParser.parseRule(ruleConfig),
      );

      ruleSet.addRules(abilityRules);
    }

    return ruleSet;
  }

  public static ruleToJSON(rule: AbilityRule): AbilityRuleConfig {
    return {
      id: rule.id,
      name: rule.name,
      subject: rule.subject,
      resource: rule.resource,
      condition: rule.condition,
      disabled: rule.disabled,
    };
  }

  public static ruleSetToJSON(ruleSet: AbilityRuleSet): AbilityRuleSetConfig {
    return {
      id: ruleSet.id.toString(),
      name: ruleSet.name.toString(),
      compareMethod: ruleSet.compareMethod,
      rules: ruleSet.rules.map(rule => AbilityJSONParser.ruleToJSON(rule)),
      disabled: ruleSet.disabled,
    };
  }

  public static policyToJSON(policy: AbilityPolicy): AbilityPolicyConfig {
    return {
      id: policy.id.toString(),
      name: policy.name.toString(),
      compareMethod: policy.compareMethod,
      ruleSet: policy.ruleSet.map(ruleSet => AbilityJSONParser.ruleSetToJSON(ruleSet)),
      permission: policy.permission,
      effect: policy.effect,
      priority: policy.priority,
      disabled: policy.disabled,
      tags: policy.tags,
    };
  }

  public static toJSON(policies: readonly AbilityPolicy[]): AbilityPolicyConfig[] {
    return policies.map(policy => AbilityJSONParser.policyToJSON(policy));
  }
}
