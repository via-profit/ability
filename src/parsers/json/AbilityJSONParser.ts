import AbilityCondition from '../../core/AbilityCondition';
import { AbilityRule, AbilityRuleConfig } from '../../core/AbilityRule';
import { AbilityRuleSet, AbilityRuleSetConfig } from '../../core/AbilityRuleSet';
import { ResourceObject } from '../../core/AbilityTypeGenerator';
import AbilityCompare from '../../core/AbilityCompare';
import AbilityPolicyEffect from '../../core/AbilityPolicyEffect';
import { AbilityPolicy, AbilityPolicyConfig } from '../../core/AbilityPolicy';

export class AbilityJSONParser {
  /**
   * Parses an array of policy configurations into an array of AbilityPolicy instances.
   * @param configs - Array of policy configurations
   * @returns Array of AbilityPolicy instances
   */
  public static parse<Resource extends ResourceObject, Environment = unknown>(
    configs: readonly AbilityPolicyConfig[],
  ): AbilityPolicy<Resource, Environment>[] {
    return configs.map(config => AbilityJSONParser.parsePolicy<Resource, Environment>(config));
  }

  public static parsePolicy<
    Resource extends ResourceObject = Record<string, unknown>,
    Environment = unknown,
  >(config: AbilityPolicyConfig): AbilityPolicy<Resource, Environment> {
    const { id, name, ruleSet, compareMethod, permission, effect } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<Resource, Environment>({
      name,
      id,
      permission: permission,
      effect: new AbilityPolicyEffect(effect),
    });

    policy.compareMethod = new AbilityCompare(compareMethod);

    ruleSet.forEach(ruleSetConfig => {
      policy.addRuleSet(AbilityJSONParser.parseRuleSet<Resource, Environment>(ruleSetConfig));
    });

    return policy;
  }

  public static parseRule<Resources extends object, Environment = unknown>(
    config: AbilityRuleConfig,
  ): AbilityRule<Resources, Environment> {
    const { id, name, subject, resource, condition } = config;

    return new AbilityRule<Resources, Environment>({
      id,
      name,
      subject,
      resource,
      condition: new AbilityCondition(condition),
    });
  }

  /**
   * Parse the config JSON format to Group class instance
   */
  public static parseRuleSet<
    Resource extends ResourceObject = Record<string, unknown>,
    Environment = unknown,
  >(config: AbilityRuleSetConfig): AbilityRuleSet<Resource, Environment> {
    const { id, name, rules, compareMethod } = config;

    const ruleSet = new AbilityRuleSet<Resource, Environment>({
      compareMethod: new AbilityCompare(compareMethod),
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
      condition: rule.condition.code,
    };
  }

  public static ruleSetToJSON(ruleSet: AbilityRuleSet): AbilityRuleSetConfig {
    return {
      id: ruleSet.id.toString(),
      name: ruleSet.name.toString(),
      compareMethod: ruleSet.compareMethod.code.toString() as AbilityRuleSetConfig['compareMethod'],
      rules: ruleSet.rules.map(rule => AbilityJSONParser.ruleToJSON(rule)),
    };
  }

  public static policyToJSON(policy: AbilityPolicy): AbilityPolicyConfig {
    return {
      id: policy.id.toString(),
      name: policy.name.toString(),
      compareMethod: policy.compareMethod.code.toString() as AbilityPolicyConfig['compareMethod'],
      ruleSet: policy.ruleSet.map(ruleSet => AbilityJSONParser.ruleSetToJSON(ruleSet)),
      permission: policy.permission,
      effect: policy.effect.code,
    };
  }

  public static toJSON(policies: readonly AbilityPolicy[]): AbilityPolicyConfig[] {
    return policies.map(policy => AbilityJSONParser.policyToJSON(policy));
  }
}
