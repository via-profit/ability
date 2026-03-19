import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import AbilityMatch from './AbilityMatch';
import AbilityCompare, { AbilityCompareCodeType } from './AbilityCompare';
import AbilityPolicyEffect, { AbilityPolicyEffectCodeType } from './AbilityPolicyEffect';
import { AbilityExplain, AbilityExplainPolicy } from '~/AbilityExplain';
import { AbilityError } from '~/AbilityError';

export type AbilityPolicyConfig = {
  readonly action: string;
  readonly effect: AbilityPolicyEffectCodeType;
  readonly compareMethod: AbilityCompareCodeType;
  readonly ruleSet: readonly AbilityRuleSetConfig[];
  readonly id: string;
  readonly name: string;
};

export type AbilityPolicyConstructorProps = {
  id: string;
  name: string;
  action: string;
  effect: AbilityPolicyEffect;
  compareMethod?: AbilityCompare;
};

export class AbilityPolicy<Resources extends object = object> {
  public matchState: AbilityMatch = AbilityMatch.pending;
  /**
   * List of rules
   */
  public ruleSet: AbilityRuleSet[] = [];

  /**
   * Policy effect
   */
  public effect: AbilityPolicyEffect;

  /**
   * Rules compare method.\
   * For the «and» method the rule will be permitted if all\
   * rules will be returns «permit» status and for the «or» - if\
   * one of the rules returns as «permit»
   */
  public compareMethod: AbilityCompare = AbilityCompare.and;

  /**
   * Policy name
   */
  public name: string;

  /**
   * Policy ID
   */
  public id: string;

  /**
   * Running the `enforce` or `resolve` method
   * will select only those from all passed policies that fall under the specified action.
   */
  public action: string;

  public constructor(params: AbilityPolicyConstructorProps) {
    const { name, id, action, effect, compareMethod = AbilityCompare.and } = params;
    this.name = name;
    this.id = id;
    this.action = action;
    this.effect = effect;
    this.compareMethod = compareMethod;
  }

  /**
   * Add rule set to the policy
   * @param ruleSet - The rule set to add
   */
  public addRuleSet(ruleSet: AbilityRuleSet): this {
    this.ruleSet.push(ruleSet);

    return this;
  }

  /**
   * Check if the policy is matched
   * @param resources - The resource to check
   */
  public check(resources: Resources): AbilityMatch {
    this.matchState = AbilityMatch.mismatch;

    if (!this.ruleSet.length) {
      return this.matchState;
    }

    const rulesetCheckStates = this.ruleSet.reduce<AbilityMatch[]>((collect, ruleSet) => {
      return collect.concat(ruleSet.check(resources));
    }, []);

    if (AbilityCompare.and.isEqual(this.compareMethod)) {
      if (rulesetCheckStates.every(ruleState => AbilityMatch.match.isEqual(ruleState))) {
        this.matchState = AbilityMatch.match;
      }
    }

    if (AbilityCompare.or.isEqual(this.compareMethod)) {
      if (rulesetCheckStates.some(ruleState => AbilityMatch.match.isEqual(ruleState))) {
        this.matchState = AbilityMatch.match;
      }
    }

    return this.matchState;
  }

  public explain(): AbilityExplain {
    if (this.matchState === AbilityMatch.pending) {
      throw new AbilityError('First, run the check method, then explain');
    }

    return new AbilityExplainPolicy(this);
  }

  /**
   * Parses an array of policy configurations into an array of AbilityPolicy instances.
   * @param configs - Array of policy configurations
   * @returns Array of AbilityPolicy instances
   */
  public static parseAll<Resources extends object = object>(
    configs: readonly AbilityPolicyConfig[],
  ): AbilityPolicy<Resources>[] {
    return configs.map(config => this.parse<Resources>(config));
  }

  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse<Resources extends object = object>(
    config: AbilityPolicyConfig,
  ): AbilityPolicy<Resources> {
    const { id, name, ruleSet, compareMethod, action, effect } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<Resources>({
      name,
      id,
      action,
      effect: new AbilityPolicyEffect(effect),
    });

    policy.compareMethod = new AbilityCompare(compareMethod);

    ruleSet.forEach(ruleSetConfig => {
      policy.addRuleSet(AbilityRuleSet.parse(ruleSetConfig));
    });

    return policy;
  }

  public export(): AbilityPolicyConfig {
    return {
      id: this.id.toString(),
      name: this.name.toString(),
      compareMethod: this.compareMethod.code.toString() as AbilityPolicyConfig['compareMethod'],
      ruleSet: this.ruleSet.map(rule => rule.export()),
      action: this.action,
      effect: this.effect.code,
    };
  }
}

export default AbilityPolicy;
