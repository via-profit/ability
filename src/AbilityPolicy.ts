import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import AbilityMatch from './AbilityMatch';
import AbilityCompare, { AbilityCompareCodeType } from './AbilityCompare';
import AbilityPolicyEffect, { AbilityPolicyEffectCodeType } from './AbilityPolicyEffect';
import { AbilityExplain, AbilityExplainPolicy } from './AbilityExplain';
import { AbilityError } from './AbilityError';
import { ResourceObject } from './AbilityParser';

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

export class AbilityPolicy<
  Resource extends  ResourceObject = Record<string, unknown>,
  Environment = unknown,
> {
  public matchState: AbilityMatch = AbilityMatch.pending;
  /**
   * List of rules
   */
  public ruleSet: AbilityRuleSet<Resource, Environment>[] = [];

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
  public addRuleSet(ruleSet: AbilityRuleSet<Resource, Environment>): this {
    this.ruleSet.push(ruleSet);

    return this;
  }

  /**
   * Check if the policy is matched
   * @param resource - The resource to check
   * @param environment - The user environment object
   */
  public async check(resource: Resource, environment?: Environment): Promise<AbilityMatch> {
    this.matchState = AbilityMatch.mismatch;

    if (!this.ruleSet.length) {
      return this.matchState;
    }

    const rulesetCheckStates: AbilityMatch[] = [];

    for (const ruleSet of this.ruleSet) {
      const state = await ruleSet.check(resource, environment);
      rulesetCheckStates.push(state);

      if (AbilityCompare.and.isEqual(this.compareMethod) && AbilityMatch.mismatch.isEqual(state)) {
        return this.matchState; // mismatch
      }

      if (AbilityCompare.or.isEqual(this.compareMethod) && AbilityMatch.match.isEqual(state)) {
        this.matchState = AbilityMatch.match;
        return this.matchState;
      }
    }

    if (AbilityCompare.and.isEqual(this.compareMethod)) {
      if (rulesetCheckStates.every(s => AbilityMatch.match.isEqual(s))) {
        this.matchState = AbilityMatch.match;
      }
    }

    if (AbilityCompare.or.isEqual(this.compareMethod)) {
      if (rulesetCheckStates.some(s => AbilityMatch.match.isEqual(s))) {
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
  public static parseAll<
    Resource extends ResourceObject,
    Environment = unknown,
  >(configs: readonly AbilityPolicyConfig[]): AbilityPolicy<Resource, Environment>[] {
    return configs.map(config => this.parse<Resource, Environment>(config));
  }

  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse<
    Resource extends ResourceObject = Record<string, unknown>,
    Environment = unknown,
  >(config: AbilityPolicyConfig): AbilityPolicy<Resource, Environment> {
    const { id, name, ruleSet, compareMethod, action, effect } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<Resource, Environment>({
      name,
      id,
      action,
      effect: new AbilityPolicyEffect(effect),
    });

    policy.compareMethod = new AbilityCompare(compareMethod);

    ruleSet.forEach(ruleSetConfig => {
      policy.addRuleSet(AbilityRuleSet.parse<Resource, Environment>(ruleSetConfig));
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
