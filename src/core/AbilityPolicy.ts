import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import AbilityMatch from './AbilityMatch';
import AbilityCompare, { AbilityCompareCodeType } from './AbilityCompare';
import AbilityPolicyEffect, { AbilityPolicyEffectCodeType } from './AbilityPolicyEffect';
import { AbilityExplain, AbilityExplainPolicy } from './AbilityExplain';
import { AbilityError } from './AbilityError';
import { ResourceObject } from './AbilityParser';

export type AbilityPolicyConfig = {
  readonly permission: string;
  readonly effect: AbilityPolicyEffectCodeType;
  readonly compareMethod: AbilityCompareCodeType;
  readonly ruleSet: readonly AbilityRuleSetConfig[];
  readonly id: string;
  readonly name: string;
};

export type AbilityPolicyConstructorProps = {
  id: string;
  name: string;
  permission: string;
  effect: AbilityPolicyEffect;
  compareMethod?: AbilityCompare;
};

export class AbilityPolicy<
  Resource extends ResourceObject = Record<string, unknown>,
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
   * will select only those from all passed policies that fall under the specified permission key.
   */
  public permission: string;

  public constructor(params: AbilityPolicyConstructorProps) {
    const { name, id, permission, effect, compareMethod = AbilityCompare.and } = params;
    this.name = name;
    this.id = id;
    this.permission = permission;
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
   * Add rule set to the policy
   * @param ruleSets - The array of rule set to add
   */
  public addRuleSets(ruleSets: readonly AbilityRuleSet<Resource, Environment>[]): this {
    for (const ruleSet of ruleSets) {
      this.ruleSet.push(ruleSet);
    }

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

  public copyWith(
    props: Partial<{
      id: string;
      name: string;
      permission: string;
      effect: AbilityPolicyEffect;
      compareMethod: AbilityCompare;
      ruleSet: AbilityRuleSet<Resource, Environment>[];
    }>,
  ): AbilityPolicy<Resource, Environment> {
    const policy = new AbilityPolicy<Resource, Environment>({
      id: props.id ?? this.id,
      name: props.name ?? this.name,
      permission: props.permission ?? this.permission,
      effect: props.effect ?? this.effect,
      compareMethod: props.compareMethod ?? this.compareMethod,
    });

    const nextRuleSet = props.ruleSet ?? this.ruleSet;

    for (const rule of nextRuleSet) {
      policy.addRuleSet(rule);
    }

    return policy;
  }
}

export default AbilityPolicy;
