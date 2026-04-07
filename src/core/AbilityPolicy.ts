import AbilityRuleSet, { AbilityRuleSetConfig } from '~/core/AbilityRuleSet';
import AbilityMatch from '~/core/AbilityMatch';
import AbilityCompare, { AbilityCompareCodeType } from '~/core/AbilityCompare';
import AbilityPolicyEffect, { AbilityPolicyEffectCodeType } from '~/core/AbilityPolicyEffect';
import { AbilityExplain, AbilityExplainPolicy } from '~/core/AbilityExplain';
import { AbilityError } from '~/core/AbilityError';
import { EnvironmentObject, ResourceObject } from '~/core/AbilityTypeGenerator';
import { read } from 'node:fs';

export type AbilityPolicyConfig<TTag extends string = string> = {
  readonly permission: string;
  readonly effect: AbilityPolicyEffectCodeType;
  readonly compareMethod: AbilityCompareCodeType;
  readonly ruleSet: readonly AbilityRuleSetConfig[];
  readonly id: string;
  readonly name: string;
  readonly priority: number;
  readonly disabled?: boolean;
  readonly tags?: readonly TTag[];
};

export type AbilityPolicyConstructorProps<TTag extends string = string> = {
  id: string;
  name: string;
  permission: string;
  effect: AbilityPolicyEffect;
  compareMethod?: AbilityCompare;
  priority?: number | null;
  disabled?: boolean;
  tags?: readonly TTag[];
};

export class AbilityPolicy<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
  TTag extends string = string,
> {
  public matchState: AbilityMatch = AbilityMatch.pending;
  /**
   * List of rules
   */
  public ruleSet: AbilityRuleSet<R, E>[] = [];

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

  public priority: number = -1;

  public disabled: boolean;

  public tags: readonly TTag[];

  public constructor(params: AbilityPolicyConstructorProps) {
    const {
      name,
      id,
      permission,
      effect,
      compareMethod = AbilityCompare.and,
      priority,
      disabled,
      tags,
    } = params;
    this.name = name;
    this.id = id;
    this.permission = permission;
    this.effect = effect;
    this.compareMethod = compareMethod;
    this.priority = typeof priority === 'number' ? priority : -1;
    this.disabled = typeof disabled === 'boolean' ? disabled : false;
    this.tags = (tags || []) as readonly TTag[];
  }

  /**
   * Add rule set to the policy
   * @param ruleSet - The rule set to add
   */
  public addRuleSet(ruleSet: AbilityRuleSet<R, E>): this {
    this.ruleSet.push(ruleSet);

    return this;
  }

  /**
   * Add rule set to the policy
   * @param ruleSets - The array of rule set to add
   */
  public addRuleSets(ruleSets: readonly AbilityRuleSet<R, E>[]): this {
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
  public check(resource: R, environment?: E): AbilityMatch {
    this.matchState = AbilityMatch.mismatch;

    if (this.disabled) {
      this.matchState = AbilityMatch.disabled;
      return this.matchState;
    }

    if (!this.ruleSet.length) {
      return this.matchState;
    }

    const normalGroups = this.ruleSet.filter(g => !g.isExcept);
    const exceptGroups = this.ruleSet.filter(g => g.isExcept);
    const normalStates: AbilityMatch[] = [];

    for (const group of normalGroups) {
      if (group.disabled) {
        continue;
      }

      const state = group.check(resource, environment);
      normalStates.push(state);

      if (AbilityCompare.and.isEqual(this.compareMethod) && AbilityMatch.mismatch.isEqual(state)) {
        this.matchState = AbilityMatch.mismatch;
        return this.matchState;
      }

      if (AbilityCompare.or.isEqual(this.compareMethod) && AbilityMatch.match.isEqual(state)) {
        this.matchState = AbilityMatch.match;
        // break to check except-rule sets
        break;
      }
    }

    // 3. Simple rule sets
    let normalMatch = false;

    if (AbilityCompare.and.isEqual(this.compareMethod)) {
      normalMatch = normalStates.every(s => AbilityMatch.match.isEqual(s));
    } else {
      normalMatch = normalStates.some(s => AbilityMatch.match.isEqual(s));
    }

    if (!normalMatch) {
      this.matchState = AbilityMatch.mismatch;
      return this.matchState;
    }

    // 4. except-rule sets
    for (const group of exceptGroups) {
      if (group.disabled) {
        continue;
      }
      const state = group.check(resource, environment);

      if (AbilityMatch.match.isEqual(state)) {
        this.matchState = AbilityMatch.exceptMismatch;
        return this.matchState;
      }
    }

    // 5. match
    this.matchState = AbilityMatch.match;
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
      priority: number;
      permission: string;
      effect: AbilityPolicyEffect;
      compareMethod: AbilityCompare;
      ruleSet: AbilityRuleSet<R, E>[];
    }>,
  ): AbilityPolicy<R, E> {
    const policy = new AbilityPolicy<R, E>({
      id: props.id ?? this.id,
      name: props.name ?? this.name,
      priority: typeof props.priority !== 'undefined' ? props.priority : this.priority,
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
