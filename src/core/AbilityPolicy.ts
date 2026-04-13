import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import { AbilityMatch, AbilityMatchType } from './AbilityMatch';
import { AbilityCompare, AbilityCompareType } from './AbilityCompare';
import { AbilityPolicyEffectType } from './AbilityPolicyEffect';
import { AbilityExplain, AbilityExplainPolicy } from './AbilityExplain';
import { AbilityError } from './AbilityError';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import AbilityRule from '~/core/AbilityRule';

export type AbilityPolicyConfig<TTag extends string = string> = {
  readonly permission: string;
  readonly effect: AbilityPolicyEffectType;
  readonly compareMethod: AbilityCompareType;
  readonly ruleSet: readonly AbilityRuleSetConfig[];
  readonly id: string;
  readonly name: string;
  readonly description?: string | null;
  readonly priority: number;
  readonly disabled?: boolean;
  readonly tags?: readonly TTag[];
};

export type AbilityPolicyConstructorProps<TTag extends string = string> = {
  id: string | null;
  name: string | null;
  description?: string | null;
  permission: string;
  effect: AbilityPolicyEffectType;
  compareMethod?: AbilityCompareType;
  priority?: number | null;
  disabled?: boolean;
  tags?: readonly TTag[];
};

export class AbilityPolicy<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
  TTag extends string = string,
> {
  public matchState: AbilityMatchType = AbilityMatch.pending;
  /**
   * List of rules
   */
  public ruleSet: AbilityRuleSet<R, E>[] = [];

  /**
   * Policy effect
   */
  public effect: AbilityPolicyEffectType;

  /**
   * Rules compare method.\
   * For the «and» method the rule will be permitted if all\
   * rules will be returns «permit» status and for the «or» - if\
   * one of the rules returns as «permit»
   */
  public compareMethod: AbilityCompareType = AbilityCompare.and;

  /**
   * Policy name
   */
  public name: string;

  public description?: string | null;

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
      description,
      id,
      permission,
      effect,
      compareMethod = AbilityCompare.and,
      priority,
      disabled,
      tags,
    } = params;
    this.id = id || `policy:${effect}:${permission}`;
    this.name = name || this.id;
    this.permission = permission;
    this.description = description;
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
   * Extract all rules of all ruleSets of this policy
   */
  public extractRules(): readonly AbilityRule[] {
    const rules: AbilityRule[] = [];
    for (const ruleSet of this.ruleSet) {
      for (const rule of ruleSet.rules) {
        rules.push(rule);
      }
    }

    return rules;
  }

  /**
   * Check if the policy is matched
   * @param resource - The resource to check
   * @param environment - The user environment object
   */
  public check(resource: R, environment?: E): AbilityMatchType {
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
    const normalStates: AbilityMatchType[] = [];

    for (const group of normalGroups) {
      if (group.disabled) {
        continue;
      }

      const state = group.check(resource, environment);
      normalStates.push(state);

      if (AbilityCompare.and === this.compareMethod && AbilityMatch.mismatch === state) {
        this.matchState = AbilityMatch.mismatch;
        return this.matchState;
      }

      if (AbilityCompare.or === this.compareMethod && AbilityMatch.match === state) {
        this.matchState = AbilityMatch.match;
        // break to check except-rule sets
        break;
      }
    }

    // 3. Simple rule sets
    let normalMatch = false;

    if (AbilityCompare.and === this.compareMethod) {
      normalMatch = normalStates.every(s => AbilityMatch.match === s);
    } else {
      normalMatch = normalStates.some(s => AbilityMatch.match === s);
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

      if (AbilityMatch.match === state) {
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
      description?: string | null;
      priority: number;
      permission: string;
      effect: AbilityPolicyEffectType;
      compareMethod: AbilityCompareType;
      ruleSet: AbilityRuleSet<R, E>[];
    }>,
  ): AbilityPolicy<R, E> {
    const policy = new AbilityPolicy<R, E>({
      id: props.id ?? this.id,
      name: props.name ?? this.name,
      description: props.description ?? this.description,
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
