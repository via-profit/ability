import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import { AbilityMatch, AbilityMatchType } from './AbilityMatch';
import { AbilityCompare, AbilityCompareType } from './AbilityCompare';
import { AbilityPolicyEffectType } from './AbilityPolicyEffect';
import { AbilityExplain, AbilityExplainPolicy } from './AbilityExplain';
import { AbilityError } from './AbilityError';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import AbilityRule from './AbilityRule';
import { AbilityHash } from './AbilityHash';

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

export type AbilityPolicySnapshot = {
  readonly state: AbilityMatchType;
  readonly ruleSet: readonly {
    readonly state: AbilityMatchType;
    readonly rules: readonly AbilityMatchType[];
  }[];
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

  public description?: string | null;

  /**
   * Running the `enforce` or `resolve` method
   * will select only those from all passed policies that fall under the specified permission key.
   */
  public permission: string;

  public priority: number = -1;

  public disabled: boolean;

  public tags: readonly TTag[];

  private readonly _id: string | null;
  private _autoId: string | null = null;
  private _name: string | null;

  public constructor(params: AbilityPolicyConstructorProps<TTag>) {
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
    this.permission = permission;
    this.description = description;
    this.effect = effect;
    this.compareMethod = compareMethod;
    this.priority = typeof priority === 'number' ? priority : -1;
    this.disabled = typeof disabled === 'boolean' ? disabled : false;
    this.tags = (tags || []) as readonly TTag[];
    this.matchState = this.disabled ? AbilityMatch.disabled : this.matchState;
    this._id = id || null;
    this._name = name || null;
  }

  /**
   * Policy ID.
   * If it was not passed explicitly, it is generated from the policy content
   */
  public get id(): string {
    if (this._id) {
      return this._id;
    }

    if (!this._autoId) {
      this._autoId = `p_${this.hash().slice(0, 10)}`;
    }

    return this._autoId;
  }

  /**
   * Policy name
   */
  public get name(): string {
    return this._name || this.id;
  }

  public set name(value: string | null) {
    this._name = value;
  }

  /**
   * Resets the evaluation state of the policy, its rule sets and rules
   */
  public reset(): void {
    this.matchState = this.disabled ? AbilityMatch.disabled : AbilityMatch.pending;
    for (const ruleSet of this.ruleSet) {
      ruleSet.reset();
    }
  }

  /**
   * Returns a snapshot of the evaluation state of the policy, its rule sets and rules.
   * Used to explain the result after the policies have been re-checked
   */
  public snapshot(): AbilityPolicySnapshot {
    return {
      state: this.matchState,
      ruleSet: this.ruleSet.map(ruleSet => ({
        state: ruleSet.state,
        rules: ruleSet.rules.map(rule => rule.state),
      })),
    };
  }

  /**
   * Add rule set to the policy
   * @param ruleSet - The rule set to add
   */
  public addRuleSet(ruleSet: AbilityRuleSet<R, E>): this {
    this.ruleSet.push(ruleSet);
    this._autoId = null;

    return this;
  }

  /**
   * Add rule set to the policy
   * @param ruleSets - The array of rule set to add
   */
  public addRuleSets(ruleSets: readonly AbilityRuleSet<R, E>[]): this {
    for (const ruleSet of ruleSets) {
      this.addRuleSet(ruleSet);
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
    this.reset();

    if (this.disabled) {
      return this.matchState;
    }

    this.matchState = AbilityMatch.mismatch;

    const normalGroups = this.ruleSet.filter(g => !g.isExcept);
    const exceptGroups = this.ruleSet.filter(g => g.isExcept);
    const normalStates: AbilityMatchType[] = [];

    for (const group of normalGroups) {
      if (group.disabled) {
        continue;
      }

      const state = group.check(resource, environment);

      // the group has no active rules
      if (AbilityMatch.disabled === state) {
        continue;
      }

      normalStates.push(state);

      if (AbilityCompare.and === this.compareMethod && AbilityMatch.mismatch === state) {
        return this.matchState;
      }

      if (AbilityCompare.or === this.compareMethod && AbilityMatch.match === state) {
        // break to check except-rule sets
        break;
      }
    }

    // A policy without active conditions never matches
    if (!normalStates.length) {
      return this.matchState;
    }

    const normalMatch =
      AbilityCompare.and === this.compareMethod
        ? normalStates.every(s => AbilityMatch.match === s)
        : normalStates.some(s => AbilityMatch.match === s);

    if (!normalMatch) {
      return this.matchState;
    }

    // except-rule sets
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

    this.matchState = AbilityMatch.match;
    return this.matchState;
  }

  public explain(): AbilityExplain {
    if (this.matchState === AbilityMatch.pending) {
      throw new AbilityError('First, run the check method, then explain');
    }

    return new AbilityExplainPolicy(this as unknown as AbilityPolicy);
  }

  public copyWith(
    props: Partial<{
      id: string | null;
      name: string | null;
      description?: string | null;
      priority: number;
      permission: string;
      effect: AbilityPolicyEffectType;
      compareMethod: AbilityCompareType;
      ruleSet: AbilityRuleSet<R, E>[];
      disabled: boolean;
      tags: readonly TTag[];
    }>,
  ): AbilityPolicy<R, E, TTag> {
    const policy = new AbilityPolicy<R, E, TTag>({
      id: props.id !== undefined ? props.id : this._id,
      name: props.name !== undefined ? props.name : this._name,
      description: props.description !== undefined ? props.description : this.description,
      priority: typeof props.priority !== 'undefined' ? props.priority : this.priority,
      permission: props.permission ?? this.permission,
      effect: props.effect ?? this.effect,
      compareMethod: props.compareMethod ?? this.compareMethod,
      disabled: props.disabled ?? this.disabled,
      tags: props.tags ?? this.tags,
    });

    const nextRuleSet = props.ruleSet ?? this.ruleSet;

    for (const ruleSet of nextRuleSet) {
      policy.addRuleSet(ruleSet);
    }

    return policy;
  }

  public hash(): string {
    const parts: string[] = [
      `permission:${this.permission}`,
      `effect:${this.effect}`,
      `compareMethod:${this.compareMethod}`,
      `priority:${this.priority}`,
      `disabled:${this.disabled}`,
    ];

    if (this.tags && this.tags.length > 0) {
      parts.push(`tags:${[...this.tags].sort().join(',')}`);
    }

    if (this.ruleSet && this.ruleSet.length > 0) {
      const ruleHashes = this.ruleSet.map(r => r.hash());
      parts.push(`rules:${ruleHashes.sort().join('|')}`);
    }

    const str = parts.join(';');

    return AbilityHash.sha1(str);
  }
}

export default AbilityPolicy;
