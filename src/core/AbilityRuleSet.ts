import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import { AbilityCompare, AbilityCompareType } from './AbilityCompare';
import { AbilityMatch, AbilityMatchType } from './AbilityMatch';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';
import { AbilityHash } from './AbilityHash';

export type AbilityRuleSetConfig = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly description?: string | null;
  readonly compareMethod: AbilityCompareType;
  readonly rules: readonly AbilityRuleConfig[];
  /**
   * Nested groups. Combined with the rules of the group by its compare method
   */
  readonly ruleSets?: readonly AbilityRuleSetConfig[];
  readonly disabled?: boolean;
  /**
   * The group is an except block
   */
  readonly isExcept?: boolean;
};

/**
 * Evaluation state of the group, its rules and nested groups
 */
export type AbilityRuleSetSnapshot = {
  readonly state: AbilityMatchType;
  readonly rules: readonly AbilityMatchType[];
  readonly ruleSets?: readonly AbilityRuleSetSnapshot[];
};

export type AbilityRuleSetConstructorProps = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly description?: string | null;
  readonly compareMethod: AbilityCompareType;
  readonly isExcept?: boolean;
  readonly disabled?: boolean;
};

export class AbilityRuleSet<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
> {
  public state: AbilityMatchType = AbilityMatch.pending;
  /**
   * List of rules
   */
  public rules: AbilityRule<R, E>[] = [];

  /**
   * List of nested groups.\
   * Each nested group is checked as one more condition of this group
   * (after the rules) and combined with them by the group compare method
   */
  public ruleSets: AbilityRuleSet<R, E>[] = [];

  /**
   * Rules compare method.\
   * For the «and» method the rule will be permitted if all\
   * rules will be returns «permit» status and for the «or» - if\
   * one of the rules returns as «permit»
   */
  public compareMethod: AbilityCompareType = AbilityCompare.and;

  public description?: string | null;

  readonly isExcept: boolean = false;

  public disabled: boolean;

  private readonly _id: string | null;
  private _autoId: string | null = null;
  private _name: string | null;

  public constructor(params: AbilityRuleSetConstructorProps) {
    const { name, id, compareMethod, isExcept, disabled, description } = params;

    this.description = description;
    this.compareMethod = compareMethod;
    this.isExcept = isExcept === true;
    this.disabled = typeof disabled === 'boolean' ? disabled : false;
    this.state = this.disabled ? AbilityMatch.disabled : this.state;
    this._id = id || null;
    this._name = name || null;
  }

  /**
   * Group ID.
   * If it was not passed explicitly, it is generated from the group content
   */
  public get id(): string {
    if (this._id) {
      return this._id;
    }

    if (!this._autoId) {
      this._autoId = `g_${this.hash().slice(0, 10)}`;
    }

    return this._autoId;
  }

  /**
   * Group name
   */
  public get name(): string {
    return this._name || this.id;
  }

  public set name(value: string | null) {
    this._name = value;
  }

  /**
   * Resets the evaluation state of the group and its rules
   */
  public reset(): void {
    this.state = this.disabled ? AbilityMatch.disabled : AbilityMatch.pending;
    for (const rule of this.rules) {
      rule.reset();
    }
    for (const ruleSet of this.ruleSets) {
      ruleSet.reset();
    }
  }

  /**
   * Returns a snapshot of the evaluation state of the group, its rules and nested groups
   */
  public snapshot(): AbilityRuleSetSnapshot {
    return {
      state: this.state,
      rules: this.rules.map(rule => rule.state),
      ...(this.ruleSets.length
        ? { ruleSets: this.ruleSets.map(ruleSet => ruleSet.snapshot()) }
        : {}),
    };
  }

  /**
   * Extract all rules of this group and its nested groups
   */
  public extractRules(): AbilityRule<R, E>[] {
    const rules: AbilityRule<R, E>[] = [...this.rules];
    for (const ruleSet of this.ruleSets) {
      rules.push(...ruleSet.extractRules());
    }

    return rules;
  }

  public addRule(rule: AbilityRule<R, E>): this {
    this.rules.push(rule);
    this._autoId = null;

    return this;
  }

  public addRules(rules: AbilityRule<R, E>[]): this {
    rules.forEach(rule => this.addRule(rule));

    return this;
  }

  /**
   * Add nested group
   */
  public addRuleSet(ruleSet: AbilityRuleSet<R, E>): this {
    this.ruleSets.push(ruleSet);
    this._autoId = null;

    return this;
  }

  public addRuleSets(ruleSets: readonly AbilityRuleSet<R, E>[]): this {
    ruleSets.forEach(ruleSet => this.addRuleSet(ruleSet));

    return this;
  }

  public check(resources: R | null, environment?: E): AbilityMatchType {
    this.reset();

    if (this.disabled) {
      return this.state;
    }

    this.state = AbilityMatch.mismatch;

    const ruleCheckStates: AbilityMatchType[] = [];

    for (const rule of this.rules) {
      if (rule.disabled) {
        continue;
      }

      const state = rule.check(resources, environment);
      ruleCheckStates.push(state);

      if (AbilityCompare.and === this.compareMethod && AbilityMatch.mismatch === state) {
        return this.state; // mismatch
      }

      if (AbilityCompare.or === this.compareMethod && AbilityMatch.match === state) {
        this.state = AbilityMatch.match;
        return this.state;
      }
    }

    for (const ruleSet of this.ruleSets) {
      if (ruleSet.disabled) {
        continue;
      }

      const state = ruleSet.check(resources, environment);

      // the nested group has no active rules
      if (AbilityMatch.disabled === state) {
        continue;
      }

      ruleCheckStates.push(state);

      if (AbilityCompare.and === this.compareMethod && AbilityMatch.mismatch === state) {
        return this.state; // mismatch
      }

      if (AbilityCompare.or === this.compareMethod && AbilityMatch.match === state) {
        this.state = AbilityMatch.match;
        return this.state;
      }
    }

    // There are no active rules in the group - the group does not participate in the check
    if (!ruleCheckStates.length) {
      this.state = AbilityMatch.disabled;
      return this.state;
    }

    if (AbilityCompare.and === this.compareMethod) {
      if (ruleCheckStates.every(s => AbilityMatch.match === s)) {
        this.state = AbilityMatch.match;
      }
    }

    if (AbilityCompare.or === this.compareMethod) {
      if (ruleCheckStates.some(s => AbilityMatch.match === s)) {
        this.state = AbilityMatch.match;
      }
    }

    return this.state;
  }

  public toString(): string {
    const ruleSets = this.ruleSets.length
      ? `, ruleSets: ${this.ruleSets.map(ruleSet => ruleSet.toString()).join('\n')}`
      : '';

    return `AbilityRuleSet: ${this.name} compareMethod: ${this.compareMethod}, rules: ${this.rules.map(rule => rule.toString()).join('\n')}${ruleSets}`;
  }

  public copyWith(
    props: Partial<{
      id: string | null;
      name: string | null;
      description: string | null;
      compareMethod: AbilityCompareType;
      rules: AbilityRule<R, E>[];
      ruleSets: AbilityRuleSet<R, E>[];
      isExcept: boolean;
      disabled: boolean;
    }>,
  ): AbilityRuleSet<R, E> {
    const next = new AbilityRuleSet<R, E>({
      id: props.id !== undefined ? props.id : this._id,
      name: props.name !== undefined ? props.name : this._name,
      description: props.description !== undefined ? props.description : this.description,
      compareMethod: props.compareMethod ?? this.compareMethod,
      isExcept: props.isExcept ?? this.isExcept,
      disabled: props.disabled ?? this.disabled,
    });

    const nextRules = props.rules ?? this.rules;

    for (const rule of nextRules) {
      next.addRule(rule);
    }

    for (const ruleSet of props.ruleSets ?? this.ruleSets) {
      next.addRuleSet(ruleSet);
    }

    return next;
  }

  public hash(): string {
    const ruleHashes = this.rules.map(r => r.hash()).sort();
    const parts = [
      `compareMethod:${this.compareMethod}`,
      `isExcept:${this.isExcept}`,
      `disabled:${this.disabled}`,
      `rules:${ruleHashes.join('|')}`,
    ];

    // added only for the nested groups, so the IDs of the flat groups stay the same
    if (this.ruleSets.length) {
      parts.push(
        `ruleSets:${this.ruleSets
          .map(r => r.hash())
          .sort()
          .join('|')}`,
      );
    }

    return AbilityHash.sha1(parts.join(';'));
  }

  static and(rules: AbilityRule[]) {
    return new AbilityRuleSet({
      compareMethod: AbilityCompare.and,
    }).addRules(rules);
  }

  static or(rules: AbilityRule[]) {
    return new AbilityRuleSet({
      compareMethod: AbilityCompare.or,
    }).addRules(rules);
  }
}

export default AbilityRuleSet;
