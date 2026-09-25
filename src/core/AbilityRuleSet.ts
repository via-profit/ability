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
  readonly disabled?: boolean;
  /**
   * The group is an except block
   */
  readonly isExcept?: boolean;
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
    return `AbilityRuleSet: ${this.name} compareMethod: ${this.compareMethod}, rules: ${this.rules.map(rule => rule.toString()).join('\n')}`;
  }

  public copyWith(
    props: Partial<{
      id: string | null;
      name: string | null;
      description: string | null;
      compareMethod: AbilityCompareType;
      rules: AbilityRule<R, E>[];
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
