import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import { AbilityCompare, AbilityCompareType } from './AbilityCompare';
import { AbilityMatch, AbilityMatchType } from './AbilityMatch';
import { EnvironmentObject, ResourceObject } from './AbilityTypeGenerator';

export type AbilityRuleSetConfig = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly description?: string | null;
  readonly compareMethod: AbilityCompareType;
  readonly rules: readonly AbilityRuleConfig[];
  readonly disabled?: boolean;
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

  /**
   * Group name
   */
  public name: string;
  public description?: string | null;

  /**
   * Group ID
   */
  public id: string;

  readonly isExcept?: boolean = false;

  public disabled: boolean;

  public constructor(params: AbilityRuleSetConstructorProps) {
    const { name, id, compareMethod, isExcept, disabled, description } = params;

    this.name = name || `ruleset:${compareMethod}`;
    this.id = id || this.name;
    this.description = description;
    this.compareMethod = compareMethod;
    this.isExcept = isExcept;
    this.disabled = typeof disabled === 'boolean' ? disabled : false;
    this.state = this.disabled ? AbilityMatch.disabled : this.state;
  }

  public addRule(rule: AbilityRule<R, E>): this {
    this.rules.push(rule);

    return this;
  }

  public addRules(rules: AbilityRule<R, E>[]): this {
    rules.forEach(rule => this.addRule(rule));

    return this;
  }

  public check(resources: R | null, environment?: E): AbilityMatchType {
    this.state = AbilityMatch.mismatch;

    if (this.disabled) {
      this.state = AbilityMatch.disabled;
      return this.state;
    }

    if (!this.rules.length) {
      return this.state;
    }

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
    }>,
  ): AbilityRuleSet<R, E> {
    const next = new AbilityRuleSet<R, E>({
      id: props.id ?? this.id,
      name: props.name ?? this.name,
      description: props.description ?? this.description,
      compareMethod: props.compareMethod ?? this.compareMethod,
    });

    const nextRules = props.rules ?? this.rules;

    for (const rule of nextRules) {
      next.addRule(rule);
    }

    return next;
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
