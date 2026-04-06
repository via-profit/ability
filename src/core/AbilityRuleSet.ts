import AbilityRule, { AbilityRuleConfig } from '~/core/AbilityRule';
import AbilityCompare, { AbilityCompareCodeType } from '~/core/AbilityCompare';
import AbilityMatch from '~/core/AbilityMatch';
import { ResourceObject } from '~/core/AbilityTypeGenerator';

export type AbilityRuleSetConfig = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly compareMethod: AbilityCompareCodeType;
  readonly rules: readonly AbilityRuleConfig[];
  readonly disabled?: boolean;
};

export type AbilityRuleSetConstructorProps = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly compareMethod: AbilityCompare;
  readonly isExcept?: boolean;
  readonly disabled?: boolean;
};

export class AbilityRuleSet<
  Resources extends ResourceObject = Record<string, unknown>,
  Environment = unknown,
> {
  public state: AbilityMatch = AbilityMatch.pending;
  /**
   * List of rules
   */
  public rules: AbilityRule<Resources, Environment>[] = [];

  /**
   * Rules compare method.\
   * For the «and» method the rule will be permitted if all\
   * rules will be returns «permit» status and for the «or» - if\
   * one of the rules returns as «permit»
   */
  public compareMethod: AbilityCompare = AbilityCompare.and;

  /**
   * Group name
   */
  public name: string;

  /**
   * Group ID
   */
  public id: string;

  readonly isExcept?: boolean = false;

  public disabled: boolean;

  public constructor(params: AbilityRuleSetConstructorProps) {
    const { name, id, compareMethod, isExcept, disabled } = params;

    this.name = name || '';
    this.id = id || this.name;
    this.compareMethod = compareMethod;
    this.isExcept = isExcept;
    this.disabled = typeof disabled === 'boolean' ? disabled : false;
  }

  public addRule(rule: AbilityRule<Resources, Environment>): this {
    this.rules.push(rule);

    return this;
  }

  public addRules(rules: AbilityRule<Resources, Environment>[]): this {
    rules.forEach(rule => this.addRule(rule));

    return this;
  }

  public check(resources: Resources | null, environment?: Environment): AbilityMatch {
    this.state = AbilityMatch.mismatch;

    if (this.disabled) {
      return AbilityMatch.pending;
    }

    if (!this.rules.length) {
      return this.state;
    }

    const ruleCheckStates: AbilityMatch[] = [];

    for (const rule of this.rules) {
      if (rule.disabled) {
        continue;
      }

      const state = rule.check(resources, environment);
      ruleCheckStates.push(state);

      if (AbilityCompare.and.isEqual(this.compareMethod) && AbilityMatch.mismatch.isEqual(state)) {
        return this.state; // mismatch
      }

      if (AbilityCompare.or.isEqual(this.compareMethod) && AbilityMatch.match.isEqual(state)) {
        this.state = AbilityMatch.match;
        return this.state;
      }
    }

    if (AbilityCompare.and.isEqual(this.compareMethod)) {
      if (ruleCheckStates.every(s => AbilityMatch.match.isEqual(s))) {
        this.state = AbilityMatch.match;
      }
    }

    if (AbilityCompare.or.isEqual(this.compareMethod)) {
      if (ruleCheckStates.some(s => AbilityMatch.match.isEqual(s))) {
        this.state = AbilityMatch.match;
      }
    }

    return this.state;
  }

  public toString(): string {
    return `AbilityRuleSet: ${this.name} compareMethod: ${this.compareMethod.code}, rules: ${this.rules.map(rule => rule.toString()).join('\n')}`;
  }

  public copyWith(
    props: Partial<{
      id: string | null;
      name: string | null;
      compareMethod: AbilityCompare;
      rules: AbilityRule<Resources, Environment>[];
    }>,
  ): AbilityRuleSet<Resources, Environment> {
    const next = new AbilityRuleSet<Resources, Environment>({
      id: props.id ?? this.id,
      name: props.name ?? this.name,
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
