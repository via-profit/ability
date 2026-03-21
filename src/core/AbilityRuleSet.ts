import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import AbilityCompare, { AbilityCompareCodeType } from './AbilityCompare';
import AbilityMatch from './AbilityMatch';
import { ResourceObject } from './AbilityParser';

export type AbilityRuleSetConfig = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly compareMethod: AbilityCompareCodeType;
  readonly rules: readonly AbilityRuleConfig[];
};

export type AbilityRuleSetConstructorProps = {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly compareMethod: AbilityCompare;
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

  public constructor(params: AbilityRuleSetConstructorProps) {
    const { name, id, compareMethod } = params;

    this.name = name || 'No name';
    this.id = id || this.name;
    this.compareMethod = compareMethod;
  }

  public addRule(rule: AbilityRule<Resources, Environment>): this {
    this.rules.push(rule);

    return this;
  }

  public addRules(rules: AbilityRule<Resources, Environment>[]): this {
    rules.forEach(rule => this.addRule(rule));

    return this;
  }

  public async check(resources: Resources | null, environment?: Environment): Promise<AbilityMatch> {
    this.state = AbilityMatch.mismatch;

    if (!this.rules.length) {
      return this.state;
    }

    const ruleCheckStates: AbilityMatch[] = [];

    for (const rule of this.rules) {
      const state = await rule.check(resources, environment);
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

  /**
   * Parse the config JSON format to Group class instance
   */
  public static fromJSON<Resource extends ResourceObject = Record<string, unknown>, Environment = unknown,>(
    config: AbilityRuleSetConfig,
  ): AbilityRuleSet<Resource, Environment> {
    const { id, name, rules, compareMethod } = config;

    const ruleSet = new AbilityRuleSet<Resource, Environment>({
      compareMethod: new AbilityCompare(compareMethod),
      name,
      id,
    });

    // Adding rules if exists
    if (rules && rules.length > 0) {
      const abilityRules = rules.map(ruleConfig => AbilityRule.fromJSON<Resource, Environment>(ruleConfig));

      ruleSet.addRules(abilityRules);
    }

    return ruleSet;
  }

  public toJSON(): AbilityRuleSetConfig {
    return {
      id: this.id.toString(),
      name: this.name.toString(),
      compareMethod: this.compareMethod.code.toString() as AbilityRuleSetConfig['compareMethod'],
      rules: this.rules.map(rule => rule.toJSON()),
    };
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
