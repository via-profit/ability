import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import AbilityCompare, { AbilityCompareLiteralType } from './AbilityCompare';
import AbilityMatch from './AbilityMatch';

export type AbilityRuleSetConfig = {
  readonly id: string;
  readonly name: string;
  readonly compareMethod: AbilityCompareLiteralType;
  readonly rules: readonly AbilityRuleConfig[];
};

export class AbilityRuleSet<Resources extends object = object> {
  public state: AbilityMatch = AbilityMatch.pending;
  /**
   * List of rules
   */
  public rules: AbilityRule[] = [];

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

  public constructor(params: Pick<AbilityRuleSetConfig, 'id' | 'name' | 'compareMethod'>) {
    const { name, id, compareMethod } = params;

    this.name = name;
    this.id = id;
    this.compareMethod = AbilityCompare.fromLiteral(compareMethod);
  }

  public addRule(rule: AbilityRule, compareMethod: AbilityCompare): this {
    this.rules.push(rule);
    this.compareMethod = compareMethod;

    return this;
  }

  public addRules(rules: AbilityRule[], compareMethod: AbilityCompare): this {
    rules.forEach(rule => this.addRule(rule, compareMethod));

    return this;
  }

  public check(resources: Resources | null): AbilityMatch {
    this.state = AbilityMatch.mismatch;

    if (!this.rules.length) {
      return this.state;
    }

    const ruleCheckStates = this.rules.reduce<AbilityMatch[]>((collect, rule) => {
      return collect.concat(rule.check(resources));
    }, []);

    if (AbilityCompare.and.isEqual(this.compareMethod)) {
      if (ruleCheckStates.every(ruleState => AbilityMatch.match.isEqual(ruleState))) {
        this.state = AbilityMatch.match;
      }
    }

    if (AbilityCompare.or.isEqual(this.compareMethod)) {
      if (ruleCheckStates.some(ruleState => AbilityMatch.match.isEqual(ruleState))) {
        this.state = AbilityMatch.match;
      }
    }

    return this.state;
  }

  /**
   * Parse the config JSON format to Group class instance
   */
  public static parse<Resource extends object = object>(
    config: AbilityRuleSetConfig,
  ): AbilityRuleSet<Resource> {
    const { id, name, rules, compareMethod } = config;

    const ruleSet = new AbilityRuleSet<Resource>({
      compareMethod,
      name,
      id,
    });

    // Adding rules if exists
    if (rules && rules.length > 0) {
      const abilityRules = rules.map(ruleConfig => AbilityRule.parse(ruleConfig));

      ruleSet.addRules(abilityRules, ruleSet.compareMethod);
    }

    return ruleSet;
  }

  public export(): AbilityRuleSetConfig {
    return {
      id: this.id.toString(),
      name: this.name.toString(),
      compareMethod: this.compareMethod.code.toString() as AbilityRuleSetConfig['compareMethod'],
      rules: this.rules.map(rule => rule.export()),
    };
  }
}

export default AbilityRuleSet;
