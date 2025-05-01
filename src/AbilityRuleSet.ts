import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import AbilityCompare from './AbilityCompare';
import AbilityMatch from './AbilityMatch';
import AbilityParser from './AbilityParser';

export type AbilityRuleSetConfig = {
  readonly id: string;
  readonly name: string;
  readonly compareMethod: number;
  readonly rules: AbilityRuleConfig[];
};

export class AbilityRuleSet<Resources extends object = object> {
  public state: AbilityMatch = AbilityMatch.PENDING;
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
  public compareMethod: AbilityCompare = AbilityCompare.AND;

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
    this.compareMethod = new AbilityCompare(compareMethod);
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
    this.state = AbilityMatch.MISMATCH;

    if (!this.rules.length) {
      return this.state;
    }

    const ruleCheckStates = this.rules.reduce<AbilityMatch[]>((collect, rule) => {
      return collect.concat(rule.check(resources));
    }, []);

    if (AbilityCompare.AND.isEqual(this.compareMethod)) {
      if (ruleCheckStates.every(ruleState => AbilityMatch.MATCH.isEqual(ruleState))) {
        this.state = AbilityMatch.MATCH;
      }
    }

    if (AbilityCompare.OR.isEqual(this.compareMethod)) {
      if (ruleCheckStates.some(ruleState => AbilityMatch.MATCH.isEqual(ruleState))) {
        this.state = AbilityMatch.MATCH;
      }
    }

    return this.state;
  }

  /**
   * Parse the config JSON format to Group class instance
   */
  public static parse<Resource extends object = object>(
    configOrJson: AbilityRuleSetConfig | string,
  ): AbilityRuleSet<Resource> {
    const config = AbilityParser.prepareAndValidateConfig<AbilityRuleSetConfig>(configOrJson, [
      ['id', 'string', true],
      ['name', 'string', true],
      ['compareMethod', 'number', true],
      ['rules', 'array', true],
    ]);

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
      compareMethod: this.compareMethod.code,
      rules: this.rules.map(rule => rule.export()),
    };
  }
}

export default AbilityRuleSet;
