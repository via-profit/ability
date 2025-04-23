import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import AbilityCompare from './AbilityCompare';
import AbilityMatch from './AbilityMatch';

export type AbilityRuleSetConfig = {
  readonly id?: string | symbol;
  readonly name: string;
  readonly compareMethod: number;
  readonly rules: AbilityRuleConfig[];
};

export class AbilityRuleSet<Resource = unknown> {
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
  public name: string | symbol;

  /**
   * Group ID
   */
  public id: string | symbol;

  public constructor(params?: { name?: string | symbol; id?: string | symbol }) {
    const { name, id } = params || {};

    this.name = name || Symbol('name');
    this.id = id || Symbol('id');
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

  public check(subject: Resource | null): AbilityMatch {
    this.state = AbilityMatch.MISMATCH;

    if (!this.rules.length) {
      return this.state;
    }

    const ruleCheckStates = this.rules.reduce<AbilityMatch[]>((collect, rule) => {
      return collect.concat(rule.check(subject));
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
  public static parse<Resource = unknown>(
    configOrJson: AbilityRuleSetConfig | string,
  ): AbilityRuleSet<Resource> {
    const { id, name, rules, compareMethod } =
      typeof configOrJson === 'string'
        ? (JSON.parse(configOrJson) as AbilityRuleSetConfig)
        : configOrJson;

    // Create the empty group
    const ruleSet = new AbilityRuleSet<Resource>({
      name,
      id,
    });

    ruleSet.compareMethod = new AbilityCompare(compareMethod);

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
