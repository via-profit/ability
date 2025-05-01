import AbilityRule from './AbilityRule';
import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import AbilityMatch from './AbilityMatch';
import AbilityCompare from './AbilityCompare';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityParser from './AbilityParser';

export type AbilityPolicyConfig = {
  readonly action: string;
  readonly effect: number;
  readonly compareMethod: number;
  readonly ruleSet: AbilityRuleSetConfig[];
  readonly id: string;
  readonly name: string;
};

export class AbilityPolicy<Resources extends object = object> {
  public matchState: AbilityMatch = AbilityMatch.PENDING;
  /**
   * List of rules
   */
  public ruleSet: AbilityRuleSet[] = [];

  /**
   * Policy effect
   */
  public effect: AbilityPolicyEffect;

  /**
   * Rules compare method.\
   * For the «and» method the rule will be permitted if all\
   * rules will be returns «permit» status and for the «or» - if\
   * one of the rules returns as «permit»
   */
  public compareMethod: AbilityCompare = AbilityCompare.AND;

  /**
   * Policy name
   */
  public name: string;

  /**
   * Policy ID
   */
  public id: string;

  /**
   * Soon
   */
  public action: string;

  public constructor(params: {
    id: string;
    name: string;
    action: string;
    effect: AbilityPolicyEffect;
  }) {
    const { name, id, action, effect } = params;
    this.name = name;
    this.id = id;
    this.action = action;
    this.effect = effect;
  }


  /**
   * Add rule set to the policy
   * @param ruleSet - The rule set to add
   */
  public addRuleSet(ruleSet: AbilityRuleSet): this {
    this.ruleSet.push(ruleSet);

    return this;
  }


  /**
   * Check if the policy is matched
   * @param resource - The resource to check
   */
  public check(resource: Resources): AbilityMatch {
    this.matchState = AbilityMatch.MISMATCH;

    /**
     * If policy contain a rules
     */
    if (this.ruleSet.length) {
      const ruleCheckStates: AbilityMatch[] = [];
      this.ruleSet.forEach(rule => {
        const ruleCheckState = rule.check(resource);
        ruleCheckStates.push(ruleCheckState);
      });

      if (AbilityCompare.AND.isEqual(this.compareMethod)) {
        if (ruleCheckStates.every(ruleState => AbilityMatch.MATCH.isEqual(ruleState))) {
          this.matchState = AbilityMatch.MATCH;
        }
      }

      if (AbilityCompare.OR.isEqual(this.compareMethod)) {
        if (ruleCheckStates.some(ruleState => AbilityMatch.MATCH.isEqual(ruleState))) {
          this.matchState = AbilityMatch.MATCH;
        }
      }
    }

    return this.matchState;
  }




  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse<Resources extends object = object>(
    configOrJson: AbilityPolicyConfig | string,
  ): AbilityPolicy<Resources> {


    const config = AbilityParser.prepareAndValidateConfig<AbilityPolicyConfig>(configOrJson, [
      ['id', 'string', true],
      ['name', 'string', true],
      ['action', 'string', true],
      ['effect', 'number', true],
      ['compareMethod', 'number', true],
      ['ruleSet', 'array', true],
    ]);

    const { id, name, ruleSet, compareMethod, action, effect } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<Resources>({
      name,
      id,
      action,
      effect: new AbilityPolicyEffect(effect),
    });

    policy.compareMethod = new AbilityCompare(compareMethod);

    ruleSet.forEach(ruleSetConfig => {
      policy.addRuleSet(AbilityRuleSet.parse(ruleSetConfig));
    });

    return policy;
  }

  public export(): AbilityPolicyConfig {
    return {
      id: this.id.toString(),
      name: this.name.toString(),
      compareMethod: this.compareMethod.code,
      ruleSet: this.ruleSet.map(rule => rule.export()),
      action: this.action,
      effect: this.effect.code,
    };
  }
}

export default AbilityPolicy;
