import AbilityRule, { AbilityRuleConfig } from './AbilityRule';
import AbilityRuleSet, { AbilityRuleSetConfig } from './AbilityRuleSet';
import AbilityMatch from './AbilityMatch';
import AbilityCompare from './AbilityCompare';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityParser from './AbilityParser';

export type AbilityPolicyConfig = {
  readonly action: string;
  readonly effect: number;
  readonly compareMethod: number;
  readonly ruleSet: (AbilityRuleConfig | AbilityRuleSetConfig)[];
  readonly id?: string;
  readonly name?: string;
};

export class AbilityPolicy<Resource = unknown> {
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
  public name: string | symbol;

  /**
   * Policy ID
   */
  public id: string | symbol;

  /**
   * Soon
   */
  public action: string;

  public constructor(params: {
    action: string;
    effect: AbilityPolicyEffect;
    name?: string | symbol;
    id?: string | symbol;
  }) {
    const { name, id, action, effect } = params;
    this.name = name || Symbol('name');
    this.id = id || Symbol('id');
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
   * Add rule to the policy
   * @param rule - The rule to add
   */
  public addRule(rule: AbilityRule): this {
    this.addRuleSet(
      new AbilityRuleSet({
        name: rule.name,
      }).addRule(rule, AbilityCompare.AND),
    );

    return this;
  }


  /**
   * Check if the policy is matched
   * @param resource - The resource to check
   */
  public check(resource: Resource | null): AbilityMatch {
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
   * Check if the action is contained in another action
   * @param actionA - The first action to check
   * @param actionB - The second action to check
   */
  public static isInActionContain(actionA: string, actionB: string) {
    const actionAArray = String(actionA).split('.');
    const actionBArray = String(actionB).split('.');

    const a = actionAArray.length >= actionBArray.length ? actionAArray : actionBArray;
    const b = actionBArray.length >= actionAArray.length ? actionBArray : actionAArray;

    return a
      .reduce<boolean[]>((acc, chunk, index) => {
        const iterationRes = chunk === b[index] || b[index] === '*' || chunk === '*';

        return acc.concat(iterationRes);
      }, [])
      .every(Boolean);
  }


  /**
   * Parse the config JSON format to Policy class instance
   */
  public static parse<Resource = unknown>(
    configOrJson: AbilityPolicyConfig | string,
  ): AbilityPolicy<Resource> {


    const config = AbilityParser.prepareAndValidateConfig<AbilityPolicyConfig>(configOrJson, [
      ['id', 'string', false],
      ['name', 'string', true],
      ['action', 'string', true],
      ['effect', 'number', true],
      ['compareMethod', 'number', true],
      ['ruleSet', 'array', true],
    ]);

    const { id, name, ruleSet, compareMethod, action, effect } = config;

    // Create the empty policy
    const policy = new AbilityPolicy<Resource>({
      name,
      id,
      action,
      effect: new AbilityPolicyEffect(effect),
    });

    policy.compareMethod = new AbilityCompare(compareMethod);

    ruleSet.forEach(ruleOrRuleSet => {
      // is ruleset
      if ('rules' in ruleOrRuleSet) {
        policy.addRuleSet(AbilityRuleSet.parse(ruleOrRuleSet));
      }

      // is simple rule
      if (!('rules' in ruleOrRuleSet)) {
        policy.addRule(AbilityRule.parse(ruleOrRuleSet));
      }
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
