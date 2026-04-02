import AbilityRule from '~/core/AbilityRule';
import AbilityRuleSet from '~/core/AbilityRuleSet';
import AbilityPolicy from '~/core/AbilityPolicy';
import AbilityMatch from '~/core/AbilityMatch';

export type AbilityExplainConfig = {
  readonly type: AbilityExplainType;
  readonly name: string;
  readonly match: AbilityMatch;
};

export class AbilityExplain {
  readonly type: AbilityExplainType;
  readonly children: AbilityExplain[];
  readonly name: string;
  readonly match: AbilityMatch;

  constructor(config: AbilityExplainConfig, children: AbilityExplain[] = []) {
    this.type = config.type;
    this.children = children;
    this.name = config.name;
    this.match = config.match;
  }

  public toString(indent: number = 0): string {
    const pad = ' '.repeat(indent);
    const mark = this.match.code === AbilityMatch.match.code ? '✓' : '✗';

    let out = `${pad}${mark} ${this.type} «${this.name}» is ${this.match.code}`;

    this.children.forEach(child => {
      out += '\n' + child.toString(indent + 1);
    });

    return out;
  }
}

export class AbilityExplainRule extends AbilityExplain {
  constructor(rule: AbilityRule) {
    super({
      type: 'rule',
      match: rule.state,
      name: rule.name,
    });
  }
}

export class AbilityExplainRuleSet extends AbilityExplain {
  constructor(ruleSet: AbilityRuleSet) {
    const children = ruleSet.rules.map(rule => new AbilityExplainRule(rule));
    super(
      {
        type: 'ruleSet',
        match: ruleSet.state,
        name: ruleSet.name,
      },
      children,
    );
  }
}

export class AbilityExplainPolicy extends AbilityExplain {
  constructor(policy: AbilityPolicy) {
    const children = policy.ruleSet.map(ruleSet => new AbilityExplainRuleSet(ruleSet));
    super(
      {
        type: 'policy',
        name: policy.name,
        match: policy.matchState,
      },
      children,
    );
  }
}

export type AbilityExplainType = 'policy' | 'rule' | 'ruleSet';
