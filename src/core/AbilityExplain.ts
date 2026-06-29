import AbilityRule from './AbilityRule';
import AbilityRuleSet from './AbilityRuleSet';
import AbilityPolicy from './AbilityPolicy';
import { AbilityMatch, AbilityMatchType } from './AbilityMatch';

export type AbilityExplainConfig = {
  readonly type: AbilityExplainType;
  readonly name: string;
  readonly match: AbilityMatchType;
  readonly debugInfo?: string;
};

export class AbilityExplain {
  readonly type: AbilityExplainType;
  readonly children: AbilityExplain[];
  readonly name: string;
  readonly match: AbilityMatchType;
  readonly debugInfo?: string;

  constructor(config: AbilityExplainConfig, children: AbilityExplain[] = []) {
    this.type = config.type;
    this.children = children;
    this.name = config.name;
    this.match = config.match;
    this.debugInfo = config.debugInfo;
  }

  public toString(indentPrefix: string = '', isLast: boolean = true): string {
    const isMatch = this.match === AbilityMatch.match;
    const isMismatch = this.match === AbilityMatch.mismatch;
    const isPending = this.match === AbilityMatch.pending;
    // const isDisabled = this.match === AbilityMatch.disabled;

    const mark = isMatch ? `✓` : isMismatch ? `✗` : isPending ? `…` : `⊘`;

    let label: string;
    switch (this.type) {
      case 'policy':
        label = `POLICY`;
        break;
      case 'ruleSet':
        label = `RULESET`;
        break;
      default:
        label = `RULE`;
    }
    const branch =
      indentPrefix.length === 0
        ? ''
        : isLast
          ? `└─ `
          : `├─ `;
    let out = `${indentPrefix}${branch}${label} ${this.name} — ${mark}`;
    if (this.debugInfo) out += ` (${this.debugInfo})`;
    const nextIndent = indentPrefix + (isLast ? '   ' : `│  `);
    this.children.forEach((child, idx) => {
      out += '\n' + child.toString(nextIndent, idx === this.children.length - 1);
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
      debugInfo: `${rule.subject} ${rule.condition} ${JSON.stringify(rule.resource)}`,
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
        name: policy.priority > -1 ? `@priority ${policy.priority} ${policy.name}` : policy.name,
        match: policy.matchState,
      },
      children,
    );
  }
}

export type AbilityExplainType = 'policy' | 'rule' | 'ruleSet';
