import AbilityRule from './AbilityRule';
import AbilityRuleSet from './AbilityRuleSet';
import AbilityPolicy from './AbilityPolicy';
import {AbilityMatchType, AbilityMatch} from './AbilityMatch';

export type AbilityExplainConfig = {
  readonly type: AbilityExplainType;
  readonly name: string;
  readonly match: AbilityMatchType;
  readonly debugInfo?: boolean;
};



const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  blue: '\x1b[34m',
  yellow: '\x1b[33m',
  white: '\x1b[37m',
  gray: '\x1b[90m',
};

export class AbilityExplain {
  readonly type: AbilityExplainType;
  readonly children: AbilityExplain[];
  readonly name: string;
  readonly match: AbilityMatchType;
  readonly debugInfo?: boolean;

  constructor(config: AbilityExplainConfig, children: AbilityExplain[] = []) {
    this.type = config.type;
    this.children = children;
    this.name = config.name;
    this.match = config.match;
    this.debugInfo = config.debugInfo;
  }

  public toString(indentPrefix: string = '', isLast: boolean = true): string {
    const isMatch = this.match === AbilityMatch.match;

    const mark = isMatch ? `${colors.green}✓${colors.reset}` : `${colors.red}✗${colors.reset}`;

    const label =
      this.type === 'policy'
        ? `${colors.blue}POLICY${colors.reset}`
        : this.type === 'ruleSet'
          ? `${colors.yellow}RULESET${colors.reset}`
          : `${colors.white}RULE${colors.reset}`;

    const branch =
      indentPrefix.length === 0
        ? ''
        : isLast
          ? `${colors.gray}└─${colors.reset} `
          : `${colors.gray}├─${colors.reset} `;

    let out = `${indentPrefix}${branch}${label} ${this.name} — ${mark}`;

    if (this.debugInfo) {
      out += ` ${colors.gray}(${this.debugInfo})${colors.reset}`;
    }

    const nextIndent = indentPrefix + (isLast ? '   ' : `${colors.gray}│  ${colors.reset}`);

    this.children.forEach((child, index) => {
      const last = index === this.children.length - 1;
      out += '\n' + child.toString(nextIndent, last);
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
        name: policy.priority > -1 ? `@priority ${policy.priority} ${policy.name}` : policy.name,
        match: policy.matchState,
      },
      children,
    );
  }
}

export type AbilityExplainType = 'policy' | 'rule' | 'ruleSet';
