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
  readonly _name: string;
  readonly match: AbilityMatchType;
  readonly debugInfo?: string;

  constructor(config: AbilityExplainConfig, children: AbilityExplain[] = []) {
    this.type = config.type;
    this.children = children;
    this._name = config.name;
    this.match = config.match;
    this.debugInfo = config.debugInfo;
  }

  public get name() {
    const maxLength = 60;
    const stripped = this._name.substring(0, maxLength);
    const dots = stripped.length < this._name.length ? '...' : '';
    return `${stripped}${dots}`;
  }

  public toString(indentPrefix: string = '', isLast: boolean = true): string {
    const isMatch = this.match === AbilityMatch.match;
    const isMismatch = this.match === AbilityMatch.mismatch;
    const isPending = this.match === AbilityMatch.pending;

    const mark = isMatch
      ? 'MATCH ✓'
      : isMismatch
        ? 'MISMATCH ✗'
        : isPending
          ? 'PENDING …'
          : 'DISABLED ⊘';

    // колонка статуса
    const paddedStatus = `[${mark}]`.padEnd(15, ' ');

    // колонка типа
    const typeLabel =
      this.type === 'policy' ? 'POLICY' : this.type === 'ruleSet' ? 'RULESET' : 'RULE';

    const paddedType = typeLabel.padEnd(10, ' ');

    const branch = indentPrefix.length === 0 ? '' : isLast ? '└─ ' : '├─ ';

    let out = `${indentPrefix}${branch}${paddedStatus}${paddedType}${this.name}`;
    if (this.debugInfo) out += ` (${this.debugInfo})`;

    const nextIndent = indentPrefix + (isLast ? '   ' : '│  ');
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
        name:
          policy.priority > -1
            ? `@priority ${policy.priority} <${policy.effect}> ${policy.name}`
            : `<${policy.effect}> ${policy.name}`,
        match: policy.matchState,
      },
      children,
    );
  }
}

export type AbilityExplainType = 'policy' | 'rule' | 'ruleSet';
