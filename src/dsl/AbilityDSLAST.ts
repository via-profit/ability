import AbilityCompare, { AbilityCompareCodeType } from '../core/AbilityCompare';
import { AbilityConditionCodeType } from '../core/AbilityCondition';
import { AbilityPolicyEffectCodeType } from '../core/AbilityPolicyEffect';
import { AbilityDSLToken } from './AbilityDSLToken';
import { AbilityDSLTokenType } from './AbilityDSLTokenType';

export class AbilityDSLPolicyNode {
  readonly id: string;
  readonly name: string;
  readonly action: string;
  readonly effect: AbilityPolicyEffectCodeType;
  readonly compareMethod: AbilityCompare;
  readonly ruleSet: readonly AbilityDSLRuleSetNode[];

  public constructor(
    id: string,
    name: string,
    action: string,
    effect: AbilityPolicyEffectCodeType,
    compareMethod: AbilityCompare,
    ruleSet: readonly AbilityDSLRuleSetNode[],
  ) {
    this.id = id;
    this.name = name;
    this.action = action;
    this.effect = effect;
    this.compareMethod = compareMethod;
    this.ruleSet = ruleSet;
  }
}

export class AbilityDSLRuleSetNode {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly compareMethod: AbilityCompareCodeType;
  readonly rules: readonly AbilityDSLRuleNode[];

  public constructor(
    id: string | null,
    name: string | null,
    compareMethod: AbilityCompareCodeType,
    rules: readonly AbilityDSLRuleNode[],
  ) {
    this.id = id || null;
    this.name = name || null;
    this.compareMethod = compareMethod;
    this.rules = rules;
  }
}

export class AbilityDSLRuleNode {
  readonly id?: string | null;
  readonly name?: string | null;
  readonly condition: AbilityConditionCodeType;

  public constructor(id: string | null, name: string | null, condition: AbilityConditionCodeType) {
    this.id = id || null;
    this.name = name || null;
    this.condition = condition;
  }
}

export class AbilityDSLAST {
  policies: AbilityDSLPolicyNode[];

  public constructor(policies: AbilityDSLPolicyNode[]) {
    this.policies = policies;
  }

  addPolicy(policy: AbilityDSLPolicyNode) {
    this.policies.push(policy);
  }

  public static parse(dsl: string): AbilityDSLAST {
    // get lines
    const lines = dsl.split('\n');

    let parentToken : AbilityDSLToken | null = null;
    const tokens: AbilityDSLToken[] = [];
    // read lines
    for(let lineIndex = 0; lineIndex < lines.length; lineIndex += 1) {
      
      const line = lines[lineIndex].trim();

      // skip the comments
      if (line[0] == '#') {
        continue;
      }

      const words = line.split('\n');

      // read words
      for (let wordIndex = 0; wordIndex < words.length; wordIndex += 1) {
        // resolve token type
        
        const tokenType = AbilityDSLTokenType.resolve(words[wordIndex]);

        // check if resolve successfully
        if (tokenType.isEqual(AbilityDSLTokenType.unknown)) {
          
          const token: AbilityDSLToken = new AbilityDSLToken(tokenType, parentToken, words[wordIndex], lineIndex + 1);
          
          tokens.push(token);

          parentToken = token;
          continue;
        }

        // token type is unknown
        // ...

      }
    }

    console.debug(tokens);
    return new AbilityDSLAST([]);
  }

  
}
