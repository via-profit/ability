import AbilityPolicy from '~/core/AbilityPolicy';
import { AbilityDSLToken } from '~/parsers/dsl/AbilityDSLToken';
import { AbilityDSLLexer } from '~/parsers/dsl/AbilityDSLLexer';

export class AbilityDSLParser {
  private dsl: string;
  private tokens: AbilityDSLToken[] = [];

  public constructor(input: string) {
    this.dsl = input;
  }

  public parse(): readonly AbilityPolicy[] {
    this.tokens = new AbilityDSLLexer(this.dsl).tokenize();

    const str = this.tokens.map(token => token.toString());

    console.log(str);
    return [];
  }
}
