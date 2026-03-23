import AbilityCode from '../../core/AbilityCode';

export type TokenType =
  | 'if' // start rule set
  | 'effect' // allow, deny, разрешить, запретить
  | 'action' // order.update, user.create
  | 'condition' // =, <>, >, <, >=, <=, in, not in, equal, not_equal, more_than, less_than, less_or_equal, more_or_equal, in, not_in;
  | 'path' // user.role, order.status
  | 'string' // 'admin', "manager"
  | 'number' // 18, 100
  | 'boolean' // true, false
  // | 'lparen' // (
  // | 'rparen' // )
  | 'indent' // отступ
  | 'dedent' // возврат отступа
  // | 'newline' // \n
  // | 'eof' // конец файла
  | 'symbol'
  | 'word'
  | 'compareMethod' // or, and
  | 'unknown'; // otherwise token

export class AbilityDSLTokenType extends AbilityCode<TokenType> {
  public static compare = new AbilityDSLTokenType('compareMethod');
  public static word = new AbilityDSLTokenType('word');
  public static symbol = new AbilityDSLTokenType('symbol');
  public static if = new AbilityDSLTokenType('if');
  public static effect = new AbilityDSLTokenType('effect');
  public static action = new AbilityDSLTokenType('action');
  public static condition = new AbilityDSLTokenType('condition');

  public static path = new AbilityDSLTokenType('path');
  public static string = new AbilityDSLTokenType('string');
  public static number = new AbilityDSLTokenType('number');
  public static boolean = new AbilityDSLTokenType('boolean');
  // public static lparen = new AbilityDSLTokenType('lparen');
  // public static rparen = new AbilityDSLTokenType('rparen');
  public static indent = new AbilityDSLTokenType('indent');
  public static dedent = new AbilityDSLTokenType('dedent');
  // public static newline = new AbilityDSLTokenType('newline');
  // public static eof = new AbilityDSLTokenType('eof');
  public static unknown = new AbilityDSLTokenType('unknown');

  public static resolve(str: string): AbilityDSLTokenType {
    const a: Record<string, TokenType> = {
      allow: 'effect',
      deny: 'effect',
      if: 'if',

      // conditions
      '=': 'condition',
      '<>': 'condition',
      '>': 'condition',
      '<': 'condition',
      '>=': 'condition',
      '<=': 'condition',
      in: 'condition',
      not: 'condition',
      equal: 'condition',
      notequal: 'condition',
      'more than': 'condition',
      'less than': 'condition',
      'less or equal': 'condition',
      'more or equal': 'condition',
      'not in': 'condition',
    };

    if (a[str]) {
      return new AbilityDSLTokenType(str as TokenType);
    }

    return new AbilityDSLTokenType('unknown');
  }
}
