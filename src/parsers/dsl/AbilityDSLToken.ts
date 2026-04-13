export type TokenTypeCode =
  | 'EFFECT'
  | 'IF'
  | 'PERMISSION'
  | 'IDENTIFIER'
  | 'COLON'
  | 'COMMA'
  | 'DOT'
  | 'LBRACKET'
  | 'RBRACKET'
  | 'ALL'
  | 'ANY'
  | 'OF'
  | 'EOF'
  | 'COMMENT'
  | 'EQ'
  | 'CONTAINS'
  | 'IN'
  | 'NOT_IN'
  | 'NOT_CONTAINS'
  | 'GT'
  | 'GTE'
  | 'LT'
  | 'LTE'
  | 'NULL'
  | 'EQ_NULL'
  | 'NOT_EQ_NULL'
  | 'NOT_EQ'
  | 'LEN_GT'
  | 'LEN_LT'
  | 'LEN_EQ'
  | 'ALWAYS'
  | 'NEVER'
  | 'EXCEPT'
  | 'ANNOTATION'
  | 'STRING'
  | 'NUMBER'
  | 'BOOLEAN'
  | 'SYMBOL'
  | 'KEYWORD'
  | 'ALIAS'
  | 'UNKNOWN';

export type TokenType = TokenTypeCode & { __brand: 'TokenType' };

function brand(code: TokenTypeCode): TokenType {
  return code as TokenType;
}


export const TokenTypes = {
  EFFECT: brand('EFFECT'),
  IF: brand('IF'),
  PERMISSION: brand('PERMISSION'),
  IDENTIFIER: brand('IDENTIFIER'),
  COLON: brand('COLON'),
  COMMA: brand('COMMA'),
  DOT: brand('DOT'),
  LBRACKET: brand('LBRACKET'),
  RBRACKET: brand('RBRACKET'),
  ALL: brand('ALL'),
  ANY: brand('ANY'),
  OF: brand('OF'),
  EOF: brand('EOF'),
  COMMENT: brand('COMMENT'),
  EQ: brand('EQ'),
  CONTAINS: brand('CONTAINS'),
  IN: brand('IN'),
  NOT_IN: brand('NOT_IN'),
  NOT_CONTAINS: brand('NOT_CONTAINS'),
  GT: brand('GT'),
  GTE: brand('GTE'),
  LT: brand('LT'),
  LTE: brand('LTE'),
  NULL: brand('NULL'),
  EQ_NULL: brand('EQ_NULL'),
  NOT_EQ_NULL: brand('NOT_EQ_NULL'),
  NOT_EQ: brand('NOT_EQ'),
  LEN_GT: brand('LEN_GT'),
  LEN_LT: brand('LEN_LT'),
  LEN_EQ: brand('LEN_EQ'),
  ALWAYS: brand('ALWAYS'),
  NEVER: brand('NEVER'),
  EXCEPT: brand('EXCEPT'),
  ANNOTATION: brand('ANNOTATION'),
  STRING: brand('STRING'),
  NUMBER: brand('NUMBER'),
  BOOLEAN: brand('BOOLEAN'),
  SYMBOL: brand('SYMBOL'),
  KEYWORD: brand('KEYWORD'),
  ALIAS: brand('ALIAS'),
  UNKNOWN: brand('UNKNOWN'),
} as const;


export class AbilityDSLToken {
  readonly type: TokenType;
  readonly value: string;
  readonly line: number;
  readonly column: number;

  constructor(type: TokenType, value: string, line: number, column: number) {
    this.type = type;
    this.value = value;
    this.line = line;
    this.column = column;
  }

  toString(): string {
    return `AbilityDSLToken([${this.type}] "${this.value}" at ${this.line}:${this.column})`;
  }
}
