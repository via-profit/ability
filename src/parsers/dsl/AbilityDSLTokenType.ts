import AbilityCode from '../../core/AbilityCode';

export type TokenType =
  // Структурные токены
  | 'EFFECT'        // permit, deny
  | 'IF'            // if
  | 'ACTION'        // order.update
  | 'IDENTIFIER'    // user.roles, env.time.hour
  | 'COLON'         // :
  | 'COMMA'         // ,
  | 'DOT'           // .
  | 'LBRACKET'      // [
  | 'RBRACKET'      // ]
  | 'ALL'           // all
  | 'ANY'           // any
  | 'OF'            // of
  | 'EOF'            // end of file

  // Операторы сравнения (словесные)
  | 'EQ'            // equals, is
  | 'CONTAINS'      // contains
  | 'IN'            // in
  | 'GT_WORD'       // greater
  | 'LT_WORD'       // less
  | 'NULL'          // null
  |'EQ_NULL' // is null
  | 'NOT_EQ_NULL' // is not null

  // Значения
  | 'STRING'
  | 'NUMBER'
  | 'BOOLEAN'


  // Служебные
  | 'UNKNOWN';

export class AbilityDSLTokenType extends AbilityCode<TokenType> {
  // Структурные
  public static EOF = new AbilityDSLTokenType('EOF');
  public static DOT = new AbilityDSLTokenType('DOT');
  public static EFFECT = new AbilityDSLTokenType('EFFECT');
  public static IF = new AbilityDSLTokenType('IF');
  public static ACTION = new AbilityDSLTokenType('ACTION');
  public static IDENTIFIER = new AbilityDSLTokenType('IDENTIFIER');
  public static COLON = new AbilityDSLTokenType('COLON');
  public static COMMA = new AbilityDSLTokenType('COMMA');
  public static LBRACKET = new AbilityDSLTokenType('LBRACKET');
  public static RBRACKET = new AbilityDSLTokenType('RBRACKET');
  public static ALL = new AbilityDSLTokenType('ALL');
  public static ANY = new AbilityDSLTokenType('ANY');
  public static OF = new AbilityDSLTokenType('OF');

  // Операторы сравнения
  public static EQ = new AbilityDSLTokenType('EQ'); // equals, is
  public static CONTAINS = new AbilityDSLTokenType('CONTAINS');
  public static IN = new AbilityDSLTokenType('IN');
  public static GT_WORD = new AbilityDSLTokenType('GT_WORD'); // greater
  public static LT_WORD = new AbilityDSLTokenType('LT_WORD'); // less
  public static NULL = new AbilityDSLTokenType('NULL');
  public static EQ_NULL = new AbilityDSLTokenType('EQ_NULL');
public static NOT_EQ_NULL = new AbilityDSLTokenType('NOT_EQ_NULL');


  // Значения
  public static STRING = new AbilityDSLTokenType('STRING');
  public static NUMBER = new AbilityDSLTokenType('NUMBER');
  public static BOOLEAN = new AbilityDSLTokenType('BOOLEAN');

  // Служебные
  public static UNKNOWN = new AbilityDSLTokenType('UNKNOWN');

  /**
   * resolve() — определяет тип токена по строке.
   * Используется только для простых случаев.
   */
  public static resolve(str: string): AbilityDSLTokenType {
    const lower = str.toLowerCase();

    // Эффекты
    if (lower === 'permit' || lower === 'allow') return AbilityDSLTokenType.EFFECT;
    if (lower === 'deny' || lower === 'forbidden') return AbilityDSLTokenType.EFFECT;

    // IF
    if (lower === 'if') return AbilityDSLTokenType.IF;

    // Группы
    if (lower === 'all') return AbilityDSLTokenType.ALL;
    if (lower === 'any') return AbilityDSLTokenType.ANY;
    if (lower === 'of') return AbilityDSLTokenType.OF;

    // Операторы сравнения
    if (lower === 'equals' || lower === 'is') return AbilityDSLTokenType.EQ;
    if (lower === 'contains') return AbilityDSLTokenType.CONTAINS;
    if (lower === 'in') return AbilityDSLTokenType.IN;
    if (lower === 'greater') return AbilityDSLTokenType.GT_WORD;
    if (lower === 'less') return AbilityDSLTokenType.LT_WORD;
    if (lower === 'null') return AbilityDSLTokenType.NULL;

    // Значения
    if (lower === 'true' || lower === 'false') return AbilityDSLTokenType.BOOLEAN;
    if (!isNaN(Number(lower))) return AbilityDSLTokenType.NUMBER;

    // Всё остальное — идентификатор
    return AbilityDSLTokenType.IDENTIFIER;
  }
}
