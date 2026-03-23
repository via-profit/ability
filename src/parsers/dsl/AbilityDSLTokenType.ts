import AbilityCode from '../../core/AbilityCode';

export type TokenType =
  // Структурные токены
  | 'EFFECT' // allow, deny
  | 'IF' // if, when, whenever
  | 'ACTION' // order.update
  | 'IDENTIFIER' // user, order, role
  | 'DOT' // .
  | 'LPAREN' // (
  | 'RPAREN' // )
  | 'LBRACKET' // [
  | 'RBRACKET' // ]
  | 'COMMA' // ,
  | 'COLON' // :
  | 'ALL' // all
  | 'OF' // of
  | 'ANY' // any

  // Логические операторы
  | 'AND' // and
  | 'OR' // or
  | 'NOT' // not

  // Операторы сравнения
  | 'EQ' // ==
  | 'NEQ' // !=
  | 'GT' // >
  | 'GTE' // >=
  | 'LT' // <
  | 'LTE' // <=
  | 'CONTAINS' // contains
  | 'NOT_CONTAINS' // not contains
  | 'IN' // in
  | 'NOT_IN' // not in

  // Значения
  | 'STRING'
  | 'NUMBER'
  | 'BOOLEAN'

  // Служебные
  | 'UNKNOWN';

export class AbilityDSLTokenType extends AbilityCode<TokenType> {
  // Структурные
  public static ALL = new AbilityDSLTokenType('ALL');
  public static ANY = new AbilityDSLTokenType('ANY');
  public static OF = new AbilityDSLTokenType('OF');
  public static COLON = new AbilityDSLTokenType('COLON');
  public static EFFECT = new AbilityDSLTokenType('EFFECT');
  public static IF = new AbilityDSLTokenType('IF');
  public static ACTION = new AbilityDSLTokenType('ACTION');
  public static IDENTIFIER = new AbilityDSLTokenType('IDENTIFIER');
  public static DOT = new AbilityDSLTokenType('DOT');
  public static LPAREN = new AbilityDSLTokenType('LPAREN');
  public static RPAREN = new AbilityDSLTokenType('RPAREN');
  public static LBRACKET = new AbilityDSLTokenType('LBRACKET');
  public static RBRACKET = new AbilityDSLTokenType('RBRACKET');
  public static COMMA = new AbilityDSLTokenType('COMMA');

  // Логические операторы
  public static AND = new AbilityDSLTokenType('AND');
  public static OR = new AbilityDSLTokenType('OR');
  public static NOT = new AbilityDSLTokenType('NOT');

  // Операторы сравнения
  public static EQ = new AbilityDSLTokenType('EQ');
  public static NEQ = new AbilityDSLTokenType('NEQ');
  public static GT = new AbilityDSLTokenType('GT');
  public static GTE = new AbilityDSLTokenType('GTE');
  public static LT = new AbilityDSLTokenType('LT');
  public static LTE = new AbilityDSLTokenType('LTE');
  public static CONTAINS = new AbilityDSLTokenType('CONTAINS');
  public static NOT_CONTAINS = new AbilityDSLTokenType('NOT_CONTAINS');
  public static IN = new AbilityDSLTokenType('IN');
  public static NOT_IN = new AbilityDSLTokenType('NOT_IN');

  // Значения
  public static STRING = new AbilityDSLTokenType('STRING');
  public static NUMBER = new AbilityDSLTokenType('NUMBER');
  public static BOOLEAN = new AbilityDSLTokenType('BOOLEAN');

  // Служебные
  public static UNKNOWN = new AbilityDSLTokenType('UNKNOWN');

  /**
   * resolve() — определяет тип токена по строке.
   *
   * Этот метод вызывается лексером, когда он прочитал слово.
   * Он должен быть максимально предсказуемым и простым.
   */
  public static resolve(str: string): AbilityDSLTokenType {
    const lower = str.toLowerCase();

    // Эффекты
    if (['allow', 'permit'].includes(lower)) {
      return AbilityDSLTokenType.EFFECT;
    }
    if (['deny', 'forbidden'].includes(lower)) {
      return AbilityDSLTokenType.EFFECT;
    }

    // IF
    if (['if', 'when', 'whenever'].includes(lower)) {
      return AbilityDSLTokenType.IF;
    }

    // Логические операторы
    if (lower === 'and') {
      return AbilityDSLTokenType.AND;
    }
    if (lower === 'or') {
      return AbilityDSLTokenType.OR;
    }
    if (lower === 'not') {
      return AbilityDSLTokenType.NOT;
    }

    // Операторы сравнения (одиночные)
    if (lower === 'contains') {
      return AbilityDSLTokenType.CONTAINS;
    }
    if (lower === 'in') {
      return AbilityDSLTokenType.IN;
    }

    // Значения
    if (lower === 'true' || lower === 'false') {
      return AbilityDSLTokenType.BOOLEAN;
    }
    if (!isNaN(Number(lower))) {
      return AbilityDSLTokenType.NUMBER;
    }

    // Всё остальное — идентификатор
    return AbilityDSLTokenType.IDENTIFIER;
  }
}
