import {AbilityParserError} from '../core/AbilityError'

export type AbilityConditionCode =
  | '='
  | '<>'
  | '>'
  | '<'
  | '>='
  | '<='
  | 'in'
  | 'not in'
  | 'contains'
  | 'not contains'
  | 'length greater than'
  | 'length less than'
  | 'length equals'
  | 'always'
  | 'never';

export type AbilityConditionLiteral =
  | 'equals'
  | 'not_equals'
  | 'contains'
  | 'not_contains'
  | 'in'
  | 'not_in'
  | 'greater_than'
  | 'less_than'
  | 'less_or_equal'
  | 'greater_or_equal'
  | 'length_greater_than'
  | 'length_less_than'
  | 'length_equals'
  | 'always'
  | 'never';


export type AbilityConditionType = AbilityConditionCode & { __brand: 'AbilityCondition' };

function brand(code: AbilityConditionCode): AbilityConditionType {
  return code as AbilityConditionType;
}

export const AbilityCondition = {
  equals: brand('='),
  not_equals: brand('<>'),
  greater_than: brand('>'),
  less_than: brand('<'),
  less_or_equal: brand('<='),
  greater_or_equal: brand('>='),
  in: brand('in'),
  not_in: brand('not in'),
  contains: brand('contains'),
  not_contains: brand('not contains'),
  length_greater_than: brand('length greater than'),
  length_less_than: brand('length less than'),
  length_equals: brand('length equals'),
  always: brand('always'),
  never: brand('never'),
} as const;


export function fromLiteral(literal: AbilityConditionLiteral): AbilityConditionType {
  const map: Record<AbilityConditionLiteral, AbilityConditionType> = {
    equals: AbilityCondition.equals,
    not_equals: AbilityCondition.not_equals,
    greater_than: AbilityCondition.greater_than,
    less_than: AbilityCondition.less_than,
    less_or_equal: AbilityCondition.less_or_equal,
    greater_or_equal: AbilityCondition.greater_or_equal,
    in: AbilityCondition.in,
    not_in: AbilityCondition.not_in,
    contains: AbilityCondition.contains,
    not_contains: AbilityCondition.not_contains,
    length_greater_than: AbilityCondition.length_greater_than,
    length_less_than: AbilityCondition.length_less_than,
    length_equals: AbilityCondition.length_equals,
    always: AbilityCondition.always,
    never: AbilityCondition.never,
  };

  const value = map[literal];

  if (!value) {
    const expected = Object.keys(map).join(', ');
    throw new AbilityParserError(
      `Literal "${literal}" does not found in AbilityCondition. Expected one of: ${expected}`
    );
  }

  return value;
}


export function toLiteral(cond: AbilityConditionType): AbilityConditionLiteral {
  switch (cond) {
    case AbilityCondition.equals:
      return 'equals';
    case AbilityCondition.not_equals:
      return 'not_equals';
    case AbilityCondition.greater_than:
      return 'greater_than';
    case AbilityCondition.less_than:
      return 'less_than';
    case AbilityCondition.less_or_equal:
      return 'less_or_equal';
    case AbilityCondition.greater_or_equal:
      return 'greater_or_equal';
    case AbilityCondition.in:
      return 'in';
    case AbilityCondition.not_in:
      return 'not_in';
    case AbilityCondition.contains:
      return 'contains';
    case AbilityCondition.not_contains:
      return 'not_contains';
    case AbilityCondition.length_greater_than:
      return 'length_greater_than';
    case AbilityCondition.length_less_than:
      return 'length_less_than';
    case AbilityCondition.length_equals:
      return 'length_equals';
    case AbilityCondition.always:
      return 'always';
    case AbilityCondition.never:
      return 'never';
      default:
        return 'never';
  }
}


export function isConditionEqual(
  a: AbilityConditionType | null,
  b: AbilityConditionType | null
): boolean {
  return a !== null && b !== null && a === b;
}

export function isConditionNotEqual(
  a: AbilityConditionType | null,
  b: AbilityConditionType | null
): boolean {
  return !isConditionEqual(a, b);
}
