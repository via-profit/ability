type AbilityCompareCode = 'and' | 'or';

export type AbilityCompareType = AbilityCompareCode & { __brand: 'AbilityCompare' };

function brand(code: AbilityCompareCode): AbilityCompareType {
  return code as AbilityCompareType;
}

export const AbilityCompare = {
  or: brand('or'),
  and: brand('and'),
} as const;
