type AbilityMatchCode =
  | 'pending'
  | 'match'
  | 'mismatch'
  | 'except-mismatch'
  | 'disabled';

export type AbilityMatchType = AbilityMatchCode & { __brand: 'AbilityMatch' };

function brand(code: AbilityMatchCode): AbilityMatchType {
  return code as AbilityMatchType;
}

export const AbilityMatch = {
  pending: brand('pending'),
  match: brand('match'),
  mismatch: brand('mismatch'),
  exceptMismatch: brand('except-mismatch'),
  disabled: brand('disabled'),
} as const;


