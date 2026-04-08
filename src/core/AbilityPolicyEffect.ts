type AbilityPolicyEffectCode = 'deny' | 'permit';

export type AbilityPolicyEffectType = AbilityPolicyEffectCode & { __brand: 'AbilityPolicyEffect' };

function brand(code: AbilityPolicyEffectCode): AbilityPolicyEffectType {
  return code as AbilityPolicyEffectType;
}

export const AbilityPolicyEffect = {
  deny: brand('deny'),
  permit: brand('permit'),
} as const;
