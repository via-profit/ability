import {AbilityMatch} from '../core/AbilityMatch';
import AbilityPolicy from '../core/AbilityPolicy';
import { EnvironmentObject, ResourceObject } from '../core/AbilityTypeGenerator';
import { AbilityPolicyEffect, AbilityPolicyEffectType  } from '../core/AbilityPolicyEffect';

export abstract class AbilityStrategy<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment extends EnvironmentObject = Record<string, unknown>,
> {
  private readonly matched: readonly AbilityPolicy<Resource, Environment>[];

  constructor(public readonly policies: readonly AbilityPolicy<Resource, Environment>[]) {
    this.matched = policies.filter(p => p.matchState === AbilityMatch.match);
  }

  abstract evaluate(): AbilityPolicyEffectType;

  public matchedPolicies() {
    return this.matched;
  }

  protected firstMatched(): AbilityPolicy<Resource, Environment> | null {
    return this.matched[0] ?? null;
  }

  protected lastMatched(): AbilityPolicy<Resource, Environment> | null {
    return this.matched.length > 0 ? this.matched[this.matched.length - 1] : null;
  }

  protected hasPermit(): boolean {
    return this.matched.some(p => p.effect === AbilityPolicyEffect.permit);
  }

  protected hasDeny(): boolean {
    return this.matched.some(p => p.effect === AbilityPolicyEffect.deny);
  }

  public isAllowed(): boolean {
    return this.evaluate() === AbilityPolicyEffect.permit;
  }

  public isDenied(): boolean {
    return this.evaluate() === AbilityPolicyEffect.deny;
  }
}
