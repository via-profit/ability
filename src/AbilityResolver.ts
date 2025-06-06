import AbilityPolicy from './AbilityPolicy';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityMatch from './AbilityMatch';
import { AbilityError } from './AbilityError';

export class AbilityResolver<Resources extends object = object> {
  policies: readonly AbilityPolicy<Resources>[];

  public constructor(
    policyOrListOfPolicies: readonly AbilityPolicy<Resources>[] | AbilityPolicy<Resources>,
  ) {
    this.policies = Array.isArray(policyOrListOfPolicies)
      ? policyOrListOfPolicies
      : [policyOrListOfPolicies];
  }

  /**
   * Resolve policy for the resource and action
   *
   @param action - Action
   * @param resource - Resource
   */
  public resolve<Action extends keyof Resources>(
    action: Action,
    resource: Resources[Action],
  ): this {
    const filteredPolicies: AbilityPolicy[] = this.policies.filter(policy => {
      return AbilityResolver.isInActionContain(policy.action, String(action));
    });

    filteredPolicies.map(policy => policy.check(resource as object));

    this.policies = filteredPolicies;

    return this;
  }

  public enforce<Action extends keyof Resources>(
    action: Action,
    resource: Resources[Action],
  ): void | never {
    const resolver = this.resolve(action, resource);

    if (resolver.isDeny()) {
      throw new AbilityError(
        resolver.getMatchedPolicy()?.name?.toString() || 'Unknown permission error',
      );
    }
  }

  /**
   * Get the last effect of the policy
   *
   * @returns {AbilityPolicyEffect | null}
   */
  public getEffect(): AbilityPolicyEffect | null {
    const effects = this.policies.reduce<AbilityPolicyEffect[]>((collect, policy, _index) => {
      if (policy.matchState.isEqual(AbilityMatch.match)) {
        return collect.concat(policy.effect);
      }
      return collect;
    }, []);

    if (effects.length) {
      return effects[effects.length - 1];
    }

    return null;
  }

  public isPermit() {
    const effect = this.getEffect();

    return effect !== null && effect.isEqual(AbilityPolicyEffect.permit);
  }

  public isDeny() {
    const effect = this.getEffect();

    return effect !== null && effect.isEqual(AbilityPolicyEffect.deny);
  }

  public getMatchedPolicy(): AbilityPolicy<Resources> | null {
    const matchedPolicies = this.policies.filter(policy =>
      policy.matchState.isEqual(AbilityMatch.match),
    );

    const lastPolicy = matchedPolicies.length ? matchedPolicies[matchedPolicies.length - 1] : null;

    return lastPolicy || null;
  }

  /**
   * Check if the action is contained in another action
   * @param actionA - The first action to check
   * @param actionB - The second action to check
   */
  public static isInActionContain(actionA: string, actionB: string) {
    const actionAArray = String(actionA).split('.');
    const actionBArray = String(actionB).split('.');

    const a = actionAArray.length >= actionBArray.length ? actionAArray : actionBArray;
    const b = actionBArray.length <= actionAArray.length ? actionBArray : actionAArray;

    const c = a.reduce<boolean[]>((acc, chunk, index) => {
      const iterationRes = chunk === b[index] || b[index] === '*' || chunk === '*';

      return acc.concat(iterationRes);
    }, []);

    return c.every(Boolean);
  }
}

export default AbilityResolver;
