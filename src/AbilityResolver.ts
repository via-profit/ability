import AbilityPolicy from './AbilityPolicy';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityMatch from './AbilityMatch';
import { PermissionError } from './AbilityError';

export class AbilityResolver<Resources extends object = object> {
  policies: readonly AbilityPolicy<Resources>[];

  public constructor(policies: readonly AbilityPolicy<Resources>[]) {
    this.policies = policies;
  }

  /**
   * Resolve policy for the resource and action
   *
   * @param resource - Resource
   * @param action - Action
   */
  public resolve<Action extends keyof Resources>(
    resource: Resources[Action],
    action: Action,
  ): this {
    const filteredPolicies: AbilityPolicy[] = this.policies.filter(policy => {
      return AbilityResolver.isInActionContain(policy.action, String(action));
    });

    filteredPolicies.map(policy => policy.check(resource as object));

    this.policies = filteredPolicies;

    return this;
  }

  public enforce<Action extends keyof Resources>(
    resource: Resources[Action],
    action: Action,
  ): void | never {
    const resolver = this.resolve(resource, action);
    if (resolver) {
      if (resolver.isDeny()) {
        throw new PermissionError(
          ['Permission denied', resolver.getPolicy()?.name?.toString()].join('. '),
        );
      }
    }
  }

  /**
   * Get the last effect of the policy
   *
   * @returns {AbilityPolicyEffect | null}
   */
  public getEffect(): AbilityPolicyEffect | null {
    const effects = this.policies.reduce<AbilityPolicyEffect[]>((collect, policy, _index) => {
      if (policy.matchState.isEqual(AbilityMatch.MATCH)) {
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

    return effect !== null && effect.isEqual(AbilityPolicyEffect.PERMIT);
  }

  public isDeny() {
    const effect = this.getEffect();

    return effect !== null && effect.isEqual(AbilityPolicyEffect.DENY);
  }

  public getPolicy(): AbilityPolicy<Resources> | null {
    const lastPolicy = this.policies.length ? this.policies[this.policies.length - 1] : null;

    return lastPolicy && lastPolicy.matchState.isEqual(AbilityMatch.MATCH) ? lastPolicy : null;
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
    const b = actionBArray.length >= actionAArray.length ? actionBArray : actionAArray;

    return a
      .reduce<boolean[]>((acc, chunk, index) => {
        const iterationRes = chunk === b[index] || b[index] === '*' || chunk === '*';

        return acc.concat(iterationRes);
      }, [])
      .every(Boolean);
  }
}

export default AbilityResolver;
