import AbilityPolicy from './AbilityPolicy';
import { AbilityError } from './AbilityError';
import { AbilityResult } from './AbilityResult';
import AbilityMatch from './AbilityMatch';
import { ResourcesMap } from './AbilityTypeGenerator';
import { AbilityCacheAdapter } from '../cache/AbilityCacheAdapter';


export type AbilityResolverOptions = {
  readonly cache?: AbilityCacheAdapter | null;
};

export class AbilityResolver<Resources extends ResourcesMap, Environment = unknown> {
  private policies: readonly AbilityPolicy[];
  private readonly cache?: AbilityCacheAdapter | null;

  public constructor(
    /**
     * `Important!` The incorrect Resources type was intentionally passed to AbilityPolicy so that TypeScript could suggest the name of the permission and the structure of its resource in the parse method.
     */
    policyOrListOfPolicies: readonly AbilityPolicy<Resources>[] | AbilityPolicy<Resources>,
    options?: AbilityResolverOptions,
  ) {
    const { cache } = options || {};
    this.cache = cache;
    this.policies = Array.isArray(policyOrListOfPolicies)
      ? policyOrListOfPolicies
      : [policyOrListOfPolicies];
  }

  /**
   * Resolve policy for the resource and permission key
   *
   * @param permission - Permission key
   * @param resource - Resource
   * @param environment
   */
  public async resolve<Permission extends keyof Resources>(
    permission: Permission,
    resource: Resources[Permission],
    environment?: Environment,
  ): Promise<AbilityResult<Resources[Permission]>> {

    const filteredPolicies = this.policies.filter(policy =>
      AbilityResolver.isInPermissionContain(policy.permission, String(permission).replace(/^permission\./, '')),
    );

    for (const policy of filteredPolicies) {
      const cacheKey = this.cache ? this.makeCacheKey(policy.id, resource, environment) : '';
      // cache
      if (this.cache) {
        const cached = await this.cache.get<AbilityMatch>(cacheKey);

        if (cached !== undefined) {
          policy.matchState = cached;
          continue;
        }
      }

      const policyMatchState = await policy.check(resource, environment);

      if (policyMatchState === AbilityMatch.pending) {
        throw new AbilityError(
          `The policy "${policy.name}" is still in a pending state. Make sure to call "check" to evaluate the policy before resolving permissions.`,
        );
      }

      if (this.cache) {
        await this.cache.set(cacheKey, policyMatchState);
      }
    }

    return new AbilityResult(filteredPolicies);
  }

  public async enforce<Permission extends keyof Resources>(
    permission: Permission,
    resource: Resources[Permission],
    environment?: Environment,
  ): Promise<void | never> {
    const result = await this.resolve(permission, resource, environment);

    if (result.isDenied()) {
      const policyName = result.getLastMatchedPolicy()?.name?.toString() || 'unknown';
      throw new AbilityError(`Permission denied by policy "${policyName}"`);
    }
  }

  /**
   * Check if the permission key is contained in another permission key
   * @param permissionA - The first permission to check
   * @param permissionB - The second permission to check
   */
  public static isInPermissionContain(permissionA: string, permissionB: string) {
    const A = permissionA.split('.');
    const B = permissionB.split('.');

    const [longer, shorter] = A.length >= B.length ? [A, B] : [B, A];

    return shorter.every((chunk, i) => {
      return chunk === '*' || longer[i] === '*' || chunk === longer[i];
    });
  }

  private makeCacheKey<Permission extends keyof Resources>(
    policyId: string,
    resource: Resources[Permission],
    environment?: Environment,
  ): string {
    if (!this.cache) {
      return '';
    }

    return `policy:${policyId}:res:${this.cache.serialize(resource)}:env:${this.cache.serialize(environment)}`;
  }

  public async invalidatePolicy(policyId: string): Promise<void> {
    await this.cache?.deleteByPrefix(policyId);
  }

  public async invalidateCache(): Promise<void> {
    await this.cache?.clear();
  }
}

export default AbilityResolver;
