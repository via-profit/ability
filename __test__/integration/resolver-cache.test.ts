import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import AbilityResolver from '../../src/core/AbilityResolver';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';
import { AbilityMatch } from '../../src/core/AbilityMatch';

/**
 * Resolve with an arbitrary (not typed) permission key
 */
const resolveAny = (resolver: AbilityResolver<any, any>, key: string, resource: object) =>
  (resolver.resolve as (key: string, resource: object) => ReturnType<typeof resolver.resolve>)(
    key,
    resource,
  );

const getCache = (resolver: AbilityResolver<any, any>) =>
  (resolver as unknown as { selectionCache: Map<string, readonly unknown[]> }).selectionCache;

describe('AbilityResolver selection cache', () => {
  const policies = new AbilityDSLParser(`
  permit permission.* if all:
    user.roles contains 'admin'

  permit permission.order.update if all:
    order.authorId equals user.id

  deny permission.order.update if all:
    order.locked is true

  permit permission.*.create if all:
    user.active is true
  `).parse();

  it('should cache the selected policies but not the decision', () => {
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    expect(
      resolver.resolve('order.update', { order: { authorId: 1 }, user: { id: 1 } }).isAllowed(),
    ).toBeTruthy();
    expect(
      resolver.resolve('order.update', { order: { authorId: 2 }, user: { id: 1 } }).isAllowed(),
    ).toBeFalsy();
    expect(
      resolver
        .resolve('order.update', { order: { authorId: 1, locked: true }, user: { id: 1 } })
        .isDenied(),
    ).toBeTruthy();

    expect(getCache(resolver).size).toBe(1);
    expect(getCache(resolver).get('order.update')).toHaveLength(3);
  });

  it('should select the same policies for the cached and the fresh key', () => {
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    const first = resolver.resolve('order.create', { user: { active: true, roles: [] } });
    const second = resolver.resolve('order.create', { user: { active: true, roles: [] } });

    expect(first.strategy.policies).toBe(second.strategy.policies);
    expect(second.strategy.policies.map(p => p.permission)).toEqual(['*', '*.create']);
    expect(second.isAllowed()).toBeTruthy();
  });

  it('should normalize the key before selection', () => {
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
    const resource = { order: { authorId: 1 }, user: { id: 1 } };

    expect(resolver.resolve('order.update', resource).isAllowed()).toBeTruthy();
    expect(resolveAny(resolver, 'permission.order.update', resource).isAllowed()).toBeTruthy();
    expect(resolveAny(resolver, 'ORDER.UPDATE', resource).isAllowed()).toBeTruthy();
  });

  it('should respect the policy disabled flag changed after the previous check', () => {
    const [policy] = new AbilityDSLParser(`
    permit permission.a if all:
      always
    `).parse();
    const resolver = new AbilityResolver([policy], DenyOverridesStrategy);

    expect(resolver.resolve('a', {}).isAllowed()).toBeTruthy();
    policy.disabled = true;
    expect(resolver.resolve('a', {}).isAllowed()).toBeFalsy();
  });

  it('should keep the explanation of the earlier result with the shared cached list', () => {
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    const allowed = resolver.resolve('order.update', { order: { authorId: 1 }, user: { id: 1 } });
    resolver.resolve('order.update', { order: { authorId: 2 }, user: { id: 1 } });

    expect(allowed.explainToJSON().policies[1].match).toBe(AbilityMatch.match);
  });

  it('should clear the cache when the limit is reached', () => {
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
    const limit = AbilityResolver.SELECTION_CACHE_LIMIT;

    for (let i = 0; i < limit; i++) {
      resolveAny(resolver, `unknown.key${i}`, {});
    }
    expect(getCache(resolver).size).toBe(limit);

    resolver.resolve('order.update', { order: { authorId: 1 }, user: { id: 1 } });
    expect(getCache(resolver).size).toBe(1);
    expect(getCache(resolver).has('order.update')).toBeTruthy();
  });

  it('should use separate caches for resolvers with different tags', () => {
    const tagged = new AbilityDSLParser(`
    @tags admin
    permit permission.a if all:
      always
    `).parse();

    const all = new AbilityResolver(tagged, DenyOverridesStrategy);
    const users = new AbilityResolver(tagged, DenyOverridesStrategy, { tags: ['user'] });

    expect(all.resolve('a', {}).isAllowed()).toBeTruthy();
    expect(users.resolve('a', {}).isAllowed()).toBeFalsy();
  });
});
