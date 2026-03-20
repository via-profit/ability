import { AbilityResolver } from '../../core/AbilityResolver';
import { AbilityPolicy } from '../../core/AbilityPolicy';
import { AbilityMatch } from '../../core/AbilityMatch';
import { AbilityInMemoryCache } from '../../cache/AbilityInMemoryCache';
import AbilityPolicyEffect from '../../core/AbilityPolicyEffect';
import AbilityCompare from '../../core/AbilityCompare';

describe('AbilityResolver — cache integration', () => {
  const makePolicy = (id: string, result: AbilityMatch) => {
    const policy = new AbilityPolicy({
      id,
      name: id,
      action: 'test.action',
      effect: AbilityPolicyEffect.permit,
      compareMethod: AbilityCompare.and,
    });

    policy.check = jest.fn().mockResolvedValue(result);

    return policy;
  };

  const resource = { user: { id: 1 } };
  const environment = { time: { hour: 12 } };

  // policy.check() will be invoked at one time
  test('should use cache on second resolve call', async () => {
    const policy = makePolicy('p1', AbilityMatch.match);

    const resolver = new AbilityResolver([policy], {
      cache: new AbilityInMemoryCache(),
    });

    await resolver.resolve('test.action', resource, environment);
    await resolver.resolve('test.action', resource, environment);

    expect(policy.check).toHaveBeenCalledTimes(1);
  });

  /// No cache (cache = null)
  test('should not use cache when cache = null', async () => {
    const policy = makePolicy('p2', AbilityMatch.match);

    const resolver = new AbilityResolver([policy], {cache: null});

    await resolver.resolve('test.action', resource, environment);
    await resolver.resolve('test.action', resource, environment);

    expect(policy.check).toHaveBeenCalledTimes(2);
  });

  // AbilityInMemoryCache as default cache
  test('should use default AbilityInMemoryCache when cache is undefined', async () => {
    const policy = makePolicy('p3', AbilityMatch.match);

    const resolver = new AbilityResolver([policy]);

    await resolver.resolve('test.action', resource, environment);
    await resolver.resolve('test.action', resource, environment);

    expect(policy.check).toHaveBeenCalledTimes(1);
  });

  // TTl check
  test('should expire cache after TTL', async () => {
    jest.useFakeTimers();

    const cache = new AbilityInMemoryCache();
    const policy = makePolicy('p4', AbilityMatch.match);

    const resolver = new AbilityResolver([policy], { cache });

    await resolver.resolve('test.action', resource, environment);
    expect(policy.check).toHaveBeenCalledTimes(1);

    // TTL = 60 сек → истекает
    jest.advanceTimersByTime(61_000);

    await resolver.resolve('test.action', resource, environment);
    expect(policy.check).toHaveBeenCalledTimes(2);

    jest.useRealTimers();
  });


  // unique keys for the different policies
  test('should cache separately for different policies', async () => {
    const p1 = makePolicy('p1', AbilityMatch.match);
    const p2 = makePolicy('p2', AbilityMatch.match);

    const resolver = new AbilityResolver([p1, p2], { cache: new AbilityInMemoryCache() });

    await resolver.resolve('test.action', resource, environment);
    await resolver.resolve('test.action', resource, environment);

    expect(p1.check).toHaveBeenCalledTimes(1);
    expect(p2.check).toHaveBeenCalledTimes(1);
  });

  // resource and environment as a dependency of the cache
  test('should generate different cache entries for different resource/env', async () => {
    const policy = makePolicy('p6', AbilityMatch.match);

    const resolver = new AbilityResolver([policy], { cache: new AbilityInMemoryCache() });

    await resolver.resolve('test.action', { user: { id: 1 } }, { time: { hour: 10 } });
    await resolver.resolve('test.action', { user: { id: 2 } }, { time: { hour: 10 } });
    await resolver.resolve('test.action', { user: { id: 1 } }, { time: { hour: 11 } });

    expect(policy.check).toHaveBeenCalledTimes(3);
  });
});
