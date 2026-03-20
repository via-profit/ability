import { Bench } from 'tinybench';
import {
  AbilityCompare,
  AbilityInMemoryCache,
  AbilityMatch,
  AbilityPolicy,
  AbilityPolicyEffect,
  AbilityResolver,
} from '../dist/index.js';

/**
 * @param {string} id
 */
function makePolicy(id) {
  const policy = new AbilityPolicy({
    id,
    name: id,
    action: 'order.update',
    effect: AbilityPolicyEffect.permit,
    compareMethod: AbilityCompare.and,
  });

  policy.check = async () => AbilityMatch.match;
  return policy;
}

async function main() {
  const bench = new Bench({ time: 2000, warmupIterations: 500 });

  const policies = Array.from({ length: 50 }, (_, i) => makePolicy(`p${i}`));

  const resource = { user: { id: 1 }, order: { id: 10 } };
  const environment = { time: { hour: 12 } };

  const resolverNoCache = new AbilityResolver(policies, { cache: null });
  const resolverWithCache = new AbilityResolver(policies, { cache: new AbilityInMemoryCache() });

  bench
    .add('resolve() — no cache', async () => {
      await resolverNoCache.resolve('order.update', resource, environment);
    })
    .add('resolve() — warm cache', async () => {
      await resolverWithCache.resolve('order.update', resource, environment);
    })
    .add('resolve() — cold cache', async () => {
      const resolver = new AbilityResolver(policies, {cache: new AbilityInMemoryCache() });
      await resolver.resolve('order.update', resource, environment);
    });

  await bench.run();

  console.log(bench.name);
  console.table(bench.table());
}

main();
