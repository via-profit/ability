import { Bench } from 'tinybench';
import { AbilityInMemoryCache, AbilityPolicy, AbilityResolver } from '../dist/index.js';

/**
 * @param {string} id
 */


function makeHeavyPolicy(id) {
  return new AbilityPolicy({
    id,
    name: id,
    action: 'order.update',
    effect: 'allow',
    compareMethod: 'and',
    ruleSet: [
      { field: 'order.status', op: 'eq', value: 'pending' },
      { field: 'order.total', op: 'gt', value: 100 },
      { field: 'user.role', op: 'eq', value: 'manager' },
      { field: 'order.items.length', op: 'gt', value: 2 },
      { field: 'order.customer.id', op: 'eq', value: 1 },
      { field: 'env.time.hour', op: 'gte', value: 9 },
      { field: 'env.time.hour', op: 'lte', value: 18 },
      { field: 'order.meta.flags.approved', op: 'eq', value: true },
      { field: 'order.meta.tags', op: 'contains', value: 'priority' },
      { field: 'order.meta.history.length', op: 'gt', value: 3 },
    ],
  });
}

const resource = {
  user: { id: 1, role: 'manager' },
  order: {
    id: 10,
    status: 'pending',
    total: 500,
    items: Array.from({ length: 10 }, (_, i) => ({ id: i, price: 50 })),
    customer: { id: 1 },
    meta: {
      flags: { approved: true },
      tags: ['priority', 'vip'],
      history: Array.from({ length: 10 }, (_, i) => ({ ts: Date.now() - i * 1000 })),
    },
  },
};

const environment = {
  time: { hour: 12 },
};

// ----------------------------
// 3. Создаём 50 тяжёлых политик
// ----------------------------

const policies = Array.from({ length: 50 }, (_, i) => makeHeavyPolicy(`p${i}`));

// -------

async function main() {
  const bench = new Bench({ time: 2000, warmup: 500 });

  const resolverNoCache = new AbilityResolver(policies, null);
  const resolverColdCache = new AbilityResolver(policies, new AbilityInMemoryCache());
  const resolverWarmCache = new AbilityResolver(policies, new AbilityInMemoryCache());

  // Прогреваем warm cache
  await resolverWarmCache.resolve('order.update', resource, environment);

  bench
    .add('resolve() — no cache (heavy rules)', async () => {
      await resolverNoCache.resolve('order.update', resource, environment);
    })
    .add('resolve() — cold cache (heavy rules)', async () => {
      await resolverColdCache.resolve('order.update', resource, environment);
    })
    .add('resolve() — warm cache (heavy rules)', async () => {
      await resolverWarmCache.resolve('order.update', resource, environment);
    });

  await bench.run();

  console.log(bench.name);
  console.table(bench.table());
}

main();
