import { Bench } from 'tinybench';
import {
  AbilityInMemoryCache,
  AbilityResolver,
  AbilityDSLParser,
  AbilityPolicy,
} from '../dist/index.js';

/**
 * @param {string} id
 */


const dsl = `
# @name Allow order.update with complex conditions
permit permission.order.update if all:

  # @name Order status is pending
  order.status is equals 'pending'

  # @name Order total > 100
  order.total greater than 100

  # @name User is manager
  user.role is equals 'manager'

  # @name Order has more than 2 items
  order.items.length greater than 2

  # @name Customer ID is 1
  order.customer.id is equals 1

  # @name Time >= 9
  env.time.hour greater than or equal 9

  # @name Time <= 18
  env.time.hour less than or equal 18

  # @name Order approved flag is true
  order.meta.flags.approved is true

  # @name Tags contain "priority"
  order.meta.tags contains 'priority'

  # @name History length > 3
  order.meta.history.length greater than 3

`;

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
const [policy] = new AbilityDSLParser(dsl).parse();
const policies = Array.from({ length: 50 }, (_, i) => {

  return AbilityPolicy.fromJSON({
    ...policy.toJSON(),
    id: `policy-${i}`,
    name: `policy-${i}`
  });
});

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
