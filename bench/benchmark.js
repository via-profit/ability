import { Bench } from 'tinybench';
import { AbilityDSLParser, AbilityResolver, DenyOverridesStrategy } from '../dist/index.js';

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

  return policy.copyWith({
    id: `policy-${i}`,
    name: `policy-${i}`
  });
});

// -------



async function main() {
  const bench = new Bench({ time: 2000, warmup: 500 });
  const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
  bench.add('resolve policies', async () => {
    await resolver.resolve('order.update', resource, environment);
  });

  await bench.run();

  console.log(bench.name);
  console.table(bench.table());
}

main();
