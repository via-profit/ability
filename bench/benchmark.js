import os from 'node:os';
import { Bench } from 'tinybench';
import {
  AbilityDSLParser,
  AbilityJSONParser,
  AbilityResolver,
  AbilityTypeGenerator,
  DenyOverridesStrategy,
} from '../dist/index.js';

/**
 * Benchmarks of @via-profit/ability.
 *
 * Usage:
 *   npm run bench          - print the results table
 *   npm run bench -- --md  - print the results as a Markdown table (for README)
 */

const asMarkdown = process.argv.includes('--md');

const ACTIONS = ['create', 'read', 'update', 'delete', 'publish', 'archive', 'export', 'approve', 'comment', 'share'];

/**
 * Generates a realistic set of policies: for each permission key there is
 * a permit policy with two groups and a deny policy with an except block.
 * Plus one global wildcard policy for administrators.
 *
 * @param {number} permissionsCount - number of permission keys
 */
const generateDSL = permissionsCount => {
  let dsl = `
@name "Administrators can do everything"
permit permission.* if all:
  user.roles contains 'admin'
`;

  for (let i = 0; i < permissionsCount; i++) {
    const resource = `resource${Math.floor(i / ACTIONS.length)}`;
    const action = ACTIONS[i % ACTIONS.length];

    dsl += `
@name "Author or manager can ${action} ${resource}"
permit permission.${resource}.${action} if all:
  all of:
    ${resource}.authorId equals user.id
    ${resource}.status in ['draft', 'review', 'published']
  any of:
    user.roles contains any ['manager', 'editor']
    user.department equals ${resource}.department

@name "Locked ${resource} can not be changed"
deny permission.${resource}.${action} if all:
  ${resource}.locked is true
  except any of:
    user.roles contains 'admin'
`;
  }

  return dsl;
};

const makeResource = () => ({
  resource0: {
    authorId: 1,
    status: 'review',
    department: 'sales',
    locked: false,
  },
  user: {
    id: 1,
    roles: ['editor'],
    department: 'sales',
  },
});

const scenario = permissionsCount => {
  const dsl = generateDSL(permissionsCount);
  const policies = new AbilityDSLParser(dsl).parse();
  const json = AbilityJSONParser.toJSON(policies);
  const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

  return { dsl, policies, json, resolver };
};

const small = scenario(10);
const medium = scenario(100);
const large = scenario(1000);

// Stress test: 5000 policies on the same permission key, 10 rules each
const stressDSL = `
permit permission.order.update if all:
  order.status is equals 'pending'
  order.total greater than 100
  user.role is equals 'manager'
  order.items length greater than 2
  order.customer.id is equals 1
  env.time.hour greater than or equal 9
  env.time.hour less than or equal 18
  order.meta.flags.approved is true
  order.meta.tags contains 'priority'
  order.meta.history length greater than 3
`;
const [stressPolicy] = new AbilityDSLParser(stressDSL).parse();
const stressResolver = new AbilityResolver(
  Array.from({ length: 5000 }, (_, i) => stressPolicy.copyWith({ id: `policy-${i}` })),
  DenyOverridesStrategy,
);
const stressResource = {
  user: { id: 1, role: 'manager' },
  order: {
    status: 'pending',
    total: 500,
    items: [1, 2, 3, 4],
    customer: { id: 1 },
    meta: { flags: { approved: true }, tags: ['priority'], history: [1, 2, 3, 4] },
  },
};
const stressEnvironment = { time: { hour: 12 } };

const resource = makeResource();
const deniedResource = { ...resource, resource0: { ...resource.resource0, locked: true } };

// Sanity checks: the benchmarks must measure real decisions
if (!small.resolver.resolve('resource0.update', resource).isAllowed()) {
  throw new Error('Benchmark setup error: expected permit');
}
if (!large.resolver.resolve('resource0.update', deniedResource).isDenied()) {
  throw new Error('Benchmark setup error: expected deny');
}
if (!stressResolver.resolve('order.update', stressResource, stressEnvironment).isAllowed()) {
  throw new Error('Benchmark setup error: expected permit in stress test');
}

const bench = new Bench({ time: 1500, warmupTime: 300 });

bench
  .add(`resolve — ${small.policies.length} policies (10 permissions)`, () => {
    small.resolver.resolve('resource0.update', resource);
  })
  .add(`resolve — ${medium.policies.length} policies (100 permissions)`, () => {
    medium.resolver.resolve('resource0.update', resource);
  })
  .add(`resolve — ${large.policies.length} policies (1 000 permissions)`, () => {
    large.resolver.resolve('resource0.update', resource);
  })
  .add(`enforce, deny + throw — ${medium.policies.length} policies`, () => {
    try {
      medium.resolver.enforce('resource0.update', deniedResource);
    } catch {
      // expected
    }
  })
  .add('resolve + explainToString() — 3 policies', () => {
    small.resolver.resolve('resource0.update', resource).explainToString();
  })
  .add('resolve — 5 000 policies × 10 rules on one key (stress)', () => {
    stressResolver.resolve('order.update', stressResource, stressEnvironment);
  })
  .add(`parse DSL — ${medium.policies.length} policies`, () => {
    new AbilityDSLParser(medium.dsl).parse();
  })
  .add(`parse JSON — ${medium.policies.length} policies`, () => {
    AbilityJSONParser.parse(medium.json);
  })
  .add(`generate types — ${medium.policies.length} policies`, () => {
    new AbilityTypeGenerator(medium.policies).generateTypeDefs();
  });

await bench.run();

const formatTime = ms => {
  if (ms < 1) {
    return `${(ms * 1000).toFixed(1)} µs`;
  }

  return `${ms.toFixed(2)} ms`;
};

const formatOps = ops => Math.round(ops).toLocaleString('en-US');

const rows = bench.tasks.map(task => {
  const { latency, throughput } = task.result;

  return {
    name: task.name,
    ops: formatOps(throughput.mean),
    avg: formatTime(latency.mean),
    p99: formatTime(latency.p99),
    rme: `±${latency.rme.toFixed(2)}%`,
  };
});

const cpu = os.cpus()[0]?.model.trim() ?? 'unknown CPU';
const environment = `Node.js ${process.version}, ${cpu}, ${os.platform()} ${os.arch()}`;

if (asMarkdown) {
  console.log(`_${environment}_\n`);
  console.log('| Benchmark | ops/sec | avg | p99 |');
  console.log('|---|---:|---:|---:|');
  rows.forEach(row => {
    console.log(`| ${row.name} | ${row.ops} | ${row.avg} | ${row.p99} |`);
  });
} else {
  console.log(environment);
  console.table(rows);
}
