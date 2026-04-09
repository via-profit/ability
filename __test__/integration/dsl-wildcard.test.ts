import { AbilityResolver } from '../../src/core/AbilityResolver';
import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import PermitOverridesStrategy from '../../src/strategy/PermitOverridesStrategy';

describe('Wildcard in the middle of permission path (DSL syntax)', () => {
  const resource = {
    order: {
      id: 1,
      meta: {
        update: true,
        history: {
          read: true,
        },
      },
    },
  };

  const environment = {
    time: { hour: 12 },
  };

  function makeResolver(dsl: string) {
    const policies = new AbilityDSLParser(dsl).parse();
    return new AbilityResolver(policies, PermitOverridesStrategy);
  }

  test('order.*.update matches order.meta.update', () => {
    const dsl = `
      permit permission.order.*.update if all:
        env.time.hour >= 0
    `;

    const resolver = makeResolver(dsl);
    const result = resolver.resolve('order.meta.update', resource, environment);

    expect(result.isAllowed()).toBe(true);
  });

  test('order.*.update does NOT match order.update (missing middle segment)', () => {
    const dsl = `
      permit permission.order.*.update if all:
        env.time.hour >= 0
    `;

    const resolver = makeResolver(dsl);
    const result = resolver.resolve('order.update', resource, environment);

    expect(result.isAllowed()).toBe(false);
  });

  test('order.*.* matches order.meta.update', () => {
    const dsl = `
      permit permission.order.*.* if all:
        env.time.hour >= 0
    `;

    const resolver = makeResolver(dsl);
    const result = resolver.resolve('order.meta.update', resource, environment);

    expect(result.isAllowed()).toBe(true);
  });

  test('order.*.history.* matches order.meta.history.read', () => {
    const dsl = `
      permit permission.order.*.history.* if all:
        env.time.hour >= 0
    `;

    const resolver = makeResolver(dsl);
    const result = resolver.resolve('order.meta.history.read', resource, environment);

    expect(result.isAllowed()).toBe(true);
  });

  test('multiple wildcard policies combine correctly', () => {
    const dsl = `
      permit permission.order.*.update if all:
        env.time.hour >= 0

      permit permission.order.*.history.* if all:
        env.time.hour >= 0
    `;

    const resolver = makeResolver(dsl);

    const r1 = resolver.resolve('order.meta.update', resource, environment);
    const r2 = resolver.resolve('order.meta.history.read', resource, environment);

    expect(r1.isAllowed()).toBe(true);
    expect(r2.isAllowed()).toBe(true);
  });
});
