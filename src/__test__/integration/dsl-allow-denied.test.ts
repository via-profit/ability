import { AbilityDSLParser } from '~/parsers/dsl/AbilityDSLParser';
import AbilityResolver from '../../core/AbilityResolver';

describe('DSL isAllowed/isDenied test', () => {
  it('should isAllowed be true, isDenied be false', async () => {
    const dsl = `
      permit permission.test if all:
        user.age is equals 21
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('permission.test', {
      user: { age: 21 },
    });

    expect(result.isAllowed()).toBeTruthy();
    expect(result.isDenied()).toBeFalsy();
  });

  it('should isAllowed be false, isDenied be true', async () => {
    const dsl = `
      permit permission.test if all:
        user.age is equals 21
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('permission.test', {
      user: { age: 0 },
    });

    expect(result.isAllowed()).toBeFalsy();
    expect(result.isDenied()).toBeTruthy();
  });

  it('should isDenied be true, isAllowed be false', async () => {
    const dsl = `
      deny permission.test if all:
        user.age is equals 21
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('permission.test', {
      user: { age: 21 },
    });

    expect(result.isDenied()).toBeTruthy();
    expect(result.isAllowed()).toBeFalsy();
  });

  it('should isDenied be false, isAllowed be true', async () => {
    const dsl = `
      deny permission.test if all:
        user.age is equals 16
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('permission.test', {
      user: { age: 12, token: 'token-value' },
    });

    expect(result.isDenied()).toBeTruthy();
    expect(result.isAllowed()).toBeFalsy();
  });
});
