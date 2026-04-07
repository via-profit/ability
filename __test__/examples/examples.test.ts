import { AbilityDSLParser, AbilityResolver } from '../../src';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

describe('Examples', () => {
  it('Example-02', () => {
    const dsl = `
    permit permission.order.* if all:
      user.authenticated equals true
    
    deny permission.order.update if all:
      user.role not equals 'admin'
    `;

    type Resources = {
      ['order.update']: {
        user: {
          authenticated: boolean;
          role: string;
        };
      };
      ['foo']: {
        bat: boolean;
        user: {
          token: string;
        };
      };
    };

    const policies = new AbilityDSLParser<Resources>(dsl).parse();
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    resolver.resolve('order.update', {
      user: { role: 'admin', authenticated: false },
    });




    // Для не-администратора – исключение
    expect(() =>
      resolver.enforce('order.update', {
        user: { authenticated: true, role: 'manager' },
      }),
    ).toThrow();

    // Для администратора – без исключения
    expect(() =>
      resolver.enforce('order.update', {
        user: { authenticated: true, role: 'admin' },
      }),
    ).not.toThrow();
  });
});
