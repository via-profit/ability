import { AbilityDSLParser, AbilityResolver } from '../../src';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

describe('Examples', () => {
  it('Example-02 (environment-sensitive deny)', () => {
    const dsl = `
          @priority 2
      @name is auth user
    permit permission.order.* if all:

      user.authenticated equals true

    @name lorem ipsum
    deny permission.order.update if all:
      user.role not equals 'admin'
      env.hour lt 21
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

    type Environments = {
      ['order.update']: {
        hour: number;
      };
    };

    const policies = new AbilityDSLParser<Resources, Environments>(dsl).parse();
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    //
    // CASE 1: Менеджер, время < 21 → ДОЛЖНО быть deny
    //
    expect(() =>
      resolver.enforce(
        'order.update',
        {
          user: { authenticated: true, role: 'manager' },
        },
        { hour: 20 }, // <‑‑ environment влияет на deny
      ),
    ).toThrow();

    //
    // CASE 2: Менеджер, время >= 21 → allow (deny не срабатывает)
    //
    expect(() =>
      resolver.enforce(
        'order.update',
        {
          user: { authenticated: true, role: 'manager' },
        },
        { hour: 22 }, // <‑‑ deny не срабатывает
      ),
    ).not.toThrow();

    //
    // CASE 3: Администратор, любое время → allow
    //
    expect(() =>
      resolver.enforce(
        'order.update',
        {
          user: { authenticated: true, role: 'admin' },
        },
        { hour: 10 }, // <‑‑ env не важен
      ),
    ).not.toThrow();
  });

});
