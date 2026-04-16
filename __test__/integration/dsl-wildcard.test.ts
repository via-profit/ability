import { AbilityResolver } from '../../src/core/AbilityResolver';
import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

describe('Wildcard in the middle of permission path (DSL syntax)', () => {
  function makeResolver(dsl: string) {
    const policies = new AbilityDSLParser(dsl).parse();
    return new AbilityResolver(policies, DenyOverridesStrategy);
  }



  test('multiple wildcard policies combine correctly', () => {
    const dsl = `
    
      alias isDeveloper:
        user.roles contains 'developer'
      
      alias isAdministrator:
        user.roles contains 'administrator'
      
      @name "Разработчик может всё"
      permit permission.* if all:
       isDeveloper
      

      @name "Статистика заявок доступна только администратору"
      permit permission.analytics.orders.view if any:
        isAdministrator
        

    `;

    const resolver = makeResolver(dsl);

    const r1 = resolver.resolve('analytics.orders.view', {
      user: {
        roles: ['developer'],
      },
    });

    console.log(r1.explain().toString());
    expect(r1.isAllowed()).toBeTruthy();
  });
});
