import { AbilityDSLParser, AbilityResolver } from '../../src';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

describe('DSL annotations', () => {
  it('Annotation disable should disable policy', () => {
    const dsl = `
      @name policy 1
      @id 1
      @priority 1
      @tags foo, bar, baz
      @tags one, two, three
      @tags tag-name
      allow permission.annotation.test if all:
        @name rule set 1
        all of:
          user.age gte 16
          user.roles contains 'admin'
          
          @disabled
          user.name equals 'Oleg'

        @name rule set 2
        any of:
          user.age gte 16
    `;

    const policies = new AbilityDSLParser(dsl).parse();

    // check name
    const policy1ByPriority = policies.find(p => p.priority === 1);
    const policy1ByName = policies.find(p => p.name === 'policy 1');
    const policy1ByID = policies.find(p => p.id === '1');

    expect(policy1ByPriority).not.toBeUndefined();
    expect(policy1ByName).not.toBeUndefined();
    expect(policy1ByID).not.toBeUndefined();

    // expect(policy1ByName);

    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    const result = resolver.resolve('annotation.test', {
      user: {
        id: '1',
        age: 17,
        roles: ['admin'],
      },
    });

    console.log(result.explain().toString());
    //
    expect(result.isAllowed()).toBeTruthy();
    expect(result.isDenied()).toBeFalsy();
  });

});
