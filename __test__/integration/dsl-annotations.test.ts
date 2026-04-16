import { AbilityDSLParser, AbilityJSONParser, AbilityResolver } from '../../src';
import DenyOverridesStrategy from '../../src/strategy/DenyOverridesStrategy';

describe('DSL annotations', () => {
  it('Annotation disable should disable policy', () => {
    const dsl = `
      @name "policy 1"
      @id 1
      @priority 1
      @tags foo, bar, baz
      @tags one, two, three
      @tags tag-name
      allow permission.annotation.test if all:
        @name "rule name in implicit group"
        user.age gte 16
      
        @name "rule set 1"
        all of:
          user.age gte 16
          user.roles contains 'admin'
          
          @disabled
          user.name equals 'Oleg'

        @name "rule set 2"
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

    console.log(JSON.stringify(AbilityJSONParser.toJSON(policies), null, 2));
    // expect(policy1ByName);
    new AbilityResolver(policies, DenyOverridesStrategy, {
      tags: [''],
    });
    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

    const result = resolver.resolve('annotation.test', {
      user: {
        id: '1',
        age: 17,
        roles: ['admin'],
      },
    });
    console.log(result.explain().toString());

    // console.log(result.explain().toString());
    //
    // expect(result.isAllowed()).toBeTruthy();
    // expect(result.isDenied()).toBeFalsy();



    // const policy2 = policies.find(p => p.tags.includes('policy-2'));
    // expect(policy2).not.toBeUndefined();
    //
    // // Группа 1 — implicit
    // expect(policy2!.ruleSet[0].rules[0].name).toBe('Only owner');
    //
    // // Группа 2 — explicit
    // expect(policy2!.ruleSet[1].name).toBe('rule set A');
    // expect(policy2!.ruleSet[1].rules[0].name).toBe('rule A');
    //
    // // Группа 3 — explicit
    // expect(policy2!.ruleSet[2].rules[0].name).toBe('published');
    //
    // // Группа 4 — explicit
    // expect(policy2!.ruleSet[3].rules[0].name).toBe('disabled rule');
    // expect(policy2!.ruleSet[3].rules[0].disabled).toBe(true);

    expect(result.isAllowed()).toBeTruthy();
    expect(result.isDenied()).toBeFalsy();


  });

});
