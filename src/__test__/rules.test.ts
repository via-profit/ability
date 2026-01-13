import AbilityRule from '../AbilityRule';
import AbilityMatch from '../AbilityMatch';
import AbilityRuleSet from '../AbilityRuleSet';

test('Match if subject.foo = resource.bar for Oleg and Oleg', () => {
  const rule = AbilityRule.equal('user.name', 'opponent.name');
  const rule2 = AbilityRule.moreThan('user.age', 'opponent.age');

  const ruleSet = AbilityRuleSet.and([rule, rule2]);

  const result = ruleSet.check({
    user: {
      name: 'Oleg',
      age: 32,
    },
    opponent: {
      name: 'Oleg',
      age: 42,
    },
  });

  expect(result).toBe(AbilityMatch.match);
});
