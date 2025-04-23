import AbilityRule from '../AbilityRule';
import AbilityCondition from '../AbilityCondition';
import AbilityMatch from '../AbilityMatch';

test('Match if subject.foo = resource.bar for Oleg and Oleg', () => {
  const result = new AbilityRule({
    matches: ['user.name', AbilityCondition.EQUAL, 'opponent.name'],
  }).check({
    user: {
      name: 'Oleg',
    },
    opponent: {
      name: 'Oleg',
    },
  });

  expect(result).toBe(AbilityMatch.MATCH);
});
