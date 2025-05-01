import AbilityRule from '../AbilityRule';
import AbilityCondition from '../AbilityCondition';
import AbilityMatch from '../AbilityMatch';


test('Match if subject.foo = resource.bar for Oleg and Oleg', () => {
  const result = new AbilityRule({
    id: '1',
    name: 'Match if subject.foo = resource.bar for Oleg and Oleg',
    subject: 'user.name',
    resource: 'opponent.name',
    condition: AbilityCondition.EQUAL.code,
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
