import AbilityPolicy from '../AbilityPolicy';
import AbilityRule from '../AbilityRule';
import AbilityStatement, { AbilityStatementStatus } from '../AbilityStatement';

test('Permit if bane and the age are equal', () => {
  const policy = new AbilityPolicy('a').addRule(
    new AbilityRule([
      new AbilityStatement('ds', ['subject.name', '=', 'resource.name']),
      new AbilityStatement('ds', ['subject.age', '=', 'resource.age']),
    ]),
    'or',
  );

  const { permission } = policy.enforce(
    {
      name: 'Oleg',
      age: 28,
    },
    {
      name: 'Oleg',
      age: 28,
    },
  );

  expect(permission).toBe<AbilityStatementStatus>('permit');
});

test('Permit if resource is undefined, but compare by value', () => {
  const policy = new AbilityPolicy('').addRule(
    new AbilityRule([new AbilityStatement('ds', ['subject.age', '=', 28])]),
    'or',
  );

  const { permission } = policy.enforce({
    name: 'Oleg',
    age: 28,
  });

  expect(permission).toBe<AbilityStatementStatus>('permit');
});
