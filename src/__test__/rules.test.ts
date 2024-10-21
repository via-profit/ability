import AbilityRule from '../AbilityRule';
import AbilityStatement, { AbilityStatementStatus } from '../AbilityStatement';

test('The name and the same name of subject and the same age will be return Permit', () => {
  const rule = new AbilityRule([
    new AbilityStatement('1', ['subject.name', '=', 'object.name']),
    new AbilityStatement('2', ['subject.age', '=', 'object.age']),
  ]);

  const { permission } = rule.enforce(
    {
      name: 'Oleg',
      age: 26,
    },
    {
      name: 'Oleg',
      age: 26,
    },
  );

  expect(permission).toBe<AbilityStatementStatus>('permit');
});

test('The same name and age of subject gte age of the object will be return Permit', () => {
  const rule = new AbilityRule([
    new AbilityStatement('1', ['subject.name', '=', 'object.name']),
    new AbilityStatement('2', ['subject.age', '>', 'object.age']),
  ]);

  const { permission } = rule.enforce(
    {
      name: 'Oleg',
      age: 26,
    },
    {
      name: 'Oleg',
      age: 18,
    },
  );

  expect(permission).toBe<AbilityStatementStatus>('permit');
});

test('The different name and age of subject and the object will be return Deny', () => {
  const rule = new AbilityRule([
    new AbilityStatement('1', ['subject.name', '=', 'object.name']),
    new AbilityStatement('2', ['subject.age', '=', 'object.age']),
  ]);

  const { permission } = rule.enforce(
    {
      name: 'max',
      age: 26,
    },
    {
      name: 'Oleg',
      age: 26,
    },
  );

  expect(permission).toBe<AbilityStatementStatus>('deny');
});
