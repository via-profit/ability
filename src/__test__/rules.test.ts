import AbilityRule from '../AbilityRule';
import AbilityStatement, { AbilityStatementStatus } from '../AbilityStatement';

test('Rule name must be returns in deniedRules array', () => {
  const RULE_NAME = Symbol('The name of rule');
  const STATEMENT_1_NAME = Symbol('St 1');
  const STATEMENT_2_NAME = Symbol('St 2');
  const rule = new AbilityRule(RULE_NAME).addStatements(
    [
      new AbilityStatement(STATEMENT_1_NAME, ['subject.name', '=', 'resource.name']),
      new AbilityStatement(STATEMENT_2_NAME, ['subject.age', '=', 'resource.age']),
    ],
    'and',
  );

  const { permission, deniedStatements } = rule.check({ name: 'A', age: 21 }, { name: 'B' });

  expect(permission).toBe('deny');
  expect(deniedStatements[0].getName()).toBe(STATEMENT_1_NAME);
  expect(deniedStatements[1].getName()).toBe(STATEMENT_2_NAME);
});

test('The name and the same name of subject and the same age will be return Permit', () => {
  const rule = new AbilityRule('Rule name').addStatements([
    new AbilityStatement('1', ['subject.name', '=', 'resource.name']),
    new AbilityStatement('2', ['subject.age', '=', 'resource.age']),
  ]);

  const { permission } = rule.check(
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

test('The same name and age of subject gte age of the resource will be return Permit', () => {
  const rule = new AbilityRule('Rule name').addStatements([
    new AbilityStatement('1', ['subject.name', '=', 'resource.name']),
    new AbilityStatement('2', ['subject.age', '>', 'resource.age']),
  ]);

  const { permission } = rule.check(
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

test('The different name and age of subject and the resource will be return Deny', () => {
  const rule = new AbilityRule('Rule name').addStatements([
    new AbilityStatement('1', ['subject.name', '=', 'resource.name']),
    new AbilityStatement('2', ['subject.age', '=', 'resource.age']),
  ]);

  const { permission } = rule.check(
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
