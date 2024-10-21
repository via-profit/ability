import AbilityStatement, { AbilityStatementStatus } from '../AbilityStatement';

test('Permit if subject.foo = object.bar for Oleg and Oleg', () => {
  const result = new AbilityStatement('a', ['subject.foo', '=', 'object.bar']).enforce(
    { foo: 'Oleg' },
    { bar: 'Oleg' },
  );

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Deny if subject.foo = object.bar for Oleg and NotOleg', () => {
  const result = new AbilityStatement('a', ['subject.foo', '=', 'object.bar']).enforce(
    { foo: 'Oleg' },
    { bar: 'NptOleg' },
  );

  expect(result).toBe<AbilityStatementStatus>('deny');
});

test('Permit if subject.foo in object for admin and [admin]', () => {
  const result = new AbilityStatement('a', ['subject.foo', 'in', 'object']).enforce(
    { foo: 'admin' },
    ['admin', 'manager'],
  );

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Permit if subject.foo in object for [admin] and [admin]', () => {
  const result = new AbilityStatement('a', ['subject.foo', 'in', 'object']).enforce(
    { foo: ['admin'] },
    ['admin', 'manager'],
  );

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Deny if subject.foo in object for admin and [manager]', () => {
  const result = new AbilityStatement('a', ['subject.foo', 'in', 'object']).enforce(
    { foo: 'admin' },
    ['manager'],
  );

  expect(result).toBe<AbilityStatementStatus>('deny');
});

test('Deny if subject.foo in object for [admin] and [manager]', () => {
  const result = new AbilityStatement('a', ['subject.foo', 'in', 'object']).enforce(
    { foo: ['admin'] },
    ['manager'],
  );

  expect(result).toBe<AbilityStatementStatus>('deny');
});

test('Permit if subject.foo in object for 1 and [1, 2, 3]', () => {
  const result = new AbilityStatement('a', ['subject.foo', 'in', 'object']).enforce(
    { foo: 1 },
    [1, 2, 3],
  );

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Deny if subject.foo = invalid.bar for 1 and 1', () => {
  const result = new AbilityStatement('a', ['subject.foo', '=', 'invalid.bar']).enforce(
    { foo: 1 },
    { bar: 1 },
  );

  expect(result).toBe<AbilityStatementStatus>('deny');
});

test('Permit if subject.foo > object.bar for 3 and 1', () => {
  const result = new AbilityStatement('a', ['subject.foo', '>', 'object.bar']).enforce(
    { foo: 3 },
    { bar: 1 },
  );

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Deny if subject.foo > object.bar for 1 and 3', () => {
  const result = new AbilityStatement('a', ['subject.foo', '>', 'object.bar']).enforce(
    { foo: 1 },
    { bar: 3 },
  );

  expect(result).toBe<AbilityStatementStatus>('deny');
});

test('Throw Error if subject prefix missing', () => {
  expect(() =>
    new AbilityStatement('a', ['foo', '=', 'object.bar']).enforce({ foo: 1 }, { bar: 1 }),
  ).toThrow(Error);
});

test('Throw Error if subject is not an object', () => {
  expect(() =>
    new AbilityStatement('a', ['subject.foo', '=', 'object.bar']).enforce(1, { bar: 1 }),
  ).toThrow(Error);
});

test('Permit if data have a nested properties subject.foo.bar.baz = object.bar.taz.baz', () => {
  const result = new AbilityStatement('a', [
    'subject.foo.bar.baz',
    '=',
    'object.bar.taz.baz',
  ]).enforce(
    {
      foo: {
        bar: {
          baz: 'value',
        },
      },
    },
    {
      bar: {
        taz: { baz: 'value' },
      },
    },
  );

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Permit if subject.user.account.roles has roles [administrator]', () => {
  const result = new AbilityStatement('a', [
    'subject.user.account.roles',
    'in',
    'administrator',
  ]).enforce({
    user: {
      account: {
        roles: ['viewer', 'administrator', 'manager'],
      },
    },
  });

  expect(result).toBe<AbilityStatementStatus>('permit');
});

test('Permit if subject.user.age eq 21', () => {
  const result = new AbilityStatement('a', ['subject.user.age', '=', 21]).enforce({
    user: {
      age: 21,
    },
  });

  expect(result).toBe<AbilityStatementStatus>('permit');
});
