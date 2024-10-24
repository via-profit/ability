import AbilityRule, { AbilityRuleStatus } from '../AbilityRule';

test('Permit if subject.foo = resource.bar for Oleg and Oleg', () => {
  const result = new AbilityRule('a', ['subject.foo', '=', 'resource.bar']).check(
    { foo: 'Oleg' },
    { bar: 'Oleg' },
  );

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Deny if subject.foo = resource.bar for Oleg and NotOleg', () => {
  const result = new AbilityRule('a', ['subject.foo', '=', 'resource.bar']).check(
    { foo: 'Oleg' },
    { bar: 'NotOleg' },
  );

  expect(result).toBe<AbilityRuleStatus>('deny');
});

test('Permit if subject.foo in resource for admin and [admin]', () => {
  const result = new AbilityRule('a', ['subject.foo', 'in', 'resource']).check(
    { foo: 'admin' },
    ['admin', 'manager'],
  );

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Permit if subject.foo in resource for [admin] and [admin]', () => {
  const result = new AbilityRule('a', ['subject.foo', 'in', 'resource']).check(
    { foo: ['admin'] },
    ['admin', 'manager'],
  );

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Deny if subject.foo in resource for admin and [manager]', () => {
  const result = new AbilityRule('a', ['subject.foo', 'in', 'resource']).check(
    { foo: 'admin' },
    ['manager'],
  );

  expect(result).toBe<AbilityRuleStatus>('deny');
});

test('Deny if subject.foo in resource for [admin] and [manager]', () => {
  const result = new AbilityRule('a', ['subject.foo', 'in', 'resource']).check(
    { foo: ['admin'] },
    ['manager'],
  );

  expect(result).toBe<AbilityRuleStatus>('deny');
});

test('Permit if subject.foo in resource for 1 and [1, 2, 3]', () => {
  const result = new AbilityRule('a', ['subject.foo', 'in', 'resource']).check(
    { foo: 1 },
    [1, 2, 3],
  );

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Deny if subject.foo = invalid.bar for 1 and 1', () => {
  const result = new AbilityRule('a', ['subject.foo', '=', 'invalid.bar']).check(
    { foo: 1 },
    { bar: 1 },
  );

  expect(result).toBe<AbilityRuleStatus>('deny');
});

test('Permit if subject.foo > resource.bar for 3 and 1', () => {
  const result = new AbilityRule('a', ['subject.foo', '>', 'resource.bar']).check(
    { foo: 3 },
    { bar: 1 },
  );

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Deny if subject.foo > resource.bar for 1 and 3', () => {
  const result = new AbilityRule('a', ['subject.foo', '>', 'resource.bar']).check(
    { foo: 1 },
    { bar: 3 },
  );

  expect(result).toBe<AbilityRuleStatus>('deny');
});

test('Permit if data have a nested properties subject.foo.bar.baz = resource.bar.taz.baz', () => {
  const result = new AbilityRule('a', [
    'subject.foo.bar.baz',
    '=',
    'resource.bar.taz.baz',
  ]).check(
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

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Permit if subject.user.account.roles has roles [administrator]', () => {
  const result = new AbilityRule('a', [
    'subject.user.account.roles',
    'in',
    'administrator',
  ]).check({
    user: {
      account: {
        roles: ['viewer', 'administrator', 'manager'],
      },
    },
  });

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Permit if subject.user.age eq 21', () => {
  const result = new AbilityRule('a', ['subject.user.age', '=', 21]).check({
    user: {
      age: 21,
    },
  });

  expect(result).toBe<AbilityRuleStatus>('permit');
});

test('Permit if environment.deparament is NBC-news', () => {
  const result = new AbilityRule('a', ['environment.departament', '=', 'NBC-news']).check(
    null,
    null,
    {
      departament: 'NBC-news',
    },
  );

  expect(result).toBe<AbilityRuleStatus>('permit');
});
