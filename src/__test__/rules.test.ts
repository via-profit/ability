import AbilityRule, { AbilityRuleConfig, AbilityRuleState } from '../AbilityRule';

test('Match if subject.foo = resource.bar for Oleg and Oleg', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', '=', 'resource.bar']
  }).check(
    { foo: 'Oleg' },
    { bar: 'Oleg' },
  );

  expect(result).toBe<AbilityRuleState>('match');
});

test('Mismatch if subject.foo = resource.bar for Oleg and NotOleg', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', '=', 'resource.bar'],
  }).check(
    { foo: 'Oleg' },
    { bar: 'NotOleg' },
  );

  expect(result).toBe<AbilityRuleState>('mismatch');
});

test('Match if subject.foo in resource for admin and [admin]', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', 'in', 'resource'],
  }).check({ foo: 'admin' }, [
    'admin',
    'manager',
  ]);

  expect(result).toBe<AbilityRuleState>('match');
});

test('Match if subject.foo in resource for [admin] and [admin]', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', 'in', 'resource'],
  }).check({ foo: ['admin'] }, [
    'admin',
    'manager',
  ]);

  expect(result).toBe<AbilityRuleState>('match');
});

test('Mismatch if subject.foo in resource for admin and [manager]', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', 'in', 'resource'],
  }).check({ foo: 'admin' }, [
    'manager',
  ]);

  expect(result).toBe<AbilityRuleState>('mismatch');
});

test('Mismatch if subject.foo in resource for [admin] and [manager]', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', 'in', 'resource'],
  }).check({ foo: ['admin'] }, [
    'manager',
  ]);

  expect(result).toBe<AbilityRuleState>('mismatch');
});

test('Match if subject.foo in resource for 1 and [1, 2, 3]', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', 'in', 'resource'],
  }).check({ foo: 1 }, [1, 2, 3]);

  expect(result).toBe<AbilityRuleState>('match');
});

test('Mismatch if subject.foo = invalid.bar for 1 and 1', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', '=', 'invalid.bar'],
  }).check({ foo: 1 }, { bar: 1 });

  expect(result).toBe<AbilityRuleState>('mismatch');
});

test('Match if subject.foo > resource.bar for 3 and 1', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', '>', 'resource.bar']
  }).check(
    { foo: 3 },
    { bar: 1 },
  );

  expect(result).toBe<AbilityRuleState>('match');
});

test('Mismatch if subject.foo > resource.bar for 1 and 3', () => {
  const result = new AbilityRule({
    matches: ['subject.foo', '>', 'resource.bar'],
  }).check(
    { foo: 1 },
    { bar: 3 },
  );

  expect(result).toBe<AbilityRuleState>('mismatch');
});

test('Match if data have a nested properties subject.foo.bar.baz = resource.bar.taz.baz', () => {
  const result = new AbilityRule({
    matches: ['subject.foo.bar.baz', '=', 'resource.bar.taz.baz'],
  }).check(
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

  expect(result).toBe<AbilityRuleState>('match');
});

test('Match if subject.user.account.roles has roles [administrator]', () => {
  const result = new AbilityRule({
    matches: ['subject.user.account.roles', 'in', 'administrator'],
  }).check({
    user: {
      account: {
        roles: ['viewer', 'administrator', 'manager'],
      },
    },
  });

  expect(result).toBe<AbilityRuleState>('match');
});

test('Match if subject.user.age eq 21', () => {
  const result = new AbilityRule({
    matches: ['subject.user.age', '=', 21],
  }).check({
    user: {
      age: 21,
    },
  });

  expect(result).toBe<AbilityRuleState>('match');
});

test('Match if environment.deparament is NBC-news', () => {
  const result = new AbilityRule({
    matches: ['environment.departament', '=', 'NBC-news'],
  }).check(null, null, {
    departament: 'NBC-news',
  });

  expect(result).toBe<AbilityRuleState>('match');
});

test('parse and export', () => {
  const ruleConfig: AbilityRuleConfig = {
    name: 'rule',
    matches: ['subject.foo', '=', 'resource.bar'],
  };

  const config = AbilityRule.export(AbilityRule.parse(ruleConfig));

  expect(JSON.stringify(ruleConfig)).toBe(JSON.stringify(config));
});
