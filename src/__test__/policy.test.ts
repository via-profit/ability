import AbilityPolicy, { AbilityPolicyConfig } from '../AbilityPolicy';
import AbilityRule from '../AbilityRule';
import AbilityStatement, { AbilityStatementStatus } from '../AbilityStatement';

import { sameNameAndGreater21YearsPolicyConfig } from './policies';

test('Throw Error while policies and rules are empty', () => {
  const policy = AbilityPolicy.parse({
    id: '<id>',
    name: '<name>',
  });

  expect(() => policy.enforce({}, {})).toThrow(Error);
});

test('Permit two policies: compare names and the age', () => {
  const policy = AbilityPolicy.parse(sameNameAndGreater21YearsPolicyConfig);

  expect(policy.isPermit({ name: 'Oleg', age: 21 }, { name: 'Oleg' })).toBeTruthy();
});

// test('Permit if resource is undefined, but compare by value', () => {
//   const policy = new AbilityPolicy('<name>', '<id>').addRule(
//     new AbilityRule('s').addStatement(new AbilityStatement('ds', ['subject.age', '=', 28])),
//     'or',
//   );

//   const { permission } = policy.check({
//     name: 'Oleg',
//     age: 28,
//   });

//   expect(permission).toBe<AbilityStatementStatus>('permit');
// });
