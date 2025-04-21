import AbilityPolicy from '../AbilityPolicy';

test('Permit two policies with right subject, resource and environment', () => {
  const policy = AbilityPolicy.parse({
    policiesCompareMethod: 'or',
    rulesCompareMethod: 'and',
    effect: 'permit',
    policies: [
      // {
      //   id: '2',
      //   name: 'The user is the task creator and current status is «unknown» and next status is «accepted»',
      //   rulesCompareMethod: 'and',
      //   effect: 'permit',
      //   rules: [
      //     {
      //       name: 'User - is a creatable',
      //       matches: ['subject.id', '=', 'resource.creatable'],
      //       effect: 'permit',
      //     },
      //     {
      //       name: 'Prev status is «unknown»',
      //       matches: ['environment.prevStatus', '=', 'unknown'],
      //       effect: 'permit',
      //     },
      //     {
      //       name: 'Next status is «unknown»',
      //       matches: ['environment.nextStatus', '=', 'accepted'],
      //       effect: 'permit',
      //     },
      //   ],
      // },
      {
        id: '3',
        rulesCompareMethod: 'or',
        policiesCompareMethod: 'and',
        name: 'Is admin or developer',
        effect: 'permit',
        rules: [
          {

            name: 'Is administrator',
            matches: ['environment.roles', 'in', 'ADMINISTRATOR'],
          },
          {
            name: 'Is developer',
            matches: ['environment.roles', 'in', 'DEVELOPER'],
          },
        ],
      },
    ],
  });
  const subject = {
    name: 'Ivan',
    id: 'df0fa44a-c92f-4062-aed6-20f1e10bdf9d',
  };
  const resource = {
    id: '48cd6772-011a-4335-918c-8f6304507682',
    creatable: 'df0fa44a-c92f-4062-aed6-20f1e10bdf9d',
  };
  const environment = {
    roles: ['ADMINISTRATOR'],
    prevStatus: 'unknown',
    nextStatus: 'accepted',
  };

  expect(policy.check(subject, resource, environment).permission).toBe('permit');
});

test('Deny two policies with wrong subject, resource and environment', () => {
  const policy = AbilityPolicy.parse({
    name: 'Wrong',
    id: '1',
    policiesCompareMethod: 'or',
    effect: 'deny',
    policies: [
      {
        id: '2',
        name: 'The user is the task creator and current status is «unknown» and next status is «accepted»',
        rulesCompareMethod: 'and',
        effect: 'deny',
        rules: [
          {
            name: 'User - is a creatable',
            matches: ['subject.id', '=', 'resource.creatable'],

          },
          {
            name: 'Prev status is «unknown»',
            matches: ['environment.prevStatus', '=', 'unknown'],

          },
          {
            name: 'Next status is «unknown»',
            matches: ['environment.nextStatus', '=', 'accepted'],
          },
        ],
      },
      {
        id: '3',
        rulesCompareMethod: 'or',
        name: 'Is admin or developer',
        effect: 'permit',
        rules: [
          {
            name: 'Is administrator',
            matches: ['environment.rules', 'in', 'ADMINISTRATOR'],
          },
          {
            name: 'Is developer',
            matches: ['environment.rules', 'in', 'DEVELOPER'],
          },
        ],
      },
    ],
  });
  const subject = {
    name: 'Ivan',
    id: 'df0fa44a-c92f-4062-aed6-20f1e10bdf9d',
  };
  const resource = {
    id: '48cd6772-011a-4335-918c-8f6304507682',
    creatable: 'df0fa44a-c92f-4062-aed6-20f1e10bdf9d',
  };
  const environment = {
    roles: ['VIEWER'],
    prevStatus: 'unknown',
    // nextStatus: 'accepted',
    nextStatus: 'finished',
  };

  expect(policy.check(subject, resource, environment).permission).toBe('deny');
});
//
// test('parse and export', () => {
//   const policyConfig: AbilityPolicyConfig = {
//     id: '1',
//     name: 'policy',
//     rulesCompareMethod: 'and',
//     policiesCompareMethod: 'or',
//     policies: [],
//     rules: [
//       {
//         name: 'rule',
//         effect: 'permit',
//         matches: ['subject.foo', '=', 'resource.bar'],
//       },
//       {
//         name: 'rule',
//         effect: 'permit',
//         matches: ['subject.baz', '=', 'resource.taz'],
//       },
//     ],
//   };
//
//   const config = AbilityPolicy.export(AbilityPolicy.parse(policyConfig));
//
//   expect(JSON.stringify(policyConfig.policiesCompareMethod)).toBe(
//     JSON.stringify(config.policiesCompareMethod),
//   );
// });
