import AbilityPolicy from '../AbilityPolicy';

test('Permit two policies with right subject, resource and environment', () => {
  const policy = AbilityPolicy.parse({
    name: 'Right',
    id: '1',
    policiesCompareMethod: 'or',
    policies: [
      {
        id: '2',
        name: 'The user is the task creator and current status is «unknown» and next status is «accepted»',
        rulesCompareMethod: 'and',
        rules: [
          { name: 'User - is a creatable', matches: ['subject.id', '=', 'resource.creatable'] },
          { name: 'Prev status is «unknown»', matches: ['environment.prevStatus', '=', 'unknown'] },
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
    nextStatus: 'accepted',
  };

  expect(policy.isPermit(subject, resource, environment)).toBeTruthy();
});

test('Deny two policies with wrong subject, resource and environment', () => {
  const policy = AbilityPolicy.parse({
    name: 'Wrong',
    id: '1',
    policiesCompareMethod: 'or',
    policies: [
      {
        id: '2',
        name: 'The user is the task creator and current status is «unknown» and next status is «accepted»',
        rulesCompareMethod: 'and',
        rules: [
          { name: 'User - is a creatable', matches: ['subject.id', '=', 'resource.creatable'] },
          { name: 'Prev status is «unknown»', matches: ['environment.prevStatus', '=', 'unknown'] },
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

  expect(policy.isDeny(subject, resource, environment)).toBeTruthy();
});
