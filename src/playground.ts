import http from 'node:http';

import AbilityService, { AbilityPolicyConfig } from './AbilityService';
// import AbilityRule from './AbilityRule';
// import AbilityStatement from './AbilityStatement';

// const policy = new AbilityService().parsePolicyConfig({
//   policyID: 'BUGTRACKER_CAN_CHANGE_TO_ACCEPTED_STATUS',
//   policyName: 'Bugtracker. Возможность сменить статус на «accepted»',
//   info: {
//     subject: 'User',
//     object: 'BugtrackerTask',
//     target: 'BugtrackerTaskStatus',
//     environments: {
//       prevStatus: 'BugtrackerTaskStatus',
//       nextStatus: 'BugtrackerTaskStatus',
//     },
//   },
//   ruleCompareMethod: 'or',
//   rules: [
//     // [
//     //   {
//     //     name: 'Разрешено, если пользователь назначен как ответственный',
//     //     effect: 'permit',
//     //     matches: ['subject.id', '=', 'object.responsible'],
//     //   },
//     //   {
//     //     name: 'Разрешено, если предыдущий статус - «unknown»',
//     //     effect: 'permit',
//     //     matches: ['subject.id', '=', 'object.responsible'],
//     //   },
//     // ],
//     [
//       {
//         name: 'Разрешено, если пользователь является создателем задачи',
//         effect: 'permit',
//         matches: ['subject.id', '=', 'object.creator'],
//       },
//     ],
//     [
//       {
//         name: 'Разрешено, если пользователь имеет роль администратора',
//         effect: 'permit',
//         matches: ['subject.roles', 'in', 'object.roles'],
//       },
//     ],
//   ],
// });

const server = http.createServer();
server.on('request', (_, res) => {
  // const hasRoleAdministrator = new AbilityStatement('Has role «administrator»', [
  //   'subject.user.account.roles',
  //   'in',
  //   'administrator',
  // ]);

  // const hasAgeGreatherTwentyOne = new AbilityStatement('Have an age greater than twenty One', [
  //   'subject.user.age',
  //   '>=',
  //   21,
  // ]);

  // const policy = new AbilityPolicy('Admin, 21 y.o.').addRule(
  //   new AbilityRule([hasRoleAdministrator, hasAgeGreatherTwentyOne]),
  //   'or',
  // );

  const policyConfig: AbilityPolicyConfig = {
    policyID: 'BUGTRACKER_CAN_CHANGE_TO_ACCEPTED_STATUS',
    policyName: 'Bugtracker. Возможность сменить статус на «accepted»',
    target: 'BugtrackerTaskStatus',
    ruleCompareMethod: 'and',
    rules: [
      [
        {
          name: 'Пользователь должен быть назначен как ответственный',
          effect: 'permit',
          matches: ['subject.id', '=', 'object.responsible'],
        },
        // {
        //   name: 'Предыдущий статус должен быть - «unknown»',
        //   effect: 'permit',
        //   matches: ['environment.task.status', '=', 'unknown'],
        // },
      ],
      [
        {
          name: 'Пользователь должен являеться создателем задачи',
          effect: 'permit',
          matches: ['subject.id', '=', 'object.creator'],
        },
      ],
      // [
      //   {
      //     name: 'Пользователь должен имеет роль администратора',
      //     effect: 'permit',
      //     matches: ['environment.account.roles', 'in', 'administrator'],
      //   },
      // ],
    ],
  };

  // const a = new AbilityService().createPolicy('ds').addRule(
  //   new AbilityRule([
  //     new AbilityStatement('saa', ['subject.id', '=', ''])
  //   ])
  // )

  const policy = new AbilityService().parsePolicyConfig(policyConfig);

  const subject = {
    id: '1',
    age: 21,
  };
  const obj = {
    id: '6',
    responsible: '1',
    creator: '1',
  };

  const env = {
    task: {
      status: 'unknown',
    },
    account: {
      roles: ['viewer', 'administrator', 'manager'],
    },
  };
  const { permission, deniedStatements } = policy.enforce(subject, obj, env);

  console.log(`Permission ${permission === 'deny' ? 'denied' : 'granted'}`);

  if (permission === 'deny') {
    deniedStatements.forEach(statement => {
      console.log(`Deny by «${statement.getName()}»`);
    });
  }

  res.statusCode = 200;
  res.setHeader('content-type', 'application/json');
  res.write(JSON.stringify({ status: 'OK' }));
  res.end();
});

server.listen(8080, 'localhost', () => {
  console.info('server started at http://localhost:8080');
});
