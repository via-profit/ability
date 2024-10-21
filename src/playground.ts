import http from 'node:http';

import AbilityService, { AbilityPolicyConfig } from './AbilityService';

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
    ruleCompareMethod: 'or',
    dummy: {
      subject: {
        accouint: {
          roles: ['<role>'],
        },
        user: {
          id: '<id>',
        },
      },
      object: {
        task: {
          responsible: '<id>',
          creator: '<id>',
        },
      },
    },
    rules: [
      [
        {
          name: 'Пользователь должен быть назначен как ответственный',
          effect: 'permit',
          matches: ['subject.user.id', '=', 'object.task.responsible'],
        },
        {
          name: 'Предыдущий статус должен быть - «unknown»',
          effect: 'permit',
          matches: ['subject.status', '=', 'unknown'],
        },
      ],
      // [
      //   {
      //     name: 'Пользователь должен являеться создателем задачи',
      //     effect: 'permit',
      //     matches: ['subject.user.id', '=', 'object.task.creator'],
      //   },
      // ],
      // [
      //   {
      //     name: 'Пользователь должен имеет роль администратора',
      //     effect: 'permit',
      //     matches: ['subject.account.roles', 'in', 'administrator'],
      //   },
      // ],
    ],
  };

  const policy = new AbilityService().parsePolicyConfig(policyConfig);

  const subject = {
    account: {
      roles: ['viewer', 'administrator', 'manager'],
    },
    user: {
      id: '1',
      age: 21,
    },
  };
  const obj = {
    task: {
      id: '6',
      responsible: '1',
    },
  };
  const { permission, deniedRules } = policy.enforce(subject, obj);

  console.log(`Permission ${permission === 'deny' ? 'denied' : 'granted'}`);

  if (deniedRules.length) {
    deniedRules.forEach(rule => {
      rule.getStatements().forEach(st => {
        console.log(`Deny by «${st.getName()}»`);
      });
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
