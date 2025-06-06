import http from 'node:http';
import { AbilityPolicy, AbilityPolicyConfig } from './AbilityPolicy';
import AbilityCondition from '~/AbilityCondition';
import AbilityRule from './AbilityRule';
import AbilityMatch from './AbilityMatch';
import AbilityRuleSet from './AbilityRuleSet';
import AbilityCompare from './AbilityCompare';
import AbilityResolver from './AbilityResolver';

const server = http.createServer();

server.on('request', (_req, res) => {
  type Resources = {
    ['order.status']: { // <-- название экшена
      readonly account: { // <-- данные ресурса
        readonly roles: readonly string[];
      };
    };
    ['order.create']: {
      readonly user: {
        readonly department: string;
      };
    };
  };



  const config: AbilityPolicyConfig[] = [
    {
      id: 'bb758c1b-1015-4894-ba25-d23156e063cf',
      name: 'Status hui',
      action: 'order.status',
      effect: 'deny',
      compareMethod: 'and',
      ruleSet: [
        {
          id: '9cc009e5-0aa9-453a-a668-cb3f418ced92',
          name: 'Не администратор',
          compareMethod: 'and',
          rules: [
            {
              id: '4093cd50-e54f-4062-8053-2d3b5966fad3',
              name: 'Нет роли администраторв',
              subject: 'account.roles',
              resource: 'administrator',
              condition: '<>',
            },
          ],
        },
      ],
    },
    {
      id: 'bb758c1b-1015-4894-ba25-d23156e063cf',
      name: 'Status hui 1',
      action: 'order.status',
      effect: 'permit',
      compareMethod: 'and',
      ruleSet: [
        {
          id: '2f8f9d71-860b-4fa6-b395-9331f1f0848e',
          name: 'Rule set',
          compareMethod: 'and',
          rules: [
            {
              id: 'a3c7d66f-5c2d-4a24-83bc-03b0a2d9c32b',
              name: 'Rule 1',
              subject: 'order.status',
              resource: 'не обработан',
              condition: '=',
            },
            {
              id: 'a3c7d66f-5c2d-4a24-83bc-03b0a2d9c32b',
              name: 'Rule 2',
              subject: 'feature.status',
              resource: 'отменен',
              condition: '=',
            },
          ],
        },
      ],
    },
  ];

  const policies: AbilityPolicy<Resources>[] = config.map(cfg => AbilityPolicy.parse(cfg));

  new AbilityResolver(policies).enforce('order.create', {
    user: {department: 'managers'},
  });

  new AbilityResolver(policies).enforce('order.status', {
    account: {roles: ['administrator']},
  });

  const ruleSet = new AbilityRuleSet({
    id: '<set-id>',
    name: '',
    compareMethod: AbilityCompare.and,
  });

  ruleSet.addRules([...]);

  ruleSet.check()

  AbilityRuleSet.parse({
    "id": "<set-id>",
    "name": "",
    "compareMethod": "and",
    "rules": [
      {
        "id": "<rule-id>",
        "name": "Пользователь из отдела managers",
        "subject": "user.department",
        "resource": "managers",
        "condition": "="
      }
    ]
  });








  const rule = new AbilityRule({
    id: '<rule-id>',
    name: 'Пользователь является владельцем заказа',
    condition: AbilityCondition.equal,
    subject: 'user.id',
    resource: 'order.owner',
  });

  const matchState = rule.check({
    user: { id: '1' },
    order: { owner: '1' },
  });

  const is = matchState.isEqual(AbilityMatch.match); // true

  const policy = AbilityPolicy.parse(config[0]);
  policy.c

  res.statusCode = 200;
  res.setHeader('content-type', 'application/json');

  res.write(
    JSON.stringify({
      status: 'ok',
    }),
  );
  res.end();
});

server.listen(8081, 'localhost', () => {
  console.debug('server started at http://localhost:8081');
});

export type Resources = {
  readonly ['account.read']: {
    readonly account: {
      readonly id: string;
      readonly roles: readonly string[];
    } | null;
    readonly resource: {
      readonly id: string;
    };
  };
  readonly ['access.auth']: {
    readonly token: {
      readonly type: string;
      readonly id: string;
    } | null;
  };
  readonly ['order.update']: {
    readonly account: {
      readonly roles: readonly string[];
      readonly department: string;
    } | null;
    readonly order: {
      readonly status: string;
    } | null;
  };
};
