import http from 'node:http';
import AbilityPolicy, { AbilityPolicyConfig } from './AbilityPolicy';
import AbilityCondition from './AbilityCondition';
import AbilityPolicyEffect from './AbilityPolicyEffect';
import AbilityCompare from '~/AbilityCompare';
import AbilityResolver from '~/AbilityResolver';

const server = http.createServer();

type Resource = {
  readonly user: { readonly department: string; readonly roles: string[] };
  readonly order: { readonly estimatedArrivalAt: number };
};

const resource: Resource = {
  user: { department: 'managers', roles: ['logistic'] },
  order: { estimatedArrivalAt: 8 },
};

server.on('request', (_req, res) => {
  const policyConfig1: AbilityPolicyConfig = {
    action: 'order.update',
    name: 'Менеджеры не могут редактировать заказы, созданные более 3-х дней назад, а так же завершенные заявки',
    compareMethod: AbilityCompare.AND.code,
    effect: AbilityPolicyEffect.DENY.code,
    ruleSet: [
      {
        name: 'Менеджеры',
        compareMethod: AbilityCompare.OR.code,
        rules: [
          {
            name: 'отдел - менеджеры',
            matches: ['user.department', AbilityCondition.EQUAL.code, 'managers'],
          },
          {
            name: 'роль - менеджер',
            matches: ['user.roles', AbilityCondition.IN.code, 'manager'],
          },
        ],
      },
      {
        name: 'заказ создан более 3-х дней назад',
        matches: ['order.estimatedArrivalAt', AbilityCondition.MORE_THAN.code, 3],
      },
    ],
  };

  const policy2Config: AbilityPolicyConfig = {
    name: 'Developer может всё',
    action: '*',
    effect: AbilityPolicyEffect.PERMIT.code,
    compareMethod: AbilityCompare.AND.code,
    ruleSet: [
      {
        name: 'Developer может всё',
        matches: ['user.roles', AbilityCondition.IN.code, 'developer'],
      },
    ],
  };

  const policy1 = AbilityPolicy.parse(policyConfig1);
  const policy2 = AbilityPolicy.parse(policy2Config);

  const resolver = AbilityResolver.resolve([policy1, policy2], resource, 'order.update');

  const policy = resolver.getPolicy();

  if (policy !== null) {
    const a = {
      effect: policy.effect,
      message: policy.name,
    };
    console.log(a);
  }

  res.statusCode = 200;
  res.setHeader('content-type', 'application/json');

  res.write(
    JSON.stringify({
      status: 'Done',
    }),
  );
  res.end();
});

server.listen(8080, 'localhost', () => {
  console.debug('server started at http://localhost:8080');
});
