import AbilityPolicy, { AbilityPolicyConfig } from '../AbilityPolicy';
import AbilityResolver from '../AbilityResolver';

test('Deny for all managers, but not administrator', () => {
  type Resources = {
    ['order.status']: {
      readonly user: {
        readonly roles: readonly string[];
      };
      readonly order: {
        readonly status: string;
      };
      readonly feature: {
        readonly status: string;
      };
    };
  };

  const config: AbilityPolicyConfig = {
    id: 'bb758c1b-1015-4894-ba25-d23156e063cf',
    name: 'Запрещает менять статус заявки с `не обработан` на `завершен` всем, кроме администраторам',
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
            name: 'Нет роли администраторов',
            subject: 'user.roles',
            resource: 'administrator',
            condition: 'not in',
          },
        ],
      },
      {
        id: '2f8f9d71-860b-4fa6-b395-9331f1f0848e',
        name: 'Проверка статуса `не обработан` -> `завершен`',
        compareMethod: 'and',
        rules: [
          {
            id: 'a3c7d66f-5c2d-4a24-83bc-03b0a2d9c32b',
            name: 'Текущий статус `не обработан`',
            subject: 'order.status',
            resource: 'не обработан',
            condition: '=',
          },
          {
            id: 'a3c7d66f-5c2d-4a24-83bc-03b0a2d9c32b',
            name: 'Будущий статус `завершен`',
            subject: 'feature.status',
            resource: 'завершен',
            condition: '=',
          },
        ],
      },
    ],
  };

  const policy = AbilityPolicy.parse<Resources>(config);
  const resolver = new AbilityResolver(policy);
  const explain = resolver.resolveWithExplain('order.status', {
    user: {
      roles: ['user', 'couch'],
    },
    order: {
      status: 'не обработан',
    },
    feature: {
      status: 'завершен',
    },
  });

  explain.forEach((e) => {
    console.debug(e.toString());
  });

  expect(resolver.isDeny()).toBeTruthy();
});
