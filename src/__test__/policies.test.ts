import AbilityPolicy, { AbilityPolicyConfig } from '../AbilityPolicy';
import AbilityMatch from '../AbilityMatch';
import AbilityResolver from '../AbilityResolver';

test('Deny for all managers, but not administrator', () => {
  type Resources = {
    ['order.status']: {
      readonly user: {
        readonly roles: readonly string[];
      };
      readonly order: {
        readonly status: string;
      }
      readonly feature: {
        readonly status: string;
      }
    }
  };

  const config: AbilityPolicyConfig = {
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
            resource: 'завершен',
            condition: '=',
          },
        ],
      },
    ],
  };

  const policy = AbilityPolicy.parse<Resources>(config);
  const result = new AbilityResolver(policy).resolve('order.status', {
    user: {
      roles: ['user', 'couch'],
    },
    order: {
      status: 'не обработан'
    },
    feature: {
      status: 'завершен'
    }
  })

  expect(result.isDeny()).toBeTruthy();
});
