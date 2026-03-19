import AbilityPolicy, { AbilityPolicyConfig } from '../../AbilityPolicy';
import AbilityResolver from '../../AbilityResolver';

describe('Complex integration scenarios', () => {
  type Resources = {
    'order.status': {
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

  it('should deny status change for non-admins', () => {
    const config: AbilityPolicyConfig = {
      id: 'policy-1',
      name: 'Deny status change for non-admins',
      action: 'order.status',
      effect: 'deny',
      compareMethod: 'and',
      ruleSet: [
        {
          id: 'rule-set-1',
          name: 'Not administrator',
          compareMethod: 'and',
          rules: [
            {
              subject: 'user.roles',
              resource: 'administrator',
              condition: 'not in' as const,
            },
          ],
        },
        {
          id: 'rule-set-2',
          name: 'Status change check',
          compareMethod: 'and',
          rules: [
            {
              subject: 'order.status',
              resource: 'не обработан',
              condition: '=' as const,
            },
            {
              subject: 'feature.status',
              resource: 'завершен',
              condition: '=' as const,
            },
          ],
        },
      ],
    };

    const policy = AbilityPolicy.parse<Resources>(config);
    const result = new AbilityResolver(policy).resolve('order.status', {
      user: { roles: ['user', 'manager'] },
      order: { status: 'не обработан' },
      feature: { status: 'завершен' },
    });

    expect(result.isDenied()).toBe(true);
    expect(result.isAllowed()).toBe(false);
  });

  it('should deny status change for a non-admins', () => {
    const config: AbilityPolicyConfig = {
      id: 'policy-1',
      name: 'Deny status change for non-admins',
      action: 'order.status',
      effect: 'deny',
      compareMethod: 'and',
      ruleSet: [
        {
          id: 'rule-set-1',
          name: 'Not administrator',
          compareMethod: 'and',
          rules: [
            {
              subject: 'user.roles',
              resource: 'administrator',
              condition: 'not in',
            },
          ],
        },
        {
          id: 'rule-set-2',
          name: 'Status change check',
          compareMethod: 'and',
          rules: [
            {
              subject: 'order.status',
              resource: 'не обработан',
              condition: '=',
            },
            {
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
      user: { roles: ['administrator'] },
      order: { status: 'не обработан' },
      feature: { status: 'завершен' },
    });

    expect(result.isDenied()).toBe(false);
    expect(result.isAllowed()).toBe(true);
  });
});
