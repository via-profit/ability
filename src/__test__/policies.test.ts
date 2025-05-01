import AbilityPolicy, { AbilityPolicyConfig } from '../AbilityPolicy';
import AbilityPolicyEffect from '../AbilityPolicyEffect';
import AbilityCondition from '../AbilityCondition';
import AbilityMatch from '../AbilityMatch';
import AbilityCompare from '../AbilityCompare';

test('Deny for all managers, but not administrator', () => {
  type Resources = {
    readonly user: {
      readonly department: string;
      readonly roles: readonly string[];
    };
  };

  const config: AbilityPolicyConfig = {
    id: 'a114e906-7d2d-41a2-82ea-7411638021e3',
    name: 'Deny for all managers, but not administrator',
    effect: AbilityPolicyEffect.DENY.code,
    action: 'order.update',
    compareMethod: AbilityCompare.AND.code,
    ruleSet: [
      {
        id: '431cb3e5-0f98-404c-8a2f-21ca65b02869',
        name: 'Is a managers',
        compareMethod: AbilityCompare.OR.code,
        rules: [
          {
            id: 'd4ec6ad3-2c2b-4031-bf01-74b8459ec9fb',
            name: 'The department is a managers',
            condition: AbilityCondition.EQUAL.code,
            subject: 'user.department',
            resource: 'managers',
          },
          {
            id: '961272ea-3901-4a65-9ac7-45f1f0f0e892',
            name: 'The user role is a manager',
            condition: AbilityCondition.IN.code,
            subject: 'user.roles',
            resource: 'manager',
          },
        ],
      },
      {
        id: '5d862132-b914-4ba6-ab8d-586dfca9a34c',
        name: 'Is not an Administrator',
        compareMethod: AbilityCompare.AND.code,
        rules: [
          {
            id: 'fcade87a-aa63-4d28-8e9d-911e6fbf3f0b',
            name: 'Has not an administrator role',
            condition: AbilityCondition.NOT_IN.code,
            subject: 'user.roles',
            resource: 'administrator',
          },
        ],
      },
    ],
  };

  const policy = AbilityPolicy.parse<Resources>(config);

  const result = policy.check({
    user: {
      department: 'managers',
      roles: ['manager', 'couch'],
    },
  });

  expect(result).toBe(AbilityMatch.MATCH);
});
