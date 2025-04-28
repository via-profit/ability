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
    }
  }

  const config: AbilityPolicyConfig = {
    name: 'Deny for all managers, but not administrator',
    effect: AbilityPolicyEffect.DENY.code,
    action: 'order.update',
    compareMethod: AbilityCompare.AND.code,
    ruleSet: [
      {
        name: 'Is a managers',
        compareMethod: AbilityCompare.OR.code,
        rules: [
          {
            name: 'The department is a managers',
            matches: ['user.department', AbilityCondition.EQUAL.code, 'managers'],
          },
          {
            name: 'The user role is a manager',
            matches: ['user.roles', AbilityCondition.IN.code, 'manager'],
          }
        ]
      },
      {
        name: 'Is not an Administrator',
        matches: ['user.roles', AbilityCondition.NOT_IN.code, 'administrator']
      }
    ]
  };

  const policy = AbilityPolicy.parse<Resources>(config);

  const result = policy.check({
    user: {
      department: 'managers',
      roles: ['manager', 'couch']
    }
  });

  expect(result).toBe(AbilityMatch.MATCH);
});


