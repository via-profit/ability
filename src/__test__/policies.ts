import { AbilityPolicyConfig } from '../AbilityPolicy';
import { ageGreater21Config, sameNameStatementConfig } from './statements';

export const sameNameAndGreater21YearsPolicyConfig: AbilityPolicyConfig = {
  name: 'Age must be greater 21',
  id: '<id-2>',
  rules: {
    compareMethod: 'and',
    list: [
      {
        name: '',
        statements: {
          compareMethod: 'and',
          list: [ageGreater21Config, sameNameStatementConfig],
        },
      },
    ],
  },
};


