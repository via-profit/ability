import { AbilityPolicyConfig } from '../AbilityPolicy';

export const PolicyConfig_CanChangeTheStatus: AbilityPolicyConfig = {
  name: 'Can change the status',
  id: '1',
  policiesCompareMethod: 'or',
  policies: [
    {
      id: '2',
      name: 'The user is the task creator and current status is «unknown» and next status is «accepted»',
      rulesCompareMethod: 'and',
      rules: [
        { name: 'User - is a creatable', matches: ['subject.id', '=', 'resource.creatable'] },
        { name: 'Prev status is «unknown»', matches: ['environment.prevStatus', '=', 'unknown'] },
        { name: 'Next status is «unknown»', matches: ['environment.nextStatus', '=', 'accepted'] },
      ],
    },
    {
      id: '3',
      rulesCompareMethod: 'or',
      name: 'Is admin or developer',
      rules: [
        {
          name: 'Is administrator',
          matches: ['environment.rules', 'in', 'ADMINISTRATOR'],
        },
        {
          name: 'Is developer',
          matches: ['environment.rules', 'in', 'DEVELOPER'],
        },
      ],
    },
  ],
};
