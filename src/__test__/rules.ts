import { AbilityRuleConfig } from '../AbilityRule';

export const ruleConfig_SameName: AbilityRuleConfig = {
  name: 'Name must be equal',
  matches: ['subject.name', '=', 'resource.name'],
};

export const ruleConfig_AgeGreater21: AbilityRuleConfig = {
  name: 'Age must be greater 21',
  matches: ['subject.age', '>=', 21],
};

export const ruleConfig_IsAdmin: AbilityRuleConfig = {
  name: 'Is administrator',
  matches: ['subject.rules', 'in', 'ADMINISTRATOR'],
};

export const ruleConfig_IsDeveloper: AbilityRuleConfig = {
  name: 'Is developer',
  matches: ['subject.rules', 'in', 'DEVELOPER'],
};
