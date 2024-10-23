import { AbilityStatementConfig } from '../AbilityStatement';

export const sameNameStatementConfig: AbilityStatementConfig = {
  name: 'Name must be equal',
  effect: 'permit',
  matches: ['subject.name', '=', 'resource.name'],
};

export const ageGreater21Config: AbilityStatementConfig = {
  name: 'Age must be greater 21',
  effect: 'permit',
  matches: ['subject.age', '>=', 21],
};
