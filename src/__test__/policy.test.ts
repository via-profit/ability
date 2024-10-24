import AbilityPolicy from '../AbilityPolicy';
import { PolicyConfig_CanChangeTheStatus } from './policies';

test('Permit two policies: compare names and the age', () => {
  const policy = AbilityPolicy.parse(PolicyConfig_CanChangeTheStatus);
  const subject = {
    name: 'Ivan',
    id: 'df0fa44a-c92f-4062-aed6-20f1e10bdf9d',
  };
  const resource = {
    id: '48cd6772-011a-4335-918c-8f6304507682',
    creatable: 'df0fa44a-c92f-4062-aed6-20f1e10bdf9d',
  };
  const environment = {
    roles: [
      'VIEWER',
    ],
    prevStatus: 'unknown',
    nextStatus: 'accepted',
  };

  expect(policy.isPermit(subject, resource, environment)).toBeTruthy();
});
