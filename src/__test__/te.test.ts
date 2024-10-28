import AbilityPolicy from '../AbilityPolicy';
import AbilityRule, { AbilityRuleConfig } from '../AbilityRule';
import AbilityService from '../AbilityService';

const ruleUserHasAdminPrivilege: AbilityRuleConfig = {
  name: 'subject.account.roles contains ADMINISTRATOR',
  matches: ['subject.account.roles', 'in', 'ADMINISTRATOR'],
};

const downNotHavViewerRole: AbilityRuleConfig = {
  name: '',
  matches: ['subject.account.roles', 'in', 'VIEWER'],
};

const ruleUserIsACreator: AbilityRuleConfig = {
  name: 'The subject.id is eq resource.creator',
  matches: ['subject.user.id', '=', 'resource.task.creator'],
};

const ruleRolePointgreaterTwenty: AbilityRuleConfig = {
  name: 'RolePoint greater twenty',
  matches: ['subject.account.rolePoint', '>', 20],
};

test('abc', () => {
  const user = {
    id: '<user-id-1>',
    name: 'Oleg',
    age: 21,
  };

  const account = {
    id: '<account-id-1>',
    roles: ['ADMINISTRATOR', 'VIEWER'],
    rolePoint: 59,
  };

  const task = {
    id: '<task-id-1>',
    creator: '<user-id-1>',
    status: 'unknown', // accepted | finished
  };

  const subject = {
    user,
    account,
  };
  const resource = {
    task,
  };

  const policy = AbilityPolicy.parse({
    id: '',
    name: '',
    rulesCompareMethod: 'and',
    rules: [ruleUserHasAdminPrivilege, ruleUserIsACreator, ruleRolePointgreaterTwenty],
  });

  // expect(policy.isPermit(subject, resource)).toBeTruthy();

  // expect(
  //   AbilityRule.parse(ruleUserHasAdminPrivilege).isPermit(subject, resource, environment),
  // ).toBeTruthy();

  // expect(
  //   AbilityRule.parse(ruleUserIsACreator).isPermit(subject, resource, environment),
  // ).toBeTruthy();

  // expect(
  //   AbilityRule.parse(ruleRolePointgreaterTwenty).isPermit(subject, resource, environment),
  // ).toBeTruthy();
});

test('dsd', () => {
  const user = {
    id: '<user-id-1>',
    name: 'Oleg',
    age: 21,
  };

  const account = {
    id: '<account-id-1>',
    roles: ['ADMINISTRATOR', 'VIEWER'],
    rolePoint: 59,
  };

  const task = {
    id: '<task-id-1>',
    creator: '<user-id-1>',
    status: 'unknown', // accepted | finished
  };

  const policy1 = new AbilityPolicy('name', '<id>');
  const rule1 = new AbilityRule(['subject.roles', 'in', 'ADMINISTRATOR']);

  const rule133 = new AbilityRule(['subject.departament', '=', 'resource.departament']);

  const policy2 = new AbilityPolicy('', '');
  const rule2 = new AbilityRule(['subject.id', '=', 'resource.creator']);

  const { permission } = new AbilityService().checkPolicies(
    [policy1.addRule(rule1).check(account), policy2.addRule(rule2).check(user, task)],
    'or',
  );

  // expect(permission).toBe('permit');
  expect(policy2.addPolicy(policy2).isPermit(user, task)).toBeTruthy();
});

test('dsds', () => {
  // Создание правила
  const rule = new AbilityRule([
    'subject.departament', // субъект и адрес поля, в которое записано название отдела
    '=', // оператор сравнения
    'resource.type', // Ресурс и адрес поля, в которое записан тип отчета
  ]);

  // Пользователь
  const user = {
    id: '123',
    name: 'Oleg',
    age: 26,
    departament: 'analytics',
  };

  // Пользовательские отчёты
  const reports = [
    {
      id: '1',
      type: 'analytics',
    },
    {
      id: '2',
      type: 'expenses',
    },
  ];

  expect(rule.check(user, reports[0])).toBe('permit');
  expect(rule.check(user, reports[1])).toBe('deny');
  // rule.check(user, reports[1]); // --> false
});
