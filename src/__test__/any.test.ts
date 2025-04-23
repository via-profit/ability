import AbilityPolicy from '../AbilityPolicy';


test('Менеджеры не могут редактировать заказы, созданные более 3-х дней назад, а так же завершенные заявки', () => {
  const policy = AbilityPolicy.parse({
    id: '1121',
    name: 'Менеджеры не могут редактировать заказы, созданные более 3-х дней назад, а так же завершенные заявки',
    influence: 'order.update',
    effect: 'deny',
    rulesCompareMethod: 'and',
    ruleSet: [
      {
        name: 'заказ создан более 3-х дней назад',
        matches: ['order.estimatedArrivalAt', '>', 3],
      },
      {
        name: 'все из отдела менеджеров',
        matches: ['user.department', 'in', 'managers'],
      },
    ],
  });

  const subject = {
    user: {
      id: '964daead-3c73-433c-ada9-6dd39047cdc8',
      name: 'Developer',
      roles: ['logistics'],
      department: ['managers'],
    },
    order: {
      estimatedArrivalAt:  8,
      status: 'zalupa',
    },
  }

  const permission = policy.check(subject).permission;

  expect(permission).toBe('deny');
});



