import { AbilityDSLParser, AbilityTypeGenerator } from '../../src';

const dsl = `
# Пользователь должен быть аутентифицирован (токен обязателен)
@name AuthenticationRequired
@tags mutation
allow permission.mut.* if all:
  token.id not equals 'NOT_ASSIGNED'
  token.type is equals 'access'


@name OrderDeleteAccess
@tags mutation, order
permit permission.mut.order.delete if all:
  account.roles contains 'administrator'
  account.roles contains 'developer'
  env.hour is equals 12
  
@name ClientDeleteAccess
@tags mutation, client
permit permission.mut.client.delete if all:
  account.roles contains 'viewer'
  account.roles contains 'administrator'
  account.roles contains 'developer'

@name Cant remove unnamed clients
@tags mutation, client
deny permission.mut.client.delete if all:
  client.name equals 'Неизвестный'

  deny permission.mut.order.delete if all:
    order.createdHourLef gte 12
    env.time.hour > 16
`;

describe('Type defs generation', () => {
  const policies = new AbilityDSLParser(dsl).parse();

  test('Type defs generation', () => {
    const types = new AbilityTypeGenerator(policies).generateTypeDefs()

    console.log(types);
  });

});
