import { AbilityDSLParser, AbilityResolver, AbilityTypeGenerator } from '../../src';

const dsl = `
# @name AuthenticationRequired
# Пользователь должен быть аутентифицирован (токен обязателен)
allow permission.mut.* if all:
  token.id not equals 'NOT_ASSIGNED'
  token.type is equals 'access'

# @name OrderDeleteAccess
permit permission.mut.order.delete if all:
  account.roles contains 'administrator'
  account.roles contains 'developer'

# @name ClientDeleteAccess
permit permission.mut.client.delete if all:
  account.roles contains 'viewer'
  account.roles contains 'administrator'
  account.roles contains 'developer'

# @name Cant remove unnamed clients
deny permission.mut.client.delete if all:
  client.name equals 'Неизвестный'
  
  
  deny permission.mut.order.delete if all:
    order.createdHourLef gte 12
`;

describe('Type defs generation', () => {
  const policies = new AbilityDSLParser(dsl).parse();

  test('Type defs generation', () => {
    const types = new AbilityTypeGenerator(policies).generateTypeDefs()

    console.log(types);
  });

});
