import { AbilityResolver } from '../../src/core/AbilityResolver';
import { AbilityDSLParser } from '../../src/parsers/dsl/AbilityDSLParser';
import FirstMatchStrategy from '../../src/strategy/FirstMatchStrategy';

const dsl = `
# 1. Администраторы могут удалять любых клиентов
@name admin_can_delete_any_client
permit permission.client.delete if all:
  user.roles contains 'admin'


# 2. Менеджеры
@name manager_cannot_delete_ooo_pao_oao
deny permission.client.delete if all:
  @name is manager by role
  user.roles contains 'manager'

  @name is legal entity
  client.legalStatus is in ['ООО', 'ПАО', 'ОАО']

@name manager_can_delete_other_clients
permit permission.client.delete if all:
  user.roles contains 'manager'


# 3. Диспетчеры
@name dispatcher_cannot_delete_old_ip
deny permission.client.delete if all:
  user.roles contains 'dispatcher'
  client.legalStatus is equals 'ИП'
  client.createdDaysAt > 2

@name dispatcher_can_delete_fresh_ip
permit permission.client.delete if all:
  user.roles contains 'dispatcher'
  client.legalStatus is equals 'ИП'


# 4. Операторы
@name operator_cannot_delete_blocked
deny permission.client.delete if all:
  user.roles contains 'operator'
  client.blocked is equals true

@name operator_can_delete_unblocked
permit permission.client.delete if all:
  user.roles contains 'operator'
`;

describe('FirstMatchStrategy: permission.client.delete', () => {
  const policies = new AbilityDSLParser(dsl).parse();
  const resolver = new AbilityResolver(policies, FirstMatchStrategy);

  const makeUser = (role: string) => ({ roles: [role] });

  const makeClient = (overrides = {}) => ({
    legalStatus: 'ИП',
    createdDaysAt: 0,
    blocked: false,
    ...overrides,
  });

  //
  // 1. Admin
  //
  test('admin can delete any client (first match is permit)', () => {
    const user = makeUser('admin');
    const client = makeClient({ legalStatus: 'ООО', blocked: true });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });

  //
  // 2. Manager
  //
  test('manager cannot delete ООО/ПАО/ОАО (first match is deny)', () => {
    const user = makeUser('manager');
    const client = makeClient({ legalStatus: 'ООО' });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isDenied()).toBe(true);
  });

  test('manager can delete other clients (first match is permit)', () => {
    const user = makeUser('manager');
    const client = makeClient({ legalStatus: 'ИП' });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });

  //
  // 3. Dispatcher
  //
  test('dispatcher cannot delete old IP (first match is deny)', () => {
    const user = makeUser('dispatcher');
    const client = makeClient({ legalStatus: 'ИП', createdDaysAt: 3 });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isDenied()).toBe(true);
  });

  test('dispatcher can delete fresh IP (first match is permit)', () => {
    const user = makeUser('dispatcher');
    const client = makeClient({ legalStatus: 'ИП', createdDaysAt: 1 });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });

  //
  // 4. Operator
  //
  test('operator cannot delete blocked clients (first match is deny)', () => {
    const user = makeUser('operator');
    const client = makeClient({ blocked: true });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isDenied()).toBe(true);
  });

  test('operator can delete unblocked clients (first match is permit)', () => {
    const user = makeUser('operator');
    const client = makeClient({ blocked: false });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });
});
