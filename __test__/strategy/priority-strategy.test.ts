import { AbilityResolver } from '~/core/AbilityResolver';
import { PriorityStrategy } from '../../src/strategy/PriorityStrategy';
import { AbilityDSLParser } from '../../src';


const dsl = `
# 1. Администраторы могут удалять любых клиентов
@name admin_can_delete_any_client
@priority 100
permit permission.client.delete if all:
  user.roles contains 'admin'


# 2. Менеджеры
@name manager_cannot_delete_ooo_pao_oao
@priority 90
deny permission.client.delete if all:
  @name is manager by role
  user.roles contains 'manager'
  
  @name is legal entity
  client.legalStatus is in ['ООО', 'ПАО', 'ОАО']

@name manager_can_delete_other_clients
@priority 80
permit permission.client.delete if all:
  user.roles contains 'manager'


# 3. Диспетчеры
@name dispatcher_cannot_delete_old_ip
@priority 70
deny permission.client.delete if all:
  user.roles contains 'dispatcher'
  client.legalStatus is equals 'ИП'
  client.createdDaysAt > 2

@name dispatcher_can_delete_fresh_ip
@priority 60
permit permission.client.delete if all:
  user.roles contains 'dispatcher'
  client.legalStatus is equals 'ИП'


# 4. Операторы
@name operator_cannot_delete_blocked
@priority 50
deny permission.client.delete if all:
  user.roles contains 'operator'
  client.blocked is equals true

@name operator_can_delete_unblocked
@priority 40
permit permission.client.delete if all:
  user.roles contains 'operator'

`;

describe('PriorityStrategy: permission.client.delete', () => {
  const policies = new AbilityDSLParser(dsl).parse();
  const resolver = new AbilityResolver(policies, PriorityStrategy);

  const makeUser = (role: string) => ({ roles: [role] });

  const makeClient = (overrides = {}) => ({
    legalStatus: 'ИП',
    createdDaysAt: 0,
    blocked: false,
    ...overrides,
  });

  //
  // 1. Администраторы
  //
  test('admin can delete any client (highest priority)', () => {
    const user = makeUser('admin');
    const client = makeClient({ legalStatus: 'ООО', blocked: true });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });

  //
  // 2. Менеджеры
  //
  test('manager cannot delete ООО (deny priority > permit)', () => {
    const user = makeUser('manager');
    const client = makeClient({ legalStatus: 'ООО' });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isDenied()).toBe(true);
  });

  test('manager can delete ИП (permit wins when deny not applicable)', () => {
    const user = makeUser('manager');
    const client = makeClient({ legalStatus: 'ИП' });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });

  //
  // 3. Диспетчеры
  //
  test('dispatcher cannot delete old IP (deny priority > permit)', () => {
    const user = makeUser('dispatcher');
    const client = makeClient({ legalStatus: 'ИП', createdDaysAt: 3 });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isDenied()).toBe(true);
  });

  test('dispatcher can delete fresh IP', () => {
    const user = makeUser('dispatcher');
    const client = makeClient({ legalStatus: 'ИП', createdDaysAt: 1 });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });

  //
  // 4. Операторы
  //
  test('operator cannot delete blocked client (deny priority > permit)', () => {
    const user = makeUser('operator');
    const client = makeClient({ blocked: true });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isDenied()).toBe(true);
  });

  test('operator can delete unblocked client', () => {
    const user = makeUser('operator');
    const client = makeClient({ blocked: false });

    const result = resolver.resolve('permission.client.delete', { user, client });
    expect(result.isAllowed()).toBe(true);
  });
});
