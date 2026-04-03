import { AbilityDSLLexer, AbilityDSLParser, AbilityResolver } from '../../src';


describe('multiple policies', () => {
  it('Multiple policies', () => {
    const dsl = `
  # @name 1. Администраторы могут удалять любых клиентов
  permit permission.client.delete if all:
    user.roles contains 'admin'
  
  # @name 2. Менеджеры могут удалять только клиентов, созданных не более 2 дней назад
  permit permission.client.delete if all:
    user.roles contains 'manager'
    client.createdDaysAt >= 2
  
  # @name 3. Запрет менеджерам удалять клиентов, созданных более 2 дней назад
  deny permission.client.delete if all:
    user.roles contains 'manager'
    client.createdDaysAt < 2
`;

    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['admin'] },
        client: { createdDaysAt: 3 },
      }),
    ).not.toThrow();

    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['manager'] },
        client: { createdDaysAt: 3 },
      }),
    ).not.toThrow();

    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['manager'] },
        client: { createdDaysAt: 1 },
      }),
    ).toThrow();
  });


  it('Complex policies with explicit and implicit except blocks', () => {
    const dsl = `
    # @name 1. Администраторы могут удалять любых клиентов
    permit permission.client.delete if all:
      user.roles contains 'admin'

    # @name 2. Менеджеры могут удалять клиентов, кроме тех, чей юр. статус ООО или ПАО или ОАО
    permit permission.client.delete if all:
      user.roles contains 'manager'
      except any of:
        client.legalStatus is equals 'ООО'
        client.legalStatus is equals 'ПАО'
        client.legalStatus is equals 'ОАО'

    # @name 3. Диспетчеры могут удалять клиентов чей юр. статус ИП, но кроме созданных более 2 дней назад
    permit permission.client.delete if all:
      user.roles contains 'dispatcher'
      client.legalStatus is equals 'ИП'
      except any of:
        client.createdDaysAt > 2

    # @name 4. Операторы могут удалять клиентов, кроме тех, кто заблокирован (implicit except)
    permit permission.client.delete if all:
      user.roles contains 'operator'
      except:
        client.blocked is equals true
  `;

    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    //
    // 1. Администратор — всегда можно
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['admin'] },
        client: { legalStatus: 'ООО', createdDaysAt: 100, blocked: true },
      }),
    ).not.toThrow();

    //
    // 2. Менеджер — можно, если статус НЕ ООО/ПАО/ОАО
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['manager'] },
        client: { legalStatus: 'ИП', createdDaysAt: 1, blocked: false },
      }),
    ).not.toThrow();

    //
    // 2. Менеджер — нельзя, если статус ООО/ПАО/ОАО
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['manager'] },
        client: { legalStatus: 'ООО', createdDaysAt: 1, blocked: false },
      }),
    ).toThrow();

    //
    // 3. Диспетчер — можно, если ИП и создан <= 2 дней назад
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['dispatcher'] },
        client: { legalStatus: 'ИП', createdDaysAt: 1, blocked: false },
      }),
    ).not.toThrow();

    //
    // 3. Диспетчер — нельзя, если ИП, но создан > 2 дней назад
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['dispatcher'] },
        client: { legalStatus: 'ИП', createdDaysAt: 3, blocked: false },
      }),
    ).toThrow();

    //
    // 3. Диспетчер — нельзя, если НЕ ИП
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['dispatcher'] },
        client: { legalStatus: 'ООО', createdDaysAt: 1, blocked: false },
      }),
    ).toThrow();

    //
    // 4. Оператор — можно, если клиент НЕ заблокирован
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['operator'] },
        client: { legalStatus: 'ИП', createdDaysAt: 1, blocked: false },
      }),
    ).not.toThrow();

    //
    // 4. Оператор — нельзя, если клиент заблокирован (implicit except)
    //
    expect(() =>
      resolver.enforce('client.delete', {
        user: { roles: ['operator'] },
        client: { legalStatus: 'ИП', createdDaysAt: 1, blocked: true },
      }),
    ).toThrow();
  });


});
