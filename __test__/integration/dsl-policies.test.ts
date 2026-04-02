import { AbilityDSLParser, AbilityResolver } from '../../src';

const dsl = `
# ---------------------------------------------------------------------------------------- #
# Администратор может редактировать стоимость билетов.  
# Проверяется роль admin.
# Продавец может продавать билеты только в рабочие часы (09:00–23:00).
# Пользователь старше 21 года может покупать билеты.
# VIP‑пользователь может покупать билеты в любое время.
# Заблокированный пользователь (status = banned) не может покупать билеты.
# Продавец не может продавать билеты, если кинотеатр закрыт.
# Менеджер имеет те же права, что и продавец.
# Администратор имеет wildcard‑права (permission.*) и может выполнять любые действия.
# Пользователь не может купить более 6 билетов — действует лимит “6 билетов в одни руки”.
# Нельзя продавать билет, если его статус — sold.
# ---------------------------------------------------------------------------------------- #

############################################################
# @name Admin can edit ticket price
permit permission.ticket.price.edit if all:
  user.role is equals 'admin'


############################################################
# @name Seller can sell tickets during working hours
permit permission.ticket.sell if all:
  user.role is equals 'seller'
  all of:
    env.time.hour greater than or equal 9
    env.time.hour less than or equal 23


############################################################
# @name Users older than 21 can buy tickets
permit permission.ticket.buy if all:
  user.age greater than 21


############################################################
# @name VIP users can buy tickets anytime
permit permission.ticket.buy if all:
  user.isVIP is true


############################################################
# @name Deny buying tickets if user is banned
deny permission.ticket.buy if all:
  user.status is equals 'banned'


############################################################
# @name Deny selling tickets if cinema is closed
deny permission.ticket.sell if all:
  any of:
    env.time.hour less than 9
    env.time.hour greater than 23


############################################################
# @name Manager can do everything seller can
permit permission.ticket.sell if all:
  user.role is equals 'manager'


############################################################
# @name Admin wildcard permissions
permit permission.* if all:
  user.role is equals 'admin'


############################################################
# @name Limit tickets per user (max 6)
deny permission.ticket.buy if all:
  user.ticketsCount greater than or equal 6


############################################################
# @name Cannot sell already sold tickets
deny permission.ticket.sell if all:
  ticket.status is equals 'sold'

`;

describe('Cinema complex policies', () => {
  const policies = new AbilityDSLParser(dsl).parse();
  const resolver = new AbilityResolver(policies);

  test('Admin can edit ticket price', () => {
    const result = resolver.resolve('ticket.price.edit', {
      user: { role: 'admin' },
      env: { time: { hour: 12 } },
    });
    expect(result.isAllowed()).toBe(true);
  });

  test('Seller can sell tickets during working hours', () => {
    const result = resolver.resolve('ticket.sell', {
      user: { role: 'seller' },
      env: { time: { hour: 15 } },
      ticket: { status: 'available' },
    });
    expect(result.isAllowed()).toBe(true);
  });

  test('Seller cannot sell tickets at night', () => {
    const result = resolver.resolve('ticket.sell', {
      user: { role: 'seller' },
      env: { time: { hour: 2 } },
      ticket: { status: 'available' },
    });
    expect(result.isDenied()).toBe(true);
  });

  test('User older than 21 can buy tickets', () => {
    const result = resolver.resolve('ticket.buy', {
      user: { age: 25, ticketsCount: 0 },
      env: { time: { hour: 18 } },
    });
    expect(result.isAllowed()).toBe(true);
  });

  test('VIP can buy tickets anytime', () => {
    const result = resolver.resolve('ticket.buy', {
      user: { isVIP: true, ticketsCount: 0 },
      env: { time: { hour: 3 } },
    });
    expect(result.isAllowed()).toBe(true);
  });

  test('Banned user cannot buy tickets', () => {
    const result = resolver.resolve('ticket.buy', {
      user: { status: 'banned', age: 30, ticketsCount: 0 },
      env: { time: { hour: 12 } },
    });
    expect(result.isDenied()).toBe(true);
  });

  test('User cannot buy more than 6 tickets', () => {
    const result = resolver.resolve('ticket.buy', {
      user: { age: 30, ticketsCount: 6 },
      env: { time: { hour: 12 } },
    });
    expect(result.isDenied()).toBe(true);
  });

  test('Cannot sell already sold ticket', () => {
    const result = resolver.resolve('ticket.sell', {
      user: { role: 'seller' },
      env: { time: { hour: 12 } },
      ticket: { status: 'sold' },
    });
    expect(result.isDenied()).toBe(true);
  });

  test('Manager can sell tickets', () => {
    const result = resolver.resolve('ticket.sell', {
      user: { role: 'manager' },
      env: { time: { hour: 12 } },
      ticket: { status: 'available' },
    });
    expect(result.isAllowed()).toBe(true);
  });

  test('Admin wildcard: admin can sell tickets even at night', () => {
    const result = resolver.resolve('ticket.sell', {
      user: { role: 'admin' },
      env: { time: { hour: 3 } },
      ticket: { status: 'available' },
    });
    expect(result.isAllowed()).toBe(true);
  });
  test('Complex policy chain with state resets', () => {
    const dsl = `
    # 1. Разрешаем всё mut.* если токен валиден
    allow permission.mut.* if all:
      token.id not equals 'INVALID'

    # 2. Но если роль не админ — отменяем разрешение
    allow permission.mut.* if all:
      account.role equals 'admin'

    # 3. Если пользователь заблокирован — запрещаем
    deny permission.mut.* if all:
      account.status equals 'blocked'

    # 4. Но если есть флаг override — отменяем запрет
    deny permission.mut.* if all:
      flags.override equals true

    # 5. Для delete — отдельное правило: только супервайзер
    allow permission.mut.order.delete if all:
      account.role equals 'supervisor'

    # 6. И финальное правило: если нет флага allowDelete — отменяем разрешение
    allow permission.mut.order.delete if all:
      flags.allowDelete equals true
  `;

    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    //
    // CASE 1: токен валиден → allow, но роль не admin → сброс → итог neutral → deny
    //
    expect(() =>
      resolver.enforce('mut.order.delete', {
        token: { id: 'SECRET' },
        account: { role: 'manager', status: 'active' },
        flags: {},
      }),
    ).toThrow();

    //
    // CASE 2: токен валиден → allow, роль admin → allow, не blocked → allow,
    // нет override → deny mismatch → сброс,
    // delete требует supervisor → mismatch → сброс,
    // нет allowDelete → mismatch → сброс → итог deny
    //
    expect(() =>
      resolver.enforce('mut.order.delete', {
        token: { id: 'SECRET' },
        account: { role: 'admin', status: 'active' },
        flags: {},
      }),
    ).toThrow();

    //
    // CASE 3: supervisor → allow на delete, но allowDelete mismatch → сброс → deny
    //
    expect(() =>
      resolver.enforce('mut.order.delete', {
        token: { id: 'SECRET' },
        account: { role: 'supervisor', status: 'active' },
        flags: {},
      }),
    ).toThrow();

    //
    // CASE 4: supervisor + allowDelete → allow
    //
    expect(resolver.resolve('mut.order.delete', {
        token: { id: 'SECRET' },
        account: { role: 'supervisor', status: 'active' },
        flags: { allowDelete: true },
      }).isAllowed(),
    ).toBe(true);

    //
    // CASE 5: пользователь заблокирован → deny, override mismatch → сброс,
    // supervisor → allow, allowDelete → allow → итог allow
    //
    expect(
      resolver.resolve('mut.order.delete', {
        token: { id: 'SECRET' },
        account: { role: 'supervisor', status: 'blocked' },
        flags: { override: false, allowDelete: true },
      }).isAllowed(),
    ).toBe(true);

    //
    // CASE 6: пользователь заблокирован → deny, override match → deny отменён,
    // supervisor mismatch → сброс, allowDelete mismatch → сброс → итог deny
    //
    expect(() =>
      resolver.enforce('mut.order.delete', {
        token: { id: 'SECRET' },
        account: { role: 'manager', status: 'blocked' },
        flags: { override: true },
      }),
    ).toThrow();
  });


  test('Last matched policy', () => {
    const dsl = `
      # @name Пользователь должен быть аутентифицирован (токен обязателен)
      allow permission.mut.* if all:
          # @name token is not "NOT_ASSIGNED"
          token.id not equals 'NOT_ASSIGNED'
   
      # @name only admin can delete order
      permit permission.mut.order.delete if all:
        # @name roles contains 'administrator'
        account.roles contains 'administrator'
    `;

    const policies = new AbilityDSLParser(dsl).parse();
    const result = new AbilityResolver(policies).resolve('mut.order.delete', {
      token: { id: 'secret' },
      account: { roles: ['manager'] },
    });

    const finalState = result.getFinalState();
    console.log(finalState);
    console.log(result.explain().toString());

    expect(() =>
      new AbilityResolver(policies).enforce('mut.order.delete', {
        token: { id: 'secret' },
        account: { roles: ['manager'] },
      }),
    ).toThrow();
  });
});
