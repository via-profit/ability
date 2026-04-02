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
});
