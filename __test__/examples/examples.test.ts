import { AbilityDSLParser, AbilityResolver } from '../../src';

describe('Examples', () => {
  it('Example-01', () => {
    const dsl = `
      permit permission.user.passwordHash.read if all:
        viewer.id equals owner.id
    `;

    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    expect(() =>
      resolver.enforce('user.passwordHash', {
        viewer: { id: '1' },
        owner: { id: '2' },
      }),
    ).toThrow();
  });


  it('Example-02', () => {
    const dsl = `
    permit permission.order.* if all:
      user.authenticated equals true
    
    deny permission.order.update if all:
      user.role not equals 'admin'
    `;

    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    // Для не-администратора – исключение
    expect(() =>
      resolver.enforce('order.update', {
        user: { authenticated: true, role: 'manager' },
      }),
    ).toThrow();

    // Для администратора – без исключения
    expect(() =>
      resolver.enforce('order.update', {
        user: { authenticated: true, role: 'admin' },
      }),
    ).not.toThrow();

    // Другое действие (например, order.view) – разрешено
    expect(() =>
      resolver.enforce('order.view', {
        user: { authenticated: true },
      }),
    ).not.toThrow();

  });
});
