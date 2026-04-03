import { AbilityDSLParser } from '~/parsers/dsl/AbilityDSLParser';
import AbilityResolver from '~/core/AbilityResolver';

describe('DSL Operators', () => {
  describe('Any', () => {
    it('any', () => {
      const dsl = `
      deny permission.user.passwordHash if any:
        viewer.id is not equals owner.id
      `;

      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      expect(() =>
        resolver.enforce('user.passwordHash', {
          viewer: { id: '1' },
          owner: { id: '2' },
        }),
      ).toThrow();
    })
  });
  // -----------------------------
  // #region Operator "is equals"
  // -----------------------------
  describe('Operator "is equals"', () => {
    it('should permit rule operator "equals"', () => {
      const dsl = `
      permit permission.test if all:
        user.name is equals 'Oleg'
        user.age is equals 21
        user.age = 21
        user.age == 21
        user.surname equals 'Ivanov'
        user.stateOn equals true
    `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('test', {
        user: {
          name: 'Oleg',
          surname: 'Ivanov',
          age: 21,
          stateOn: true,
        },
      });

      expect(result.isAllowed()).toBeTruthy();
      expect(result.isDenied()).toBeFalsy();
    });

    it('operator "is equals" should deny when not equal', () => {
      const dsl = `
    permit permission.test if all:
        user.name is equals 'Oleg'
        user.age is equals 21
        user.age = 21
        user.age == 21
        user.surname equals 'Ivanov'
        user.stateOn equals true
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          age: 20,
        },
      });

      expect(result.isDenied()).toBeTruthy();
      expect(result.isAllowed()).toBeFalsy();
    });
  });

  // -----------------------------
  // #region Operator "is not equals"
  // -----------------------------
  describe('Operator "is not equals"', () => {
    it('operator "is not equals" should allow when values differ', () => {
      const dsl = `
    permit permission.test if all:
      user.age is not equals 21
      user.age != 16
      user.age <> 16
      user.age not equals 16
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 20 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is not equals" should deny when values equal', () => {
      const dsl = `
    permit permission.test if all:
      user.age is not equals 21
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 21 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "greater than"
  // -----------------------------
  describe('Operator "greater than"', () => {
    it('operator "greater than" should allow when greater', () => {
      const dsl = `
    permit permission.test if all:
      user.age greater than 18
      user.age > 18
      user.age gt 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 19 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "greater than" should deny when not greater', () => {
      const dsl = `
    permit permission.test if all:
      user.age greater than 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 18 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "greater than or equal"
  // -----------------------------
  describe('Operator "greater than or equal"', () => {
    it('operator "greater than or equal" should allow when equal or greater', () => {
      const dsl = `
    permit permission.test if all:
      user.age greater than or equal 18
      user.val >= 6
      user.val gte 6
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 18, val: 6 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "greater than or equal" should deny when less', () => {
      const dsl = `
    permit permission.test if all:
      user.age greater than or equal 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 17 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "less than"
  // -----------------------------
  describe('Operator "less than"', () => {
    it('operator "less than" should allow when less', () => {
      const dsl = `
    permit permission.test if all:
      user.age less than 30
      user.age lt 30
      user.age < 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 29 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "less than" should deny when not less', () => {
      const dsl = `
    permit permission.test if all:
      user.age less than 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 30 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "less than or equal"
  // -----------------------------
  describe('Operator "less than or equal"', () => {
    it('operator "less than or equal" should allow when equal or less', () => {
      const dsl = `
    permit permission.test if all:
      user.age less than or equal 30
      user.vol <= 15
      user.vol lte 15
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('test', { user: { age: 30, vol: 15 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "less than or equal" should deny when greater', () => {
      const dsl = `
    permit permission.test if all:
      user.age less than or equal 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { age: 31 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "is null"
  // -----------------------------
  describe('Operator "is null"', () => {
    it('operator "is null" should allow when value is null', () => {
      const dsl = `
    permit permission.test if all:
      user.middleName is null
      user.surname = null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          middleName: null,
          surname: null,
        },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is null" should deny when value is not null', () => {
      const dsl = `
    permit permission.test if all:
      user.middleName is null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { middleName: 'Ivan' } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "is not null"
  // -----------------------------
  describe('Operator "is not null"', () => {
    it('operator "is not null" should allow when value is not null', () => {
      const dsl = `
    permit permission.test if all:
      user.name is not null
      user.surname is not null
      user.surname != null
      user.surname <> null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          name: 'Oleg',
          surname: 'Ivanov',
        },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is not null" should deny when value is null', () => {
      const dsl = `
    permit permission.test if all:
      user.name is not null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { name: null } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "in [...]"
  // -----------------------------
  describe('Operator "in [...]"', () => {
    it('operator "in" should allow when value is in list', () => {
      const dsl = `
    permit permission.test if all:
      user.role in ['admin', 'manager']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          role: 'admin',
        },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "in" should deny when value is not in list', () => {
      const dsl = `
    permit permission.test if all:
      user.role in ['admin', 'manager']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { role: 'guest' } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "not in [...]"
  // -----------------------------
  describe('Operator "not in [...]"', () => {
    it('operator "not in" should allow when value not in list', () => {
      const dsl = `
    permit permission.test if all:
      user.role not in ['banned', 'blocked']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { role: 'user' } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "not in" should deny when value is in list', () => {
      const dsl = `
    permit permission.test if all:
      user.role not in ['banned', 'blocked']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', { user: { role: 'banned' } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "contains"
  // -----------------------------
  describe('Operator "contains"', () => {
    it('operator "contains" should allow when array contains value', () => {
      const dsl = `
    permit permission.test if all:
      user.tags contains 'vip'
      user.tags includes 'vip'
      user.tags has 'vip'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: { tags: ['vip', 'premium'] },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "contains" should deny when array does not contain value', () => {
      const dsl = `
    permit permission.test if all:
      user.tags contains 'vip'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: { tags: ['basic'] },
      });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "not contains"
  // -----------------------------
  describe('Operator "not contains"', () => {
    it('operator "not contains" should allow when array does not contain value', () => {
      const dsl = `
    permit permission.test if all:
      user.tags not contains 'banned'
      user.tags not includes 'banned'
      user.tags not has 'banned'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: { tags: ['vip', 'premium'] },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "not contains" should deny when array contains value', () => {
      const dsl = `
    permit permission.test if all:
      user.tags not contains 'banned'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: { tags: ['banned'] },
      });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "is true"
  // -----------------------------
  describe('Operator "is true"', () => {
    it('operator "is true" should allow when value is true', () => {
      const dsl = `
    permit permission.test if all:
      user.statusOn is true
      user.statusOn = true
      user.statusOn == true
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: { statusOn: true },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is true" should deny when value is not true', () => {
      const dsl = `
    permit permission.test if all:
      user.statusOn is true
      user.statusOn is true
      user.statusOn = true
      user.statusOn == true
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: { statusOn: true },
      });

      expect(result.isAllowed()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "length equals"
  // -----------------------------
  describe('Operator "length greater than"', () => {
    it('Operator "length greater than" should permit', () => {
      const dsl = `
    permit permission.test if all:
      user.name length equals 4
      user.name length = 4
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          name: 'Oleg',
        },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('Operator "length greater than" should deny', () => {
      const dsl = `
    permit permission.test if all:
      user.name length equals 3
      user.name length = 3
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          name: 'Oleg',
        },
      });

      expect(result.isAllowed()).toBeFalsy();
      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "length greater than"
  // -----------------------------
  describe('Operator "length greater than"', () => {
    it('Operator "length greater than" should permit', () => {
      const dsl = `
    permit permission.test if all:
      user.name length greater than 3
      user.name length > 3
      user.roles length greater than 2
      user.roles length > 2
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          name: 'Oleg',
          roles: ['admin', 'manager', 'operator'],
        },
      });

      expect(result.isAllowed()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "length less than"
  // -----------------------------
  describe('Operator "length less than"', () => {
    it('Operator "length less than" should permit', () => {
      const dsl = `
    permit permission.test if all:
      user.name length less than 5
      user.name length < 5
      user.roles length less than 4
      user.roles length < 4
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        user: {
          name: 'Oleg',
          roles: ['admin', 'manager', 'operator'],
        },
      });

      expect(result.isAllowed()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "always"
  // -----------------------------
  describe('Operator "always"', () => {
    it('Operator "always" should permit', () => {
      const dsl = `
        allow permission.* if all:
          always
      `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        foo: 'bar',
      });

      expect(result.isAllowed()).toBeTruthy();
      expect(result.isDenied()).toBeFalsy();
    });

    it('Operator "always" should permit, but deny fo next', () => {
      const dsl = `
        allow permission.* if all:
          always

        deny permission.test if any:
          env.hour >= 16
      `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        env: {
          hour: 16,
        },
      });
      expect(result.isAllowed()).toBeFalsy();
      expect(result.isDenied()).toBeTruthy();
    });

    it('Operator "always" should NOT permit after 16 under DenyOverrides', () => {
      const dsl = `
        @name deny_after_16
        deny permission.test if any:
          env.hour >= 16
    
        @name allow_all_permissions
        permit permission.* if all:
          always
      `;

      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        env: { hour: 16 },
      });

      expect(result.isDenied()).toBeTruthy();
      expect(result.isAllowed()).toBeFalsy();
    });

  });

  // -----------------------------
  // #region Operator "never"
  // -----------------------------
  describe('Operator "never"', () => {
    it('Operator "never" should deny', () => {
      const dsl = `
        permit permission.* if all:
          never
      `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        foo: 'bar',
      });

      expect(result.isAllowed()).toBeFalsy();
      expect(result.isDenied()).toBeTruthy();
    });

    it('Operator "never" should deny', () => {
      const dsl = `
        deny permission.* if all:
          never

        permit permission.* if any:
          never

        permit permission.* if any:
          always
        
      `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = resolver.resolve('permission.test', {
        foo: 'bar',
      });

      expect(result.isAllowed()).toBeTruthy();
      expect(result.isDenied()).toBeFalsy();
    });
  });
});
