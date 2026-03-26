import { AbilityDSLParser } from '~/parsers/dsl/AbilityDSLParser';
import AbilityResolver from '../../core/AbilityResolver';

describe('DSL Operators', () => {
  // -----------------------------
  // #region Operator "is equals"
  // -----------------------------
  describe('Operator "is equals"', () => {
    it('should permit rule operator "equals"', async () => {
      const dsl = `
      permit action.test if all:
        user.name is equals 'Oleg'
        user.age is equals 21
        user.stateOn is equals true
    `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', {
        user: { name: 'Oleg', age: 21, stateOn: true },
      });

      expect(result.isAllowed()).toBeTruthy();
      expect(result.isDenied()).toBeFalsy();
    });

    it('operator "is equals" should deny when not equal', async () => {
      const dsl = `
    permit action.test if all:
      user.age is equals 21
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 20 } });

      expect(result.isDenied()).toBeTruthy();
      expect(result.isAllowed()).toBeFalsy();
    });
  });

  // -----------------------------
  // #region Operator "is not equals"
  // -----------------------------
  describe('Operator "is not equals"', () => {
    it('operator "is not equals" should allow when values differ', async () => {
      const dsl = `
    permit action.test if all:
      user.age is not equals 21
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 20 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is not equals" should deny when values equal', async () => {
      const dsl = `
    permit action.test if all:
      user.age is not equals 21
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 21 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "greater than"
  // -----------------------------
  describe('Operator "greater than"', () => {
    it('operator "greater than" should allow when greater', async () => {
      const dsl = `
    permit action.test if all:
      user.age greater than 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 19 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "greater than" should deny when not greater', async () => {
      const dsl = `
    permit action.test if all:
      user.age greater than 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 18 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "greater than or equal"
  // -----------------------------
  describe('Operator "greater than or equal"', () => {
    it('operator "greater than or equal" should allow when equal or greater', async () => {
      const dsl = `
    permit action.test if all:
      user.age greater than or equal 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 18 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "greater than or equal" should deny when less', async () => {
      const dsl = `
    permit action.test if all:
      user.age greater than or equal 18
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 17 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "less than"
  // -----------------------------
  describe('Operator "less than"', () => {
    it('operator "less than" should allow when less', async () => {
      const dsl = `
    permit action.test if all:
      user.age less than 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 29 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "less than" should deny when not less', async () => {
      const dsl = `
    permit action.test if all:
      user.age less than 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 30 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "less than or equal"
  // -----------------------------
  describe('Operator "less than or equal"', () => {
    it('operator "less than or equal" should allow when equal or less', async () => {
      const dsl = `
    permit action.test if all:
      user.age less than or equal 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 30 } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "less than or equal" should deny when greater', async () => {
      const dsl = `
    permit action.test if all:
      user.age less than or equal 30
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { age: 31 } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "is null"
  // -----------------------------
  describe('Operator "is null"', () => {
    it('operator "is null" should allow when value is null', async () => {
      const dsl = `
    permit action.test if all:
      user.middleName is null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { middleName: null } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is null" should deny when value is not null', async () => {
      const dsl = `
    permit action.test if all:
      user.middleName is null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { middleName: 'Ivan' } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "is not null"
  // -----------------------------
  describe('Operator "is not null"', () => {
    it('operator "is not null" should allow when value is not null', async () => {
      const dsl = `
    permit action.test if all:
      user.name is not null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { name: 'Oleg' } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "is not null" should deny when value is null', async () => {
      const dsl = `
    permit action.test if all:
      user.name is not null
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { name: null } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "in [...]"
  // -----------------------------
  describe('Operator "in [...]"', () => {
    it('operator "in" should allow when value is in list', async () => {
      const dsl = `
    permit action.test if all:
      user.role in ['admin', 'manager']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { role: 'admin' } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "in" should deny when value is not in list', async () => {
      const dsl = `
    permit action.test if all:
      user.role in ['admin', 'manager']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { role: 'guest' } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "not in [...]"
  // -----------------------------
  describe('Operator "not in [...]"', () => {
    it('operator "not in" should allow when value not in list', async () => {
      const dsl = `
    permit action.test if all:
      user.role not in ['banned', 'blocked']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { role: 'user' } });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "not in" should deny when value is in list', async () => {
      const dsl = `
    permit action.test if all:
      user.role not in ['banned', 'blocked']
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', { user: { role: 'banned' } });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "contains"
  // -----------------------------
  describe('Operator "contains"', () => {
    it('operator "contains" should allow when array contains value', async () => {
      const dsl = `
    permit action.test if all:
      user.tags contains 'vip'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', {
        user: { tags: ['vip', 'premium'] },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "contains" should deny when array does not contain value', async () => {
      const dsl = `
    permit action.test if all:
      user.tags contains 'vip'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', {
        user: { tags: ['basic'] },
      });

      expect(result.isDenied()).toBeTruthy();
    });
  });

  // -----------------------------
  // #region Operator "not contains"
  // -----------------------------
  describe('Operator "not contains"', () => {
    it('operator "not contains" should allow when array does not contain value', async () => {
      const dsl = `
    permit action.test if all:
      user.tags not contains 'banned'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', {
        user: { tags: ['vip', 'premium'] },
      });

      expect(result.isAllowed()).toBeTruthy();
    });

    it('operator "not contains" should deny when array contains value', async () => {
      const dsl = `
    permit action.test if all:
      user.tags not contains 'banned'
  `;
      const policies = new AbilityDSLParser(dsl).parse();
      const resolver = new AbilityResolver(policies);

      const result = await resolver.resolve('action.test', {
        user: { tags: ['banned'] },
      });

      expect(result.isDenied()).toBeTruthy();
    });
  });
});
