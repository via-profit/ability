import AbilityResolver from '../../core/AbilityResolver';
import { AbilityDSLParser } from '../../parsers/dsl/AbilityDSLParser';

describe('Policy DSL operator tests', () => {

  // -----------------------------
  // equals
  // -----------------------------
  it('should permit rule operator equals (string)', async () => {
    const dsl = `
      permit action.test if any:
        user.role equals 'admin'
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { role: 'admin' },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should deny rule operator equals (string mismatch)', async () => {
    const dsl = `
      permit action.test if any:
        user.role is equal 'admin'
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { role: 'admin' },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should permit rule operator equals (string mismatch)', async () => {
    const dsl = `
      permit action.test if any:
        user.role equals 'admin'
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { role: 'admin' },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should permit rule operator equals (boolean)', async () => {
    const dsl = `
      permit action.test if any:
        user.active equals true
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { active: true },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // not equals
  // -----------------------------
  it('should permit rule operator not equals', async () => {
    const dsl = `
      permit action.test if any:
        user.role not equals 'banned'
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { role: 'admin' },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // in
  // -----------------------------
  it('should permit rule operator in (value in array)', async () => {
    const dsl = `
      permit action.test if any:
        user.role in ['admin','moderator']
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { role: 'moderator' },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should permit rule operator in (array intersects array)', async () => {
    const dsl = `
      permit action.test if any:
        user.roles in ['admin','moderator']
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { roles: ['guest', 'moderator'] },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // not in
  // -----------------------------
  it('should permit rule operator not in (value not in array)', async () => {
    const dsl = `
      permit action.test if any:
        user.role not in ['banned','deleted']
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { role: 'admin' },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // contains
  // -----------------------------
  it('should permit rule operator contains (array contains value)', async () => {
    const dsl = `
      permit action.test if any:
        user.roles contains 'admin'
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { roles: ['admin', 'operator'] },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should permit rule operator contains (array intersects array)', async () => {
    const dsl = `
      permit action.test if any:
        user.roles contains ['admin','moderator']
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { roles: ['guest', 'moderator'] },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // not contains
  // -----------------------------
  it('should permit rule operator not contains (array does not contain value)', async () => {
    const dsl = `
      permit action.test if any:
        user.roles not contains 'banned'
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { roles: ['admin', 'operator'] },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // numeric operators
  // -----------------------------
  it('should permit rule operator less', async () => {
    const dsl = `
      permit action.test if any:
        user.age < 18
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 16 },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should permit rule operator less than', async () => {
    const dsl = `
      permit action.test if any:
        user.age <= 18
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 18 },
    });

    expect(result.isAllowed()).toBeTruthy();
  });
  it('should permit rule operator <', async () => {
    const dsl = `
      permit action.test if any:
        user.age < 18
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 16 },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should permit rule operator <=', async () => {
    const dsl = `
      permit action.test if any:
        user.age <= 18
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 18 },
    });

    expect(result.isAllowed()).toBeTruthy();
  });


  it('should permit rule operator >', async () => {
    const dsl = `
      permit action.test if any:
        user.age > 18
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 25 },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should deny rule operator is equal wrong', async () => {
    const dsl = `
      permit action.test if any:
        all of:
          user.age is equal 14
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 18 },
    });

    const e = result.explain().toString();

    expect(result.isAllowed()).toBeFalsy();
  });

  it('should permit rule operator =', async () => {
    const dsl = `
      permit action.test if any:
        user.age is equal 14
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 18 },
    });

    expect(result.isAllowed()).toBeFalsy();
  });

  it('should permit rule operator >=', async () => {
    const dsl = `
      permit action.test if any:
        user.age >= 18
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { age: 18 },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  // -----------------------------
  // null handling
  // -----------------------------
  it('should handle null equals null', async () => {
    const dsl = `
      permit action.test if any:
        user.value equals null
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { value: null },
    });

    expect(result.isAllowed()).toBeTruthy();
  });

  it('should deny non-equals with null', async () => {
    const dsl = `
      permit action.test if any:
        user.value equals null
    `;
    const policies = new AbilityDSLParser(dsl).parse();
    const resolver = new AbilityResolver(policies);

    const result = await resolver.resolve('action.test', {
      user: { value: 5 },
    });

    expect(result.isAllowed()).toBeFalsy();
  });

});
