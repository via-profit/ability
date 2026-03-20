import AbilityRule from '../../AbilityRule';
import AbilityCondition from '../../AbilityCondition';
import AbilityMatch from '../../AbilityMatch';

describe('AbilityRule — environment support', () => {
  test('should extract value from environment (subject = env.*)', async () => {
    const rule = new AbilityRule({
      subject: 'env.time.hour',
      resource: 10,
      condition: AbilityCondition.more_or_equal,
    });

    const env = { time: { hour: 12 } };

    const result = await rule.check(null, env);

    expect(result).toBe(AbilityMatch.match);
  });

  test('should extract value from environment (resource = env.*)', async () => {
    const rule = new AbilityRule({
      subject: 'env.minAge',
      resource: 5,
      condition: AbilityCondition.less_or_equal,
    });

    const env = { minAge: 3 };

    const result = await rule.check(null, env);

    expect(result).toBe(AbilityMatch.match);
  });

  test('should compare resource.* with env.*', async () => {
    const rule = new AbilityRule({
      subject: 'user.age',
      resource: 'env.minAge',
      condition: AbilityCondition.more_or_equal,
    });

    const resource = { user: { age: 25 } };
    const env = { minAge: 18 };

    const result = await rule.check(resource, env);

    expect(result).toBe(AbilityMatch.match);
  });

  test('should return mismatch when env.* value is lower', async () => {
    const rule = new AbilityRule({
      subject: 'env.time.hour',
      resource: 18,
      condition: AbilityCondition.more_or_equal,
    });

    const env = { time: { hour: 10 } };

    const result = await rule.check(null, env);

    expect(result).toBe(AbilityMatch.mismatch);
  });

  test('should return undefined when env.* used but environment is missing', async () => {
    const rule = new AbilityRule({
      subject: 'env.time.hour',
      resource: 10,
      condition: AbilityCondition.more_or_equal,
    });

    const result = await rule.check(null, undefined);

    // undefined >= 10 → false
    expect(result).toBe(AbilityMatch.mismatch);
  });

  test('should handle literal subject and env.* resource', async () => {
    const rule = new AbilityRule({
      subject: 'env.limit',
      resource: 20,
      condition: AbilityCondition.less_or_equal,
    });

    const env = { limit: 18 };

    const result = await rule.check(null, env);

    expect(result).toBe(AbilityMatch.match);
  });

  test('should handle env.* subject and literal resource', async () => {
    const rule = new AbilityRule({
      subject: 'env.score',
      resource: 50,
      condition: AbilityCondition.more_than,
    });

    const env = { score: 60 };

    const result = await rule.check(null, env);

    expect(result).toBe(AbilityMatch.match);
  });

  test('should work when resourceData is null but env.* is used', async () => {
    const rule = new AbilityRule({
      subject: 'env.flag',
      resource: true,
      condition: AbilityCondition.equal,
    });

    const env = { flag: true };

    const result = await rule.check(null, env);

    expect(result).toBe(AbilityMatch.match);
  });
});
