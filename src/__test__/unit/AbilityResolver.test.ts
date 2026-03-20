import AbilityPolicy from '../../AbilityPolicy';
import AbilityResolver from '../../AbilityResolver';

describe('AbilityResolver', () => {
  describe('isInActionContain', () => {
    it('should return true for exact matches', () => {
      expect(AbilityResolver.isInActionContain('order.create', 'order.create')).toBe(true);
      expect(AbilityResolver.isInActionContain('user.update', 'user.update')).toBe(true);
    });

    it('should handle wildcard at the end', () => {
      expect(AbilityResolver.isInActionContain('order.*', 'order.create')).toBe(true);
      expect(AbilityResolver.isInActionContain('order.*', 'order.update')).toBe(true);
      expect(AbilityResolver.isInActionContain('order.*', 'order.delete')).toBe(true);
      expect(AbilityResolver.isInActionContain('order.*', 'user.create')).toBe(false);
    });

    it('should handle wildcard at the beginning', () => {
      expect(AbilityResolver.isInActionContain('*.create', 'order.create')).toBe(true);
      expect(AbilityResolver.isInActionContain('*.create', 'user.create')).toBe(true);
      expect(AbilityResolver.isInActionContain('*.create', 'order.update')).toBe(false);
    });

    it('should handle multiple wildcards', () => {
      expect(AbilityResolver.isInActionContain('user.*.*', 'user.profile.update')).toBe(true);
      expect(AbilityResolver.isInActionContain('user.*.*', 'user.settings.delete')).toBe(true);
      expect(AbilityResolver.isInActionContain('user.*.*', 'order.create')).toBe(false);
    });

    it('should handle wildcard in the middle', () => {
      expect(AbilityResolver.isInActionContain('user.*.update', 'user.profile.update')).toBe(true);
      expect(AbilityResolver.isInActionContain('user.*.update', 'user.settings.update')).toBe(true);
      expect(AbilityResolver.isInActionContain('user.*.update', 'user.profile.delete')).toBe(false);
    });

    it('should handle different path lengths', () => {
      expect(AbilityResolver.isInActionContain('account.read', 'account.private.read')).toBe(false);
      expect(
        AbilityResolver.isInActionContain('account.some.foo.bar', 'account.some.foo.bar'),
      ).toBe(true);
      expect(AbilityResolver.isInActionContain('account.some.foo', 'account.some.*')).toBe(true);
    });
  });

  describe('constructor', () => {
    it('should accept single policy', () => {
      const policy = AbilityPolicy.parse({
        id: 'test',
        name: 'test',
        action: 'test.action',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const resolver = new AbilityResolver(policy);

      expect(resolver['policies']).toHaveLength(1);
    });

    it('should accept array of policies', () => {
      const policy1 = AbilityPolicy.parse({
        id: 'test1',
        name: 'test1',
        action: 'test.action1',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const policy2 = AbilityPolicy.parse({
        id: 'test2',
        name: 'test2',
        action: 'test.action2',
        effect: 'deny',
        compareMethod: 'and',
        ruleSet: [],
      });

      const resolver = new AbilityResolver([policy1, policy2]);

      expect(resolver['policies']).toHaveLength(2);
    });
  });


  describe('resolve', () => {

    type TestResources = {
      [key: string]: Record<string, unknown>;
    };

    it('should filter policies by action', async () => {
      const policy1 = AbilityPolicy.parse<TestResources>({
        id: 'policy1',
        name: 'Policy 1',
        action: 'order.create',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const policy2 = AbilityPolicy.parse<TestResources>({
        id: 'policy2',
        name: 'Policy 2',
        action: 'order.update',
        effect: 'deny',
        compareMethod: 'and',
        ruleSet: [],
      });

      const result = await new AbilityResolver([policy1, policy2]).resolve('order.create', {});

      expect(result.policies).toHaveLength(1);
      expect(result.policies[0].id).toBe('policy1');
    });

    it('should handle wildcard actions', () => {
      const policy = AbilityPolicy.parse<TestResources>({
        id: 'policy',
        name: 'Policy',
        action: 'order.*',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const resolver = new AbilityResolver(policy);
      resolver.resolve('order.create', {});

      expect(resolver['policies']).toHaveLength(1);
    });
  });
});
