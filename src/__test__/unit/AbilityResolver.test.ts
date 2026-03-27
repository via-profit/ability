import AbilityPolicy from '../../core/AbilityPolicy';
import AbilityResolver from '../../core/AbilityResolver';

describe('AbilityResolver', () => {
  describe('isInPermissionContain', () => {
    it('should return true for exact matches', () => {
      expect(AbilityResolver.isInPermissionContain('order.create', 'order.create')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('user.update', 'user.update')).toBe(true);
    });

    it('should handle wildcard at the end', () => {
      expect(AbilityResolver.isInPermissionContain('order.*', 'order.create')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('order.*', 'order.update')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('order.*', 'order.delete')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('order.*', 'user.create')).toBe(false);
    });

    it('should handle wildcard at the beginning', () => {
      expect(AbilityResolver.isInPermissionContain('*.create', 'order.create')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('*.create', 'user.create')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('*.create', 'order.update')).toBe(false);
    });

    it('should handle multiple wildcards', () => {
      expect(AbilityResolver.isInPermissionContain('user.*.*', 'user.profile.update')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('user.*.*', 'user.settings.delete')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('user.*.*', 'order.create')).toBe(false);
    });

    it('should handle wildcard in the middle', () => {
      expect(AbilityResolver.isInPermissionContain('user.*.update', 'user.profile.update')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('user.*.update', 'user.settings.update')).toBe(true);
      expect(AbilityResolver.isInPermissionContain('user.*.update', 'user.profile.delete')).toBe(false);
    });

    it('should handle different path lengths', () => {
      expect(AbilityResolver.isInPermissionContain('account.read', 'account.private.read')).toBe(false);
      expect(
        AbilityResolver.isInPermissionContain('account.some.foo.bar', 'account.some.foo.bar'),
      ).toBe(true);
      expect(AbilityResolver.isInPermissionContain('account.some.foo', 'account.some.*')).toBe(true);
    });
  });

  describe('constructor', () => {
    it('should accept single policy', () => {
      const policy = AbilityPolicy.fromJSON({
        id: 'test',
        name: 'test',
        permission: 'test.permission',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const resolver = new AbilityResolver(policy);

      expect(resolver['policies']).toHaveLength(1);
    });

    it('should accept array of policies', () => {
      const policy1 = AbilityPolicy.fromJSON({
        id: 'test1',
        name: 'test1',
        permission: 'test.permission1',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const policy2 = AbilityPolicy.fromJSON({
        id: 'test2',
        name: 'test2',
        permission: 'test.permission2',
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

    it('should filter policies by permission', async () => {
      const policy1 = AbilityPolicy.fromJSON<TestResources>({
        id: 'policy1',
        name: 'Policy 1',
        permission: 'order.create',
        effect: 'permit',
        compareMethod: 'and',
        ruleSet: [],
      });

      const policy2 = AbilityPolicy.fromJSON<TestResources>({
        id: 'policy2',
        name: 'Policy 2',
        permission: 'order.update',
        effect: 'deny',
        compareMethod: 'and',
        ruleSet: [],
      });

      const result = await new AbilityResolver([policy1, policy2]).resolve('order.create', {});

      expect(result.policies).toHaveLength(1);
      expect(result.policies[0].id).toBe('policy1');
    });

    it('should handle wildcard permissions', () => {
      const policy = AbilityPolicy.fromJSON<TestResources>({
        id: 'policy',
        name: 'Policy',
        permission: 'order.*',
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
