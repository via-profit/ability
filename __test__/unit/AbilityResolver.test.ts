import AbilityResolver from '~/core/AbilityResolver';

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
});
