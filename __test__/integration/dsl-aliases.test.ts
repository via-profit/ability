import { AbilityDSLParser } from '~/parsers/dsl/AbilityDSLParser';

describe('Ability DSL — alias definitions', () => {
  describe('parses multiple aliases', () => {
    const input = `
    
      @name AdminCheck
      alias isAdmin: user.roles contains 'admin'
      
      @name DeveloperCheck
      alias isDeveloper: user.roles contains 'developer'

      @disabled
      alias isManager: user.roles contains 'manager'

      permit permission.order.update if any:
        isAdmin
        isDeveloper
        isManager
      `;

    const policies = new AbilityDSLParser(input).parse();
    const policy = policies[0];
    const rules = policy.extractRules();

    it('Rule name is a alias name', () => {
      expect(rules[0].name).toBe('AdminCheck');
      expect(rules[1].name).toBe('DeveloperCheck');
      expect(rules[2].name).toBe('isManager');
    });

    it('Rule isManager from alias must be disabled', () => {
      expect(rules[2].disabled).toBeTruthy();
    });

  });
});
