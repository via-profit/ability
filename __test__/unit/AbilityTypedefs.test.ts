import AbilityTypeGenerator from '~/core/AbilityTypeGenerator';
import { AbilityDSLParser } from '~/parsers/dsl/AbilityDSLParser';

describe('AbilityDSLParser', () => {
  it('should parse a policy with two rule sets (all of and any of)', () => {
    const dsl = `
# @name can order update
permit permission.order.update if any:
  # @name authorized admin
  all of:
    # @name contains role admin
    user.roles contains 'admin'
    user.token is not null

  # @name if is developer
  any of:
    user.roles contains 'developer'
    user.logit is equals 'dev'
    
    
allow permission.order.create if any:
  never
`;

    const parser = new AbilityDSLParser(dsl);
    const policies = parser.parse();

   const defs= new AbilityTypeGenerator(policies).generateTypeDefs();
console.log(defs);
  });


});
