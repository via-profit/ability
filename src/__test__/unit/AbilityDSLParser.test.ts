import { AbilityDSLParser } from '../../parsers/dsl/AbilityDSLParser';
import {AbilityJSONParser} from '../../parsers/AbilityJSONParser';

const dsl = `
# Политика
permit order.update if any:
  all of:
    user.roles contains 'admin'
    user.token is not null
  any of:
    user.roles contains 'developer'
    user.logit equals 'dev'
`;

describe('AbilityDSLParser', () => {
  describe('Tokens', () => {
    it('unknown', () => {
      const parser = new AbilityDSLParser(dsl);
      const policies = parser.parse();

      const json = AbilityJSONParser.toJSON(policies);
      
      expect({ foo: 'bar' }).toEqual({
        foo: 'bar',
      });
    });
  });
});
