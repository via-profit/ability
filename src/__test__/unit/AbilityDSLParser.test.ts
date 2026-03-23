/* eslint-disable @typescript-eslint/no-explicit-any */

import AbilityParser, { NestedDict } from '../../core/AbilityParser';
import AbilityPolicy from '../../core/AbilityPolicy';
import { AbilityParserError } from '../../core/AbilityError';
import { AbilityDSLParser } from '../../parsers/dsl/AbilityDSLParser';

describe('AbilityDSLParser', () => {


  describe('Tokens', () => {
    it('unknown', () => {

      const dsl = `
allow order.update if
  user.name is not equals 'Oleg'
or 
  user.role contains managers and user.id is order.owner
or 
    user.department not contains administrators
or 
 user.roles contains administrator
      `;

      const parser = new AbilityDSLParser(dsl);
     parser.parse();

      expect({foo: 'bar'}).toEqual({
        foo: 'bar',
      });
    });
  });

});
