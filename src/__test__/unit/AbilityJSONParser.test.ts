/* eslint-disable @typescript-eslint/no-explicit-any */

import AbilityParser, { NestedDict } from '../../core/AbilityParser';
import { AbilityParserError } from '../../core/AbilityError';

describe('AbilityJSONParser', () => {
  describe('setValueDotValue', () => {
    it('should set value on simple path', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'user.name', 'John');

      expect(obj).toEqual({
        user: {
          name: 'John',
        },
      });
    });

    it('should set value on nested path', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'user.profile.address.city', 'Moscow');

      expect(obj).toEqual({
        user: {
          profile: {
            address: {
              city: 'Moscow',
            },
          },
        },
      });
    });

    it('should create arrays for numeric indices', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'users[0].name', 'John');
      AbilityParser.setValueDotValue(obj, 'users[1].name', 'Jane');

      expect(obj).toEqual({
        users: [{ name: 'John' }, { name: 'Jane' }],
      });
    });

    it('should handle mixed object and array paths', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'company.employees[0].profile.name', 'John');

      expect(obj).toEqual({
        company: {
          employees: [
            {
              profile: {
                name: 'John',
              },
            },
          ],
        },
      });
    });

    it('should throw error for invalid path', () => {
      const obj: NestedDict = {};

      expect(() => AbilityParser.setValueDotValue(obj, '', 'value')).toThrow(AbilityParserError);

      expect(() => AbilityParser.setValueDotValue(obj, '   ', 'value')).toThrow(AbilityParserError);
    });

    it('should throw error when trying to traverse non-object', () => {
      const obj = {
        user: 'not an object',
      };

      expect(() => AbilityParser.setValueDotValue(obj, 'user.name', 'John')).toThrow(
        AbilityParserError,
      );

      expect(() => AbilityParser.setValueDotValue(obj, 'user.name', 'John')).toThrow(
        "Cannot set property 'user' on non-object value at path: user.name",
      );
    });

    it('should throw error when trying to set primitive on existing object', () => {
      const obj: NestedDict = {
        user: {
          profile: {
            name: 'John',
          },
        },
      };

      expect(() => AbilityParser.setValueDotValue(obj, 'user.profile', 'not object')).toThrow(
        AbilityParserError,
      );

      expect(() => AbilityParser.setValueDotValue(obj, 'user.profile', 'not object')).toThrow(
        'Cannot set primitive value on existing object',
      );
    });

    it('should handle array with index', () => {
      const obj: NestedDict = {};

      AbilityParser.setValueDotValue(obj, 'users[0].name', 'John');

      expect(obj).toEqual({
        users: [{ name: 'John' }],
      });
    });

    it('should preserve existing values', () => {
      const obj: NestedDict = {
        user: {
          name: 'John',
          age: 30,
        },
      };

      AbilityParser.setValueDotValue(obj, 'user.city', 'Moscow');

      expect(obj).toEqual({
        user: {
          name: 'John',
          age: 30,
          city: 'Moscow',
        },
      });
    });
  });
});
