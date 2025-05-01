import { AbilityParserError } from './AbilityError';
import AbilityPolicy from './AbilityPolicy';
import AbilityCondition from './AbilityCondition';

type FieldValidateConfig = [string, 'string' | 'number' | 'array', boolean][];


export class AbilityParser {

  /**
   * Validates the configuration object based on the provided field validation configurations.
   * @param config - The configuration object to validate.
   * @param fields - An array of field validation configurations.
   * @throws {AbilityParserError} If a required field is missing or if a field has an incorrect type.
   */
  public static validateConfig(config: Record<string, any>, fields: FieldValidateConfig): void | never {
    fields.forEach(([field, type, isRequired]) => {
      const value = config[field as keyof typeof config];
      if (isRequired) {
        if (typeof value === 'undefined') {
          throw new AbilityParserError(`Missing required field [${field}]`);
        }
      }

      switch (type) {
        case 'array':
          if (typeof value !== 'object' || !Array.isArray(value)) {
            throw new AbilityParserError(`Field [${field}] must be an type of [${type}], bit got [${typeof value}]`);
          }
          break;

        default:
          if (typeof value !== type && typeof value !== 'undefined') {
            throw new AbilityParserError(`Field [${field}] must be a type of [${type}], bit got [${typeof value}]`);
          }
          break;
      }


    });
  }

  /**
   * Prepares and validates the configuration object or JSON string.
   * @param configOrJson - The configuration object or JSON string to validate.
   * @param fields - An array of field validation configurations.
   * @returns The validated configuration object.
   */
  public static prepareAndValidateConfig<T>(configOrJson: string | unknown, fields: FieldValidateConfig): T {
    const config = typeof configOrJson === 'string'
      ? (JSON.parse(configOrJson))
      : configOrJson;

    AbilityParser.validateConfig(config, fields);

    return config;
  }

  /*
  *
  *  readonly ['order.update']: {
  *     readonly user: {
  *       readonly roles: readonly string[];
  *       readonly department: string;
  *     };
  *     readonly order: {
  *       readonly estimatedArrivalAt: number;
  *       readonly status: string;
  *     }
  *  }
  *
  * */

  /**
   * Sets a value in a nested object structure based on a dot/bracket notation path.
   * @param object - The target object to modify.
   * @param path - The path to the property in dot/bracket notation.
   * @param value - The value to set at the specified path.
   */
  public static setValueDotValue(
    object: Record<string, any>,
    path: string,
    value: string | number | boolean
  ): void {
    const way = path.replace(/\[/g, '.').replace(/\]/g, '').split('.');
    const last = way.pop();

    if (!last) {
      throw new AbilityParserError(`Invalid path provided on a [${path}]`);
    }

    way.reduce((o, k, i, kk) => {
      if (!o[k]) {
        o[k] = isFinite(Number(kk[i + 1])) ? [] : {};
      }
      return o[k];
    }, object)[last] = value;
  }

  /**
   * Generates TypeScript type definitions based on the provided policies.
   * @param policies - An array of AbilityPolicy instances.
   * @param outPath - The output path for the generated type definitions.
   * @returns A record containing the generated type definitions.
   */
  public static generateTypeDefs(policies: readonly AbilityPolicy[], outPath: string) {
    const record: Record<string, any> = {};

    policies.forEach(policy => {
      policy.ruleSet.forEach(ruleSet => {
        ruleSet.rules.forEach(rule => {
          let value: string = 'any';

          switch (true) {
            case rule.condition.isEqual(AbilityCondition.NOT_EQUAL):
            case rule.condition.isEqual(AbilityCondition.EQUAL):
              value = typeof rule.resource;
              break;

            case rule.condition.isEqual(AbilityCondition.IN):
            case rule.condition.isEqual(AbilityCondition.NOT_IN):
              value = `${typeof rule.resource}[]`;
              break;

            case rule.condition.isEqual(AbilityCondition.MORE_OR_EQUAL):
            case rule.condition.isEqual(AbilityCondition.MORE_THAN):
            case rule.condition.isEqual(AbilityCondition.LESS_OR_EQUAL):
            case rule.condition.isEqual(AbilityCondition.LESS_THAN):
              value = 'number';
              break;
          }
          AbilityParser.setValueDotValue(record, rule.subject, value);
        });
      });
    });

    console.log(JSON.stringify(record));

    return record;
  }

}

export default AbilityParser;