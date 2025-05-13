import { AbilityParserError } from './AbilityError';
import AbilityPolicy from './AbilityPolicy';
import AbilityCondition from './AbilityCondition';


export class AbilityParser {
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
    value: string | number | boolean,
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
            case rule.condition.isEqual(AbilityCondition.not_equal):
            case rule.condition.isEqual(AbilityCondition.equal):
              value = typeof rule.resource;
              break;

            case rule.condition.isEqual(AbilityCondition.in):
            case rule.condition.isEqual(AbilityCondition.not_in):
              value = `${typeof rule.resource}[]`;
              break;

            case rule.condition.isEqual(AbilityCondition.more_or_equal):
            case rule.condition.isEqual(AbilityCondition.more_than):
            case rule.condition.isEqual(AbilityCondition.less_or_equal):
            case rule.condition.isEqual(AbilityCondition.less_than):
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
