import AbilityMatch from './AbilityMatch';
import AbilityCondition from './AbilityCondition';
import AbilityParser from './AbilityParser';

export type AbilityRuleMatches = [string, AbilityCondition, string | number | boolean];

export type AbilityRuleConfig = {
  readonly name?: string | symbol;
  readonly matches: [string, string, string | number | boolean];
};


export class AbilityRule<Subject = unknown> {
  public matches: AbilityRuleMatches;
  public name: string | symbol;
  public state: AbilityMatch = AbilityMatch.PENDING;

  public constructor(params: { matches: AbilityRuleMatches; name?: string | symbol }) {
    const { name, matches } = params;
    this.name = name || Symbol('name');
    this.matches = matches;
  }

  /**
   * Check if the rule is matched
   * @param subject - The subject to check
   */
  public check(subject: Subject): AbilityMatch {
    const [_subjectPathName, condition, _staticValueOrPathName] = this.matches;

    let is: boolean = false;
    const [valueS, valueO] = this.extractValues(subject);

    if (AbilityCondition.LESS_THAN.isEqual(condition)) {
      is = Number(valueS) < Number(valueO);
    }

    if (AbilityCondition.LESS_OR_EQUAL.isEqual(condition)) {
      is = Number(valueS) <= Number(valueO);
    }

    if (AbilityCondition.MORE_THAN.isEqual(condition)) {
      is = Number(valueS) > Number(valueO);
    }

    if (AbilityCondition.MORE_OR_EQUAL.isEqual(condition)) {
      is = Number(valueS) >= Number(valueO);
    }

    if (AbilityCondition.EQUAL.isEqual(condition)) {
      is = valueS === valueO;
    }

    if (AbilityCondition.NOT_EQUAL.isEqual(condition)) {
      is = valueS !== valueO;
    }

    if (AbilityCondition.IN.isEqual(condition)) {
      // [<some>] and [<some>]
      if (Array.isArray(valueS) && Array.isArray(valueO)) {
        is = valueS.some(v => valueO.find(v1 => v1 === v));
      }
      // <some> and [<some>]
      if ((typeof valueS === 'string' || typeof valueS === 'number') && Array.isArray(valueO)) {
        is = valueO.includes(valueS);
      }
      // [<some>] and <some>
      if ((typeof valueO === 'string' || typeof valueO === 'number') && Array.isArray(valueS)) {
        is = valueS.includes(valueO);
      }
    }

    if (AbilityCondition.NOT_IN.isEqual(condition)) {
      // [<some>] and [<some>]
      if (Array.isArray(valueS) && Array.isArray(valueO)) {
        is = !valueS.some(v => valueO.find(v1 => v1 === v));
      }
      // <some> and [<some>]
      if ((typeof valueS === 'string' || typeof valueS === 'number') && Array.isArray(valueO)) {
        is = !valueO.includes(valueS);
      }
      // [<some>] and <some>
      if ((typeof valueO === 'string' || typeof valueO === 'number') && Array.isArray(valueS)) {
        is = !valueS.includes(valueO);
      }
    }

    this.state = is ? AbilityMatch.MATCH : AbilityMatch.MISMATCH;

    return this.state;
  }

  /**
   * Extract values from the subject
   * @param subject - The subject to extract values from
   */
  public extractValues(
    subject: unknown,
  ): [
      string | number | boolean | (string | number)[] | null | undefined,
      string | number | boolean | (string | number)[] | null | undefined,
  ] {
    const [subjectPathName, _condition, staticValueOrPathName] = this.matches;
    let leftSideValue;
    let rightSideValue;

    const isPath = (str: unknown): str is string => {
      return typeof str === 'string' && str.match(/\./g) !== null;
    };

    if (isPath(subjectPathName)) {
      leftSideValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
        subject,
        subjectPathName,
      );
    }
    if (isPath(staticValueOrPathName)) {
      rightSideValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
        subject,
        staticValueOrPathName,
      );
    } else {
      rightSideValue = staticValueOrPathName as number | boolean | string | (string | number)[];
    }

    return [leftSideValue, rightSideValue];
  }

  /**
   * Get the value of the object by dot notation
   * @param resource - The object to get the value from
   * @param desc - The dot notation string
   */
  public getDotNotationValue<T = unknown>(resource: unknown, desc: string): T | undefined {
    const arr = desc.split('.');

    while (arr.length && resource) {
      const comp = arr.shift() || '';
      const match = new RegExp('(.+)\\[([0-9]*)\\]').exec(comp);

      if (match !== null && match.length == 3) {
        const arrayData = {
          arrName: match[1],
          arrIndex: match[2],
        };

        if (resource[arrayData.arrName as keyof typeof resource] !== undefined) {
          resource = resource[arrayData.arrName as keyof typeof resource][arrayData.arrIndex];
        } else {
          resource = undefined;
        }
      } else {
        resource = resource[comp as keyof typeof resource];
      }
    }

    return resource as T;
  }


  public static parse<Subject = unknown>(
    configOrJson: AbilityRuleConfig | string,
  ): AbilityRule<Subject> {

    const config = AbilityParser.prepareAndValidateConfig<AbilityRuleConfig>(configOrJson, [
      ['id', 'string', false],
      ['name', 'string', true],
      ['matches', 'array', true],
    ]);


    const { name, matches } = config;
    const [leftField, condition, rightField] = matches;

    return new AbilityRule<Subject>({
      name,
      matches: [leftField, new AbilityCondition(condition), rightField],
    });
  }

  /**
   * Export the rule to config object
   */
  public export(): AbilityRuleConfig {
    const [leftField, condition, rightField] = this.matches;

    return {
      name: this.name,
      matches: [leftField, condition.code, rightField],
    };
  }
}

export default AbilityRule;
