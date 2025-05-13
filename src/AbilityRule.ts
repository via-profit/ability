import AbilityMatch from './AbilityMatch';
import AbilityCondition, { AbilityConditionLiteralType, AbilityConditionCodeType } from './AbilityCondition';

export type AbilityRuleConfig = {
  readonly id: string;
  readonly name: string;

  /**
   * Subject key path like a 'user.name'
   */
  readonly subject: string;
  /**
   * Resource key path like a 'user.name' or value
   */
  readonly resource: string | number | boolean | (string | number)[];

  readonly condition: AbilityConditionCodeType;
};

export class AbilityRule<Resources extends object = object> {
  /**
   * Subject key path like a 'user.name'
   */
  public subject: string;
  /**
   * Resource key path like a 'user.name' or value
   */
  public resource: string | number | boolean | (string | number)[];

  public condition: AbilityCondition;
  public name: string;
  public id: string;
  public state: AbilityMatch = AbilityMatch.pending;

  public constructor(params: AbilityRuleConfig) {
    const { id, name, subject, resource, condition } = params;
    this.id = id;
    this.name = name;
    this.subject = subject;
    this.resource = resource;
    this.condition = new AbilityCondition(condition);
  }

  /**
   * Check if the rule is matched
   * @param resource - The resource to check
   */
  public check(resource: Resources | null): AbilityMatch {
    let is: boolean = false;

    const [valueS, valueO] = this.extractValues(resource);

    if (AbilityCondition.less_than.isEqual(this.condition)) {
      is = Number(valueS) < Number(valueO);
    }

    if (AbilityCondition.less_or_equal.isEqual(this.condition)) {
      is = Number(valueS) <= Number(valueO);
    }

    if (AbilityCondition.more_than.isEqual(this.condition)) {
      is = Number(valueS) > Number(valueO);
    }

    if (AbilityCondition.more_or_equal.isEqual(this.condition)) {
      is = Number(valueS) >= Number(valueO);
    }

    if (AbilityCondition.equal.isEqual(this.condition)) {
      is = valueS === valueO;
    }

    if (AbilityCondition.not_equal.isEqual(this.condition)) {
      is = valueS !== valueO;
    }

    if (AbilityCondition.in.isEqual(this.condition)) {
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

    if (AbilityCondition.not_in.isEqual(this.condition)) {
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

    this.state = is ? AbilityMatch.match : AbilityMatch.mismatch;

    return this.state;
  }

  /**
   * Extract values from the resourceData
   * @param resourceData - The resourceData to extract values from
   */
  public extractValues(
    resourceData: Resources | null,
  ): [
    string | number | boolean | (string | number)[] | null | undefined,
    string | number | boolean | (string | number)[] | null | undefined,
  ] {
    let leftSideValue;
    let rightSideValue;

    if (resourceData === null || typeof resourceData === 'undefined') {
      return [NaN, NaN];
    }

    const isPath = (str: unknown): str is string => {
      return typeof str === 'string' && str.match(/\./g) !== null;
    };

    if (isPath(this.subject)) {
      leftSideValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
        resourceData,
        this.subject,
      );
    } else {
      leftSideValue = this.subject;
    }
    if (isPath(this.resource)) {
      rightSideValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
        resourceData,
        this.resource,
      );
    } else {
      rightSideValue = this.resource as number | boolean | string | (string | number)[];
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

  public static parse<Resources extends object>(config: AbilityRuleConfig): AbilityRule<Resources> {
    const { id, name, subject, resource, condition } = config;

    return new AbilityRule<Resources>({
      id,
      name,
      subject,
      resource,
      condition,
    });
  }

  /**
   * Export the rule to config object
   */
  public export(): AbilityRuleConfig {
    return {
      id: this.id,
      name: this.name,
      subject: this.subject,
      resource: this.resource,
      condition: this.condition.code,
    };
  }
}

export default AbilityRule;
