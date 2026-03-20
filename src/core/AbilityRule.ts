import AbilityMatch from './AbilityMatch';
import AbilityCondition, { AbilityConditionCodeType } from './AbilityCondition';

export type AbilityRuleConfig = {
  readonly id?: string | null;
  readonly name?: string | null;

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

export type AbilityRuleConstructorProps = Omit<AbilityRuleConfig, 'condition'> & {
  readonly condition: AbilityCondition;
};

/**
 * Represents a rule that defines a condition to be checked against a subject and resource.
 */
export class AbilityRule<Resources extends object = object, Environment = unknown> {
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

  /**
   * Creates an instance of AbilityRule.
   * @param {string} params.id - The unique identifier of the rule.
   * @param {string} params.name - The name of the rule.
   * @param {AbilityCondition} params.condition - The condition to evaluate.
   * @param {string} params.subject - The subject of the rule.
   * @param {string} params.resource - The resource to compare against.
   * @param params
   */
  public constructor(params: AbilityRuleConstructorProps) {
    const { id, name, subject, resource, condition } = params;
    this.name = name || `${JSON.stringify(subject)} ${condition.code} ${JSON.stringify(resource)}`;
    this.id = id || this.name;

    this.subject = subject;
    this.resource = resource;
    this.condition = condition;
  }

  /**
   * Check if the rule is matched
   * @param resource - The resource to check
   * @param environment
   */
  public async check(resource: Resources | null, environment?: Environment): Promise<AbilityMatch> {
    let is: boolean = false;

    const [subjectValue, resourceValue] = this.extractValues(resource, environment);

    if (AbilityCondition.less_than.isEqual(this.condition)) {
      is = Number(subjectValue) < Number(resourceValue);
    }

    if (AbilityCondition.less_or_equal.isEqual(this.condition)) {
      is = Number(subjectValue) <= Number(resourceValue);
    }

    if (AbilityCondition.more_than.isEqual(this.condition)) {
      is = Number(subjectValue) > Number(resourceValue);
    }

    if (AbilityCondition.more_or_equal.isEqual(this.condition)) {
      is = Number(subjectValue) >= Number(resourceValue);
    }

    if (AbilityCondition.equal.isEqual(this.condition)) {
      is = subjectValue === resourceValue;
    }

    if (AbilityCondition.not_equal.isEqual(this.condition)) {
      is = subjectValue !== resourceValue;
    }

    if (AbilityCondition.in.isEqual(this.condition)) {
      // [<some>] and [<some>]
      if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = subjectValue.some(v => resourceValue.find(v1 => v1 === v));
      }
      // <some> and [<some>]
      if ((typeof subjectValue === 'string' || typeof subjectValue === 'number') && Array.isArray(resourceValue)) {
        is = resourceValue.includes(subjectValue);
      }
      // [<some>] and <some>
      if ((typeof resourceValue === 'string' || typeof resourceValue === 'number') && Array.isArray(subjectValue)) {
        is = subjectValue.includes(resourceValue);
      }
    }

    if (AbilityCondition.not_in.isEqual(this.condition)) {
      // [<some>] and [<some>]
      if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = !subjectValue.some(v => resourceValue.find(v1 => v1 === v));
      }
      // <some> and [<some>]
      if ((typeof subjectValue === 'string' || typeof subjectValue === 'number') && Array.isArray(resourceValue)) {
        is = !resourceValue.includes(subjectValue);
      }
      // [<some>] and <some>
      if ((typeof resourceValue === 'string' || typeof resourceValue === 'number') && Array.isArray(subjectValue)) {
        is = !subjectValue.includes(resourceValue);
      }
    }

    this.state = is ? AbilityMatch.match : AbilityMatch.mismatch;

    return this.state;
  }

  /**
   * Extract values from the resourceData
   * @param resourceData - The resourceData to extract values from
   * @param environment - Environment data
   */
  public extractValues(
    resourceData: Resources | null,
    environment?: Environment | null,
  ): [
    string | number | boolean | (string | number)[] | null | undefined,
    string | number | boolean | (string | number)[] | null | undefined,
  ] {
    let subjectValue;
    let resourceValue;

    if (
      (resourceData === null || typeof resourceData === 'undefined') &&
      (environment === null || typeof environment === 'undefined')
    ) {
      return [NaN, NaN];
    }

    // left side resolve
    if (this.subject.includes('.')) {
      // if is environment
      if (this.subject.startsWith('env.') && typeof environment !== 'undefined') {
        subjectValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
          environment,
          this.subject.replace(/^env\./, ''),
        );
        // if is resource
      } else {
        subjectValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
          resourceData,
          this.subject,
        );
      }
    } else {
      subjectValue = this.subject;
    }

    // right side resolve
    if (typeof this.resource === 'string' && this.resource.includes('.')) {
      // if is environment
      if (this.resource.startsWith('env.') && typeof environment !== 'undefined') {
        resourceValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
          environment,
          this.resource.replace(/^env\./, ''),
        );
      } else {
        // if is resource
        resourceValue = this.getDotNotationValue<number | boolean | string | (string | number)[]>(
          resourceData,
          this.resource,
        );
      }
    } else {
      resourceValue = this.resource;
    }

    return [subjectValue, resourceValue];
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
      const match = new RegExp('(.+)\\[([0-9]*)]').exec(comp);

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

  public static parse<Resources extends object, Environment = unknown>(
    config: AbilityRuleConfig,
  ): AbilityRule<Resources, Environment> {
    const { id, name, subject, resource, condition } = config;

    return new AbilityRule<Resources, Environment>({
      id,
      name,
      subject,
      resource,
      condition: new AbilityCondition(condition),
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

  static equal<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.equal,
      subject,
      resource,
    });
  }

  static notIn<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_in,
      subject,
      resource,
    });
  }

  static in<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.in,
      subject,
      resource,
    });
  }

  static notEqual<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_equal,
      subject,
      resource,
    });
  }

  static lessThan<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.less_than,
      subject,
      resource,
    });
  }

  static lessOrEqual<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.less_or_equal,
      subject,
      resource,
    });
  }
  static moreThan<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.more_than,
      subject,
      resource,
    });
  }

  static moreOrEqual<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.more_or_equal,
      subject,
      resource,
    });
  }
}

export default AbilityRule;
