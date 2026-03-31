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
  readonly resource: string | number | boolean | null | (string | number | boolean | null)[];

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
  public resource: AbilityRuleConfig['resource'];

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
    const isValue = (v: unknown): v is string | number | boolean | null =>
      typeof v === 'string' || typeof v === 'number' || typeof v === 'boolean' || v === null;


    // always
    if(AbilityCondition.always.isEqual(this.condition)) {
      is = true;
    }

    
    // never
    if(AbilityCondition.never.isEqual(this.condition)) {
      is = false;
    }


    // equals
    if (AbilityCondition.equals.isEqual(this.condition)) {
      is = subjectValue === resourceValue;
    }

    // not equals
    if (AbilityCondition.not_equals.isEqual(this.condition)) {
      is = subjectValue !== resourceValue;
    }

    // less than
    if (AbilityCondition.less_than.isEqual(this.condition)) {
      if (typeof subjectValue === 'number' && typeof resourceValue === 'number') {
        is = subjectValue < resourceValue;
      }
    }

    // less or equal
    if (AbilityCondition.less_or_equal.isEqual(this.condition)) {
      if (typeof subjectValue === 'number' && typeof resourceValue === 'number') {
        is = subjectValue <= resourceValue;
      }
    }

    // more than
    if (AbilityCondition.greater_than.isEqual(this.condition)) {
      if (typeof subjectValue === 'number' && typeof resourceValue === 'number') {
        is = subjectValue > resourceValue;
      }
    }

    // more or equal
    if (AbilityCondition.greater_or_equal.isEqual(this.condition)) {
      if (typeof subjectValue === 'number' && typeof resourceValue === 'number') {
        is = subjectValue >= resourceValue;
      }
    }

    // in
    if (AbilityCondition.in.isEqual(this.condition)) {
      // value in array
      if (isValue(subjectValue) && Array.isArray(resourceValue)) {
        is = resourceValue.includes(subjectValue);
      }
      // array intersects array
      else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = subjectValue.some(v => resourceValue.includes(v));
      }
    }

    // not in
    if (AbilityCondition.not_in.isEqual(this.condition)) {
      if (isValue(subjectValue) && Array.isArray(resourceValue)) {
        is = !resourceValue.includes(subjectValue);
      } else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = !subjectValue.some(v => resourceValue.includes(v));
      }
    }

    // contains
    if (AbilityCondition.contains.isEqual(this.condition)) {
      // array contains value
      if (Array.isArray(subjectValue) && isValue(resourceValue)) {
        is = subjectValue.includes(resourceValue);
      }
      // array intersects array
      else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = subjectValue.some(v => resourceValue.includes(v));
      }
    }

    // not contains
    if (AbilityCondition.not_contains.isEqual(this.condition)) {
      if (Array.isArray(subjectValue) && isValue(resourceValue)) {
        is = !subjectValue.includes(resourceValue);
      } else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = !subjectValue.some(v => resourceValue.includes(v));
      }
    }

    // length equals
    if (AbilityCondition.length_equals.isEqual(this.condition)) {
      // foo.bar == n
      if (isValue(subjectValue) && typeof resourceValue === 'number') {
        is = String(subjectValue).length === resourceValue;
      }
      // ['foo', 'bar'] = n
      else if (Array.isArray(subjectValue) && typeof resourceValue === 'number') {
        is = subjectValue.length === resourceValue;
      }
      // ['foo', 'bar'] = ['baz', 'taz']
      else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = subjectValue.length === resourceValue.length;
      }
      // 'foo' = 'bar'
      else if (typeof subjectValue === 'string' && typeof resourceValue === 'string') {
        is = subjectValue.length === resourceValue.length;
      }
    }

    // length greater than
    if (AbilityCondition.length_greater_than.isEqual(this.condition)) {
      // foo.bar > n
      if (isValue(subjectValue) && typeof resourceValue === 'number') {
        is = String(subjectValue).length > resourceValue;
      }
      // ['foo', 'bar'] > n
      else if (Array.isArray(subjectValue) && typeof resourceValue === 'number') {
        is = subjectValue.length > resourceValue;
      }
      // ['foo', 'bar'] > ['baz', 'taz']
      else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = subjectValue.length > resourceValue.length;
      }
      // 'foo' > 'bar'
      else if (typeof subjectValue === 'string' && typeof resourceValue === 'string') {
        is = subjectValue.length > resourceValue.length;
      }
    }

    // length greater than
    if (AbilityCondition.length_less_than.isEqual(this.condition)) {
      // foo.bar < n
      if (isValue(subjectValue) && typeof resourceValue === 'number') {
        is = String(subjectValue).length < resourceValue;
      }
      // ['foo', 'bar'] < n
      else if (Array.isArray(subjectValue) && typeof resourceValue === 'number') {
        is = subjectValue.length < resourceValue;
      }
      // ['foo', 'bar'] < ['baz', 'taz']
      else if (Array.isArray(subjectValue) && Array.isArray(resourceValue)) {
        is = subjectValue.length < resourceValue.length;
      }
      // 'foo' < 'bar'
      else if (typeof subjectValue === 'string' && typeof resourceValue === 'string') {
        is = subjectValue.length < resourceValue.length;
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
  ): [AbilityRuleConfig['resource'] | undefined, AbilityRuleConfig['resource'] | undefined] {
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
        subjectValue = this.getDotNotationValue<AbilityRuleConfig['resource']>(
          environment,
          this.subject.replace(/^env\./, ''),
        );
        // if is resource
      } else {
        subjectValue = this.getDotNotationValue<AbilityRuleConfig['resource']>(
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
        resourceValue = this.getDotNotationValue<AbilityRuleConfig['resource']>(
          environment,
          this.resource.replace(/^env\./, ''),
        );
      } else {
        // if is resource
        resourceValue = this.getDotNotationValue<AbilityRuleConfig['resource']>(
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

  public toString(): string {
    return `AbilityRule: ${this.name} condition: ${this.condition.code} subject: "${this.subject?.toString()}" resource: "${this.resource?.toString()}"`;
  }

  public copyWith(
    props: Partial<{
      id: string | null;
      name: string | null;
      subject: string;
      resource: AbilityRuleConfig['resource'];
      condition: AbilityCondition;
    }>,
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      id: props.id ?? this.id,
      name: props.name ?? this.name,
      subject: props.subject ?? this.subject,
      resource: props.resource ?? this.resource,
      condition: props.condition ?? this.condition,
    });
  }

  static equals<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.equals,
      subject,
      resource,
    });
  }

  static notEquals<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_equals,
      subject,
      resource,
    });
  }

  static contains<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.contains,
      subject,
      resource,
    });
  }

  static notContains<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_contains,
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
      condition: AbilityCondition.not_equals,
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
      condition: AbilityCondition.greater_than,
      subject,
      resource,
    });
  }

  static moreOrEqual<Resources extends object = object, Environment = unknown>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.greater_or_equal,
      subject,
      resource,
    });
  }
}

export default AbilityRule;
