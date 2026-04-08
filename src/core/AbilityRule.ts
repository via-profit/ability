import {AbilityMatch, AbilityMatchType} from './AbilityMatch';
import {
  AbilityCondition,
  AbilityConditionType,
  AbilityConditionLiteral,
  toLiteral,
} from './AbilityCondition';

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

  readonly condition: AbilityConditionType;

  readonly disabled?: boolean;
};

export type AbilityRuleConstructorProps = Omit<AbilityRuleConfig, 'condition'> & {
  readonly condition: AbilityConditionType;
};

/**
 * Represents a rule that defines a condition to be checked against a subject and resource.
 */
export class AbilityRule<Resources extends object = object, Environment extends object = object> {
  /**
   * Subject key path like a 'user.name'
   */
  public subject: string;
  /**
   * Resource key path like a 'user.name' or value
   */
  public resource: AbilityRuleConfig['resource'];

  public condition: AbilityConditionType;
  public name: string;
  public id: string;
  public state: AbilityMatchType = AbilityMatch.pending;
  public disabled: boolean;

  /**
   * Creates an instance of AbilityRule.
   * @param {string} params.id - The unique identifier of the rule.
   * @param {string} params.name - The name of the rule.
   * @param {AbilityCondition} params.condition - The condition to evaluate.
   * @param {string} params.subject - The subject of the rule.
   * @param {string} params.resource - The resource to compare against.
   * @param {boolean} params.disabled - Disabling flag.
   * @param params
   */
  public constructor(params: AbilityRuleConstructorProps) {
    const { id, name, subject, resource, condition, disabled } = params;
    this.name = name || `rule:${JSON.stringify(subject)}:${condition}:${JSON.stringify(resource)}`;
    this.id = id || this.name;
    this.disabled = typeof disabled === 'boolean' ? disabled : false;

    this.subject = subject;
    this.resource = resource;
    this.condition = condition;
    this.state = this.disabled ? AbilityMatch.disabled : this.state;
  }

  private isPrimitive(v: unknown): v is string | number | boolean | null {
    return typeof v === 'string' || typeof v === 'number' || typeof v === 'boolean' || v === null;
  }

  private isNumber(v: unknown): v is number {
    return typeof v === 'number';
  }

  private isString(v: unknown): v is string {
    return typeof v === 'string';
  }

  private valueLen = (v: unknown): number | null =>
    this.isString(v) || Array.isArray(v) ? v.length : null;

  private operatorHandlers = {
    [toLiteral(AbilityCondition.always)]: () => true,
    [toLiteral(AbilityCondition.never)]: () => false,
    [toLiteral(AbilityCondition.equals)]: (a: unknown, b: unknown) => a === b,
    [toLiteral(AbilityCondition.not_equals)]: (a: unknown, b: unknown) => a !== b,
    [toLiteral(AbilityCondition.contains)]: (a: unknown, b: unknown) => {
      if (Array.isArray(a) && this.isPrimitive(b)) {
        return a.includes(b);
      }
      if (Array.isArray(a) && Array.isArray(b)) {
        return a.some(v => b.includes(v));
      }
      return false;
    },
    [toLiteral(AbilityCondition.not_contains)]: (a: unknown, b: unknown) => {
      if (Array.isArray(a) && this.isPrimitive(b)) {
        return !a.includes(b);
      }
      if (Array.isArray(a) && Array.isArray(b)) {
        return !a.some(v => b.includes(v));
      }
      return false;
    },
    [toLiteral(AbilityCondition.in)]: (a: unknown, b: unknown) => {
      if (this.isPrimitive(a) && Array.isArray(b)) {
        return b.includes(a);
      }
      if (Array.isArray(a) && Array.isArray(b)) {
        return a.some(v => b.includes(v));
      }
      return false;
    },
    [toLiteral(AbilityCondition.not_in)]: (a: unknown, b: unknown) => {
      if (this.isPrimitive(a) && Array.isArray(b)) {
        return !b.includes(a);
      }
      if (Array.isArray(a) && Array.isArray(b)) {
        return !a.some(v => b.includes(v));
      }
      return false;
    },
    [toLiteral(AbilityCondition.greater_than)]: (a: unknown, b: unknown) => {
      return this.isNumber(a) && this.isNumber(b) ? a > b : false;
    },
    [toLiteral(AbilityCondition.less_than)]: (a: unknown, b: unknown) => {
      return this.isNumber(a) && this.isNumber(b) ? a < b : false;
    },
    [toLiteral(AbilityCondition.greater_or_equal)]: (a: unknown, b: unknown) => {
      return this.isNumber(a) && this.isNumber(b) ? a >= b : false;
    },
    [toLiteral(AbilityCondition.less_or_equal)]: (a: unknown, b: unknown) => {
      return this.isNumber(a) && this.isNumber(b) ? a <= b : false;
    },
    [toLiteral(AbilityCondition.length_greater_than)]: (a: unknown, b: unknown) => {
      const alen = this.valueLen(a);
      if (alen === null) {
        return false;
      }

      if (this.isNumber(b)) {
        return alen > b;
      }

      const bLen = this.valueLen(b);
      if (bLen !== null) {
        return alen > bLen;
      }

      return false;
    },
    [toLiteral(AbilityCondition.length_less_than)]: (a: unknown, b: unknown) => {
      const alen = this.valueLen(a);
      if (alen === null) {
        return false;
      }

      if (this.isNumber(b)) {
        return alen < b;
      }

      const bLen = this.valueLen(b);
      if (bLen !== null) {
        return alen < bLen;
      }
      return false;
    },
    [toLiteral(AbilityCondition.length_equals)]: (a: unknown, b: unknown) => {
      const alen = this.valueLen(a);
      if (alen === null) {
        return false;
      }
      if (this.isNumber(b)) {
        return alen === b;
      }
      const bLen = this.valueLen(b);
      if (bLen !== null) {
        return alen === bLen;
      }
      return false;
    },
  } as {
    [K in AbilityConditionLiteral]: (a: unknown, b: unknown) => boolean;
  };

  /**
   * Check if the rule is matched
   * @param resource - The resource to check
   * @param environment
   */
  public check(resource: Resources | null, environment?: Environment): AbilityMatchType {

    if (this.disabled) {
      this.state = AbilityMatch.disabled;
      return this.state;
    }

    const [subjectValue, resourceValue] = this.extractValues(resource, environment);
    const handler = this.operatorHandlers[toLiteral(this.condition)];
    const result = handler(subjectValue, resourceValue);

    this.state = result ? AbilityMatch.match : AbilityMatch.mismatch;

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
    return `AbilityRule: ${this.name} condition: ${toLiteral(this.condition)} subject: "${this.subject?.toString()}" resource: "${this.resource?.toString()}"`;
  }

  public copyWith(
    props: Partial<{
      id: string | null;
      name: string | null;
      subject: string;
      resource: AbilityRuleConfig['resource'];
      condition: AbilityConditionType;
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

  static equals<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.equals,
      subject,
      resource,
    });
  }

  static notEquals<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_equals,
      subject,
      resource,
    });
  }

  static contains<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.contains,
      subject,
      resource,
    });
  }

  static notContains<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_contains,
      subject,
      resource,
    });
  }

  static notIn<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_in,
      subject,
      resource,
    });
  }

  static in<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.in,
      subject,
      resource,
    });
  }

  static notEqual<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.not_equals,
      subject,
      resource,
    });
  }

  static lessThan<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.less_than,
      subject,
      resource,
    });
  }

  static lessOrEqual<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.less_or_equal,
      subject,
      resource,
    });
  }
  static moreThan<Resources extends object = object, Environment extends object = object>(
    subject: string,
    resource: AbilityRuleConfig['resource'],
  ): AbilityRule<Resources, Environment> {
    return new AbilityRule<Resources, Environment>({
      condition: AbilityCondition.greater_than,
      subject,
      resource,
    });
  }

  static moreOrEqual<Resources extends object = object, Environment extends object = object>(
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
