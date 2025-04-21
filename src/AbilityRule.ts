export type AbilityRuleState = 'match' | 'mismatch' | 'pending';
export type SubjectPrefix = 'subject.' | 'environment.';
export type AbilityRuleMatches = [
  `${SubjectPrefix}${string}`,
  AbilityCondition,
    string | number | boolean,
];
export type AbilityCondition = '=' | '<>' | '>' | '<' | '<=' | '>=' | 'in' | 'not in';

export type AbilityRuleConfig = {
  readonly name?: string | symbol;
  readonly matches: AbilityRuleMatches;
};


export class AbilityRule<Subject = unknown, Resource = unknown, Environment = unknown> {
  public matches: AbilityRuleMatches;
  public name: string | symbol;
  public state: AbilityRuleState = 'pending';

  /**
   * Create the rule to compare
   *
   * \
   * For example, be compared two's data\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartment": "NBC"}
   * ```
   * and _The resource_
   * ```json
   * {"departmentID": "154", "departmentName": "NBC"}
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartment", "=", "resource.departmentName"]
   * ```
   *
   * \
   * **Example 2.**\
   * In this case will be compared resource and string:
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartment": "NBC"}
   * ```
   * and _The resource_ will be «undefined».\
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartment", "=", "NBC"]
   * ```
   * \
   * **Example 3.**\
   * In this case will be compared resource and array of string:\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartment": "NBC"}
   * ```
   * and _The resource_
   * ```json
   * ["FOX", "NBC", "AONE"]
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartment", "=", "resource"]
   * ```
   * **Note: In this rule whe set the resource field as the «resource» string.\
   * This means that we will compare the entire resource as a whole,\
   * and not search for it by field name.**
   * \
   * **Example 4.**\
   * In this case will be compared resource and array of string:\
   * \
   * _The subject_
   * ```json
   * {"user": {"account": {"roles": ["admin", "viewer"]}}}
   * ```
   * and _The resource_
   * ```json
   * undefined
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.user.account.roles", "in", "admin"]
   */
  public constructor(
    params: {
      matches: AbilityRuleMatches;
      name?: string | symbol;
    },
  ) {
    const {
      name,
      matches,
    } = params;
    this.name = name || Symbol('name');
    this.matches = matches;
  }

  // public isPermit(
  //   subject: Subject,
  //   resource?: Resource | undefined,
  //   environment?: Environment | undefined,
  // ): boolean {
  //   return 'permit' === this.check(subject, resource, environment);
  // }
  //
  // public isDeny(
  //   subject: Subject,
  //   resource?: Resource | undefined,
  //   environment?: Environment | undefined,
  // ): boolean {
  //   return 'deny' === this.check(subject, resource, environment);
  // }

  public check(
    subject: Subject,
    resource?: Resource | undefined,
    environment?: Environment | undefined,
  ): AbilityRuleState {
    const [_subjectFieldName, condition, _resourceFieldName] = this.matches;

    let is: boolean = false;
    const [valueS, valueO] = this.extractValues(subject, resource, environment);

    if (condition === '<') {
      is = Number(valueS) < Number(valueO);
    }

    if (condition === '<=') {
      is = Number(valueS) <= Number(valueO);
    }

    if (condition === '>') {
      is = Number(valueS) > Number(valueO);
    }

    if (condition === '>=') {
      is = Number(valueS) >= Number(valueO);
    }

    if (condition === '=') {
      is = valueS === valueO;
    }

    if (condition === '<>') {
      is = valueS !== valueO;
    }

    if (condition === 'in') {
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

    if (condition === 'not in') {
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

    this.state = is ? 'match' : 'mismatch';

    return this.state;
  }


  public extractValues(
    sub: unknown,
    res?: unknown | undefined,
    env?: unknown | undefined,
  ): [
      string | number | boolean | (string | number)[] | null | undefined,
      string | number | boolean | (string | number)[] | null | undefined,
  ] {
    const [subjectFieldName, _condition, resourceFieldName] = this.matches;
    const REGEXP = /^(subject|resource|environment)\./;

    //  The subject field must be named at «subject.<field-name>»
    if (!subjectFieldName.match(/^(subject|environment)\./)) {
      throw new Error(
        `Matches error. The subject field must be named at «subject.<field-name>», but got ${subjectFieldName}`,
      );
    }

    const sFieldName = subjectFieldName.replace(/^(subject|environment)\./, '');
    const subject = typeof sub === 'undefined' || sub === null ? {} : sub;
    const resource = typeof res === 'undefined' || res === null ? {} : res;

    const sValue = subject
      ? this.getDotNotationValue(
        subjectFieldName.match(/^subject\./)
          ? subject
          : subjectFieldName.match(/^environment\./)
            ? env
            : {},
        sFieldName,
      )
      : subject;

    // The resource field name can be «resource».
    // In this case the resource be comparing as is
    if (resourceFieldName === 'resource') {
      return [sValue, resource] as ReturnType<AbilityRule['extractValues']>;
    }

    // Object field name - is a «resource.<field-name>»
    if (resource && String(resourceFieldName).match(REGEXP)) {
      const oFieldName = String(resourceFieldName).replace(REGEXP, '');
      return [sValue, this.getDotNotationValue(resource, oFieldName)] as ReturnType<
        AbilityRule['extractValues']
      >;
    }

    // The resource field can be «<some-value>» only
    if (String(resourceFieldName).match(REGEXP) === null) {
      return [sValue, resourceFieldName] as ReturnType<AbilityRule['extractValues']>;
    }

    return [NaN, NaN];
  }

  public getDotNotationValue(resource: unknown, desc: string) {
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

    return resource;
  }

  /**
   * Parsing the rule config object or JSON string\
   * of config and returns the AbilityRule class instance
   */
  public static parse<Subject = unknown, Resource = unknown, Environment = unknown>(
    configOrJson: AbilityRuleConfig | string,
  ): AbilityRule<Subject, Resource, Environment> {
    const { name, matches } =
      typeof configOrJson === 'string'
        ? (JSON.parse(configOrJson) as AbilityRuleConfig)
        : configOrJson;

    return new AbilityRule<Subject, Resource, Environment>({
      matches, name,
    });
  }

  /**
   * Export the rule to config object
   */
  public static export(rule: AbilityRule): AbilityRuleConfig {
    return {
      name: rule.name,
      matches: rule.matches,
    };

  }
}

export default AbilityRule;
