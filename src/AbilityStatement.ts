export type AbilityStatementStatus = 'permit' | 'deny';
type SubjectPrefix = 'subject.' | 'environment.';
export type AbilityStatementMatches = [
  `${SubjectPrefix}${string}`,
  AbilityCondition,
  string | number | boolean,
];
export type AbilityCondition = '=' | '<>' | '>' | '<' | '<=' | '>=' | 'in';

// type AddPrefix<TKey, TPrefix extends string> = TKey extends string ? `${TPrefix}${TKey}` : never;

// type AbilityStatementMatches<S extends resource, O> = [
//   AddPrefix<keyof S, 'subject.' | 'environment.' | ''>,
//   AbilityCondition,
//   AddPrefix<keyof O, 'resource.' | ''>,
// ][];

class AbilityStatement<Subject = unknown, Resource = unknown, Environment = unknown> {
  public matches: AbilityStatementMatches;
  public name: string;
  public effect: AbilityStatementStatus;

  /**
   * Create the statement to compare
   *
   * @param statementName {string} - The statement name
   * @param effect {AbilityStatementStatus} - Return value
   * @param matches {AbilityStatementMatches} - The matching rule he matching rule can be on of the format:
   * \
   * For example, be compared two's data\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The resource_
   * ```json
   * {"departamentID": "154", "departamentName": "NBC"}
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "resource.departamentName"]
   * ```
   *
   * \
   * **Example 2.**\
   * In this case will be compared resource and string:
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The resource_ will be «undefined».\
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "NBC"]
   * ```
   * \
   * **Example 3.**\
   * In this case will be compared resource and array of string:\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The resource_
   * ```json
   * ["FOX", "NBC", "AONE"]
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "resource"]
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
    statementName: string,
    matches: AbilityStatementMatches,
    effect: AbilityStatementStatus = 'permit',
  ) {
    this.name = statementName;
    this.effect = effect;
    this.matches = matches;
  }

  public getName() {
    return this.name;
  }

  public getEffect() {
    return this.effect;
  }

  public isPermit(
    subject: Subject,
    resource?: Resource | undefined,
    environment?: Environment | undefined,
  ): boolean {
    return 'permit' === this.check(subject, resource, environment);
  }

  public isDeny(
    subject: Subject,
    resource?: Resource | undefined,
    environment?: Environment | undefined,
  ): boolean {
    return 'deny' === this.check(subject, resource, environment);
  }

  public check(
    subject: Subject,
    resource?: Resource | undefined,
    environment?: Environment | undefined,
  ): AbilityStatementStatus {
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

    return is ? this.effect : this.effect === 'permit' ? 'deny' : 'permit';
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
    // In this case the resource be compare as is
    if (resourceFieldName === 'resource') {
      return [sValue, resource] as ReturnType<AbilityStatement['extractValues']>;
    }

    // Object field name - is a «resource.<field-name>»
    if (resource && String(resourceFieldName).match(REGEXP)) {
      const oFieldName = String(resourceFieldName).replace(REGEXP, '');
      return [sValue, this.getDotNotationValue(resource, oFieldName)] as ReturnType<
        AbilityStatement['extractValues']
      >;
    }

    // The resource field abne can be «<some-value>» only
    if (String(resourceFieldName).match(REGEXP) === null) {
      return [sValue, resourceFieldName] as ReturnType<AbilityStatement['extractValues']>;
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
}

export default AbilityStatement;
