export type AbilityStatementStatus = 'permit' | 'deny';
type SubjectPrefix = 'subject.' | 'environment.';
export type AbilityStatementMatches = [
  `${SubjectPrefix}${string}`,
  AbilityCondition,
  string | number | boolean,
];
export type AbilityCondition = '=' | '<>' | '>' | '<' | '<=' | '>=' | 'in';

// type AddPrefix<TKey, TPrefix extends string> = TKey extends string ? `${TPrefix}${TKey}` : never;

// type AbilityStatementMatches<S extends object, O> = [
//   AddPrefix<keyof S, 'subject.' | 'environment.' | ''>,
//   AbilityCondition,
//   AddPrefix<keyof O, 'object.' | ''>,
// ][];

class AbilityStatement {
  #matches: AbilityStatementMatches;
  #name: string;
  #effect: AbilityStatementStatus;

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
   * and _The object_
   * ```json
   * {"departamentID": "154", "departamentName": "NBC"}
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "object.departamentName"]
   * ```
   *
   * \
   * **Example 2.**\
   * In this case will be compared object and string:
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The object_ will be «undefined».\
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "NBC"]
   * ```
   * \
   * **Example 3.**\
   * In this case will be compared object and array of string:\
   * \
   * _The subject_
   * ```json
   * {"userID": "1", "userDepartament": "NBC"}
   * ```
   * and _The object_
   * ```json
   * ["FOX", "NBC", "AONE"]
   * ```
   * \
   * Now we can make the matching rule:
   * ```json
   * ["subject.userDepartament", "=", "object"]
   * ```
   * **Note: In this rule whe set the object field as the «object» string.\
   * This means that we will compare the entire object as a whole,\
   * and not search for it by field name.**
   * \
   * **Example 4.**\
   * In this case will be compared object and array of string:\
   * \
   * _The subject_
   * ```json
   * {"user": {"account": {"roles": ["admin", "viewer"]}}}
   * ```
   * and _The object_
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
    this.#name = statementName;
    this.#effect = effect;
    this.#matches = matches;
  }

  public getName() {
    return this.#name;
  }

  public getEffect() {
    return this.#effect;
  }

  public enforce(
    subject: unknown,
    obj?: unknown | undefined,
    env?: unknown | undefined,
  ): AbilityStatementStatus {
    const [_subjectFieldName, condition, _objectFieldName] = this.#matches;

    let is: boolean = false;
    const [valueS, valueO] = this.extractValues(subject, obj, env);

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

    return is ? this.#effect : this.#effect === 'permit' ? 'deny' : 'permit';
  }

  protected extractValues(
    sub: unknown,
    obj?: unknown | undefined,
    env?: unknown | undefined,
  ): [
    string | number | boolean | (string | number)[] | null | undefined,
    string | number | boolean | (string | number)[] | null | undefined,
  ] {
    const [subjectFieldName, _condition, objectFieldName] = this.#matches;
    const REGEXP = /^(subject|object|environment)\./;

    //  The subject field must be named at «subject.<field-name>»
    if (!subjectFieldName.match(/^(subject|environment)\./)) {
      throw new Error(
        `Matches error. The subject field must be named at «subject.<field-name>», but got ${subjectFieldName}`,
      );
    }

    const sFieldName = subjectFieldName.replace(/^(subject|environment)\./, '');
    const subject = typeof sub === 'undefined' || sub === null ? {} : sub;
    const object = typeof obj === 'undefined' || obj === null ? {} : obj;

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

    // The object field name can be «object».
    // In this case the object be compare as is
    if (objectFieldName === 'object') {
      return [sValue, object] as ReturnType<AbilityStatement['extractValues']>;
    }

    // Object field name - is a «object.<field-name>»
    if (object && String(objectFieldName).match(REGEXP)) {
      const oFieldName = String(objectFieldName).replace(REGEXP, '');
      return [sValue, this.getDotNotationValue(object, oFieldName)] as ReturnType<
        AbilityStatement['extractValues']
      >;
    }

    // The object field abne can be «<some-value>» only
    if (String(objectFieldName).match(REGEXP) === null) {
      return [sValue, objectFieldName] as ReturnType<AbilityStatement['extractValues']>;
    }

    return [NaN, NaN];
  }

  protected getDotNotationValue(obj: unknown, desc: string) {
    const arr = desc.split('.');

    while (arr.length && obj) {
      const comp = arr.shift() || '';
      const match = new RegExp('(.+)\\[([0-9]*)\\]').exec(comp);

      if (match !== null && match.length == 3) {
        const arrayData = {
          arrName: match[1],
          arrIndex: match[2],
        };

        if (obj[arrayData.arrName as keyof typeof obj] !== undefined) {
          obj = obj[arrayData.arrName as keyof typeof obj][arrayData.arrIndex];
        } else {
          obj = undefined;
        }
      } else {
        obj = obj[comp as keyof typeof obj];
      }
    }

    return obj;
  }
}

export default AbilityStatement;
