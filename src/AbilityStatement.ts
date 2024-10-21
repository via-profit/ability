export type AbilityStatementStatus = 'permit' | 'deny';
export type AbilityStatementMatches = [string, AbilityCondition, string | number];
export type AbilityCondition = '=' | '<>' | '>' | '<' | '<=' | '>=' | 'in';

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

  public enforce(subject: unknown, obj?: unknown | undefined): AbilityStatementStatus {
    const [_subjectFieldName, condition, _objectFieldName] = this.#matches;

    let is: boolean = false;
    const [valueS, valueO] = this.extractValues(subject, obj);

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
    subject: unknown,
    obj?: unknown | undefined,
  ): [
    string | number | boolean | (string | number)[] | null | undefined,
    string | number | boolean | (string | number)[] | null | undefined,
  ] {
    const [subjectFieldName, _condition, objectFieldName] = this.#matches;
    const REGEXP = /^(subject|object)\./;

    //  The subject field must be named at «subject.<field-name>»
    if (!subjectFieldName.match(REGEXP)) {
      throw new Error(
        `Matches error. The subject field must be named at «subject.<field-name>», but got ${subjectFieldName}`,
      );
    }

    // The subject must be an object only
    if (typeof subject !== 'object') {
      throw new Error(`The subject reference must be an object type, but got «${subject}»`);
    }

    const sFieldName = subjectFieldName.replace(REGEXP, '');

    // The subject field must be null, or any value, not undefined
    if (subject !== null && typeof this.getDotNotationValue(subject, sFieldName) === 'undefined') {
      return [NaN, NaN];
    }

    const sValue = subject ? this.getDotNotationValue(subject, sFieldName) : subject;

    // The object field name can be «object».
    // In this case the object be compare as is
    if (objectFieldName === 'object') {
      return [sValue, obj] as ReturnType<AbilityStatement['extractValues']>;
    }

    // Object field name - is a «object.<field-name>»
    if (obj && String(objectFieldName).match(REGEXP)) {
      const oFieldName = String(objectFieldName).replace(REGEXP, '');
      return [sValue, this.getDotNotationValue(obj, oFieldName)] as ReturnType<
        AbilityStatement['extractValues']
      >;
    }

    // The object field abne can be «<some-value>» only
    if (String(objectFieldName).match(REGEXP) === null) {
      return [sValue, objectFieldName] as ReturnType<AbilityStatement['extractValues']>;
    }

   
    return [NaN, NaN]
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
