import { ability, AbilityCondition, AbilityJSONParser } from '../../src';

describe('DSl to JSON and JSON to policies', () => {
  it('Checking the Annotations', () => {
    const policiesFromDSL = ability`
      @priority 2
      @name "policy-001"
      @disabled true
      @tags tag-001, tag-002
      permit permission.test if all:
        @name "ruleset-001"
        @disabled true
        all of:
          @name "rule-001"
          @disabled true
          user.firstName is equals 'Petrosyan'
          user.age is equals 16
    `;

    const json = AbilityJSONParser.toJSON(policiesFromDSL);
    const policiesFromJSON = AbilityJSONParser.parse(json);

    // Check length of policies
    expect(policiesFromDSL.length).toEqual(policiesFromJSON.length);

    // Check policy priority
    expect(policiesFromJSON[0].priority).toBe(2);

    // Check policy name
    expect(policiesFromJSON[0].name).toBe('policy-001');

    // Check disabled
    expect(policiesFromJSON[0].disabled).toBeTruthy();

    // Check policy tags
    expect(policiesFromJSON[0].tags).toContain('tag-001');
    expect(policiesFromJSON[0].tags).toContain('tag-002');

    // Check rule set name
    expect(policiesFromJSON[0].ruleSet[0].name).toEqual('ruleset-001');

    // Check rule set disabled
    expect(policiesFromJSON[0].ruleSet[0].disabled).toBeTruthy();

    // Check rule disabled
    expect(policiesFromJSON[0].ruleSet[0].rules[0].disabled).toBeTruthy();

    // Check rule name
    expect(policiesFromJSON[0].ruleSet[0].rules[0].name).toEqual('rule-001');
  });

  it('Checking base operators', () => {
    const policiesFromDSL = ability`
      permit permission.test if all:
        user.name is equals 'Name'
        user.name is not equals 'Name'
        user.name is null
        user.name is not null
        user.name is defined
        user.name is not defined
        user.name length > 12
        user.name len less than 12
    `;

    const json = AbilityJSONParser.toJSON(policiesFromDSL);
    const policiesFromJSON = AbilityJSONParser.parse(json);

    expect(policiesFromJSON.length).toBe(1);
    expect(policiesFromJSON[0].ruleSet.length).toEqual(1);

    const rules = policiesFromJSON[0].ruleSet[0].rules;
    expect(rules[0].subject).toEqual('user.name');

    // operators check
    expect(rules[0].condition).toEqual(AbilityCondition.equals);
    expect(rules[1].condition).toEqual(AbilityCondition.not_equals);
    expect(rules[2].condition).toEqual(AbilityCondition.equals);
    expect(rules[3].condition).toEqual(AbilityCondition.not_equals);
    expect(rules[4].condition).toEqual(AbilityCondition.defined);
    expect(rules[5].condition).toEqual(AbilityCondition.not_defined);
    expect(rules[6].condition).toEqual(AbilityCondition.length_greater_than);
    expect(rules[7].condition).toEqual(AbilityCondition.length_less_than);

    // const rule2 = rules[1];
  });
});
