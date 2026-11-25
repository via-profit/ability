import ts from 'typescript';
import {
  ability,
  AbilityDSLParser,
  AbilityJSONParser,
  AbilityParserError,
  AbilityResolver,
  AbilityRule,
  AbilityTypeGenerator,
  DenyOverridesStrategy,
  FirstMatchStrategy,
  fromLiteral,
  AbilityCondition,
} from '../../src';

describe('Access decision validation', () => {
  it('rejects template substitutions before converting them to text', () => {
    const value = { toString: jest.fn(() => 'reader') };
    // @ts-expect-error JavaScript callers must also get a runtime error.
    expect(() => ability`# ${value}`).toThrow(AbilityParserError);
    expect(value.toString).not.toHaveBeenCalled();
  });

  it('only uses own attributes from resources and environments', () => {
    const resolver = new AbilityResolver(
      ability`
      permit permission.document.edit if all:
        user.active is true
        env.enabled is true
    `,
      DenyOverridesStrategy,
    );
    const user = Object.create({ active: true });
    const environment = Object.create({ enabled: true });

    expect(resolver.resolve('document.edit', { user }, { enabled: true }).isDenied()).toBe(true);
    expect(
      resolver.resolve('document.edit', { user: { active: true } }, environment).isDenied(),
    ).toBe(true);
    expect(
      resolver.resolve('document.edit', { user: { active: true } }, { enabled: true }).isAllowed(),
    ).toBe(true);
  });

  it('requires both attributes for an equality comparison between paths', () => {
    const resolver = new AbilityResolver(
      ability`
      permit permission.document.edit if all:
        document.ownerId equals env.user.id
    `,
      DenyOverridesStrategy,
    );

    expect(resolver.resolve('document.edit', {}).isDenied()).toBe(true);
    expect(
      resolver
        .resolve('document.edit', { document: { ownerId: 1 } }, { user: { id: 1 } })
        .isAllowed(),
    ).toBe(true);
  });

  it('rejects malformed array indexes and still reads valid ones', () => {
    const rule = new AbilityRule({
      id: null,
      name: null,
      subject: 'members[0].active',
      condition: AbilityCondition.equals,
      resource: true,
    });
    const resource = { members: [{ active: true }] };

    expect(rule.getDotNotationValue(resource, 'members[0].active')).toBe(true);
    expect(rule.getDotNotationValue(resource, 'members[].active')).toBeUndefined();
  });

  it('rejects inherited names in the condition lookup', () => {
    expect(() => fromLiteral('toString' as never)).toThrow(AbilityParserError);
  });

  it('denies an unknown effect returned by a strategy', () => {
    const policies = ability`
      permit permission.document.edit if all:
        always
    `;
    Reflect.set(policies[0], 'effect', 'unknown');

    const onAllow = jest.fn();
    const resolver = new AbilityResolver(policies, FirstMatchStrategy, { onAllow });

    expect(resolver.resolve('document.edit', {}).isDenied()).toBe(true);
    expect(() => resolver.enforce('document.edit', {})).toThrow('Permission denied');
    expect(onAllow).not.toHaveBeenCalled();
  });

  it('rejects invalid JSON policy effects, comparison methods and conditions', () => {
    const [policy] = AbilityJSONParser.toJSON(ability`
      permit permission.document.edit if all:
        always
    `);
    const [group] = policy.ruleSet;
    const [rule] = group.rules;

    expect(() => AbilityJSONParser.parsePolicy({ ...policy, effect: 'unknown' as never })).toThrow(
      AbilityParserError,
    );
    expect(() =>
      AbilityJSONParser.parseRuleSet({ ...group, compareMethod: 'unknown' as never }),
    ).toThrow(AbilityParserError);
    expect(() => AbilityJSONParser.parseRule({ ...rule, condition: 'unknown' as never })).toThrow(
      AbilityParserError,
    );
  });

  it('emits valid types for quoted tags, annotation text and prototype-named keys', () => {
    const policies = ability`
      @tags "o'reilly"
      @name 'Read */ document'
      permit permission.__proto__ if all:
        document.status equals 'ready'
    `;
    policies[0].ruleSet[0].rules[0].subject = 'document.review-status';

    const source = new AbilityTypeGenerator(policies).generateTypeDefs();
    const { diagnostics } = ts.transpileModule(source, { reportDiagnostics: true });

    expect(diagnostics).toEqual([]);
    expect(source).toContain("['__proto__']");
    expect(source).toContain("readonly 'review-status'?");
    expect(source).toContain("o\\'reilly");
  });

  it.each([
    ['@name "unfinished', 'Unterminated annotation string'],
    ['@priority 9007199254740992', 'expects an integer in the safe range'],
  ])('rejects invalid policy annotations', (annotation, message) => {
    expect(() =>
      new AbilityDSLParser(`
      ${annotation}
      permit permission.document.edit if all:
        always
    `).parse(),
    ).toThrow(message);
  });
});
