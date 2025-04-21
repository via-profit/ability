import AbilityRule from '../AbilityRule';

test('Dot notation path foo.bar.baz.taz returns 2', () => {
  class Test extends AbilityRule {
    public proxyDotNotation(o: unknown, p: string) {
      return this.getDotNotationValue(o, p);
    }
  }

  const resource = {
    any: 6,
    foo: {
      any: 7,
      bar: {
        any: 8,
        baz: {
          any: 9,
          taz: 1,
        },
      },
    },
  };
  const value = new Test({
    matches: ['subject.', '=', ''],
  }).proxyDotNotation(resource, 'foo.bar.baz.taz');

  expect(value).toBe(resource.foo.bar.baz.taz);
});
