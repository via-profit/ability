import AbilityStatement from '~/AbilityStatement';

test('Dot notation path foo.bar.baz.taz returns 2', () => {
  class Test extends AbilityStatement {
    public proxyDotNotation(o: unknown, p: string) {
      return this.getDotNotationValue(o, p);
    }
  }

  const object = {
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
  const value = new Test('sa', ['', '=', '']).proxyDotNotation(object, 'foo.bar.baz.taz');

  expect(value).toBe(object.foo.bar.baz.taz);
});
