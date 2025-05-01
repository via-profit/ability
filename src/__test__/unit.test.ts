import AbilityRule from '../AbilityRule';
import AbilityCondition from '../AbilityCondition';

class Test extends AbilityRule {
  public proxyDotNotation(o: unknown, p: string) {
    return this.getDotNotationValue(o, p);
  }
}

test('Dot notation path foo.bar.baz.taz returns 2', () => {
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
    id: '23906511-fef3-4c2b-8651-a7a148c01e60',
    name: 'Dot notation path foo.bar.baz.taz returns 2',
    condition: AbilityCondition.EQUAL.code,
    subject: 'subject.',
    resource: '',
  }).proxyDotNotation(resource, 'foo.bar.baz.taz');

  expect(value).toBe(resource.foo.bar.baz.taz);
});
