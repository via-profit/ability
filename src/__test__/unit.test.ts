import AbilityRule from '../AbilityRule';
import AbilityCondition from '../AbilityCondition';
import AbilityCode from '../AbilityCode';
import AbilityCompare from '../AbilityCompare';
import AbilityResolver from '../AbilityResolver';

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
    condition: AbilityCondition.equal,
    subject: 'subject.',
    resource: '',
  }).proxyDotNotation(resource, 'foo.bar.baz.taz');

  expect(value).toBe(resource.foo.bar.baz.taz);
});



test('isInActionContain', () => {
  const isFalsy = AbilityResolver.isInActionContain('account.read', 'account.private.read');
  const isTruthy = AbilityResolver.isInActionContain('account.some.foo.bar', 'account.some.foo.bar');
  const isTruthyToo = AbilityResolver.isInActionContain('account.some.foo', 'account.some.*');


  expect(isFalsy).toBeFalsy();
  expect(isTruthy).toBeTruthy();
  expect(isTruthyToo).toBeTruthy();
})