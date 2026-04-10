import { AbilityDSLParser } from './AbilityDSLParser';
import { EnvironmentObject, ResourceObject } from '~/core/AbilityTypeGenerator';

export function ability<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
  T extends string = string,
>(strings: TemplateStringsArray, ...expr: any[]) {
  const dsl = strings.reduce((acc, s, i) => acc + s + (expr[i] ?? ''), '');
  return new AbilityDSLParser<R, E, T>(dsl).parse();
}


