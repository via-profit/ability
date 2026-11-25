import { AbilityDSLParser } from './AbilityDSLParser';
import { AbilityParserError } from '../../core/AbilityError';
import type { EnvironmentObject, ResourceObject } from '../../core/AbilityTypeGenerator';

export function ability<
  R extends ResourceObject = Record<string, unknown>,
  E extends EnvironmentObject = Record<string, unknown>,
  T extends string = string,
>(strings: TemplateStringsArray, ...expressions: never[]) {
  if (strings.length !== 1 || expressions.length !== 0) {
    throw new AbilityParserError(
      'The ability template does not support substitutions. Pass dynamic values as resource or environment attributes.',
    );
  }

  return new AbilityDSLParser<R, E, T>(strings[0]).parse();
}
