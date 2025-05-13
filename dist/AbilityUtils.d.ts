import AbilityCode from '~/AbilityCode';
export default class AbilityUtils {
    createCodeFromLiteral<Literal extends string>(literal: Literal): AbilityCode<Literal>;
}
