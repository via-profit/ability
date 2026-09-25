/**
 * Prism grammar of the Ability DSL for the code blocks of the documentation
 */
type PrismLike = { languages: Record<string, unknown> };

function dsl(Prism: PrismLike) {
  Prism.languages.dsl = {
    comment: { pattern: /#.*/, greedy: true },
    annotation: {
      pattern: /@\w+.*/,
      greedy: true,
      inside: {
        atrule: /^@\w+/,
        string: /.+/,
      },
    },
    string: { pattern: /(["'])(?:\\.|(?!\1)[^\\\r\n])*\1/, greedy: true },
    'class-name': /\bpermission\.[\w.*]+/,
    keyword: /\b(?:permit|allow|deny|forbidden|if|all|any|of|except|alias)\b/,
    boolean: /\b(?:true|false|null)\b/,
    operator:
      /\b(?:is|not|equals|equal|in|contains|includes|has|greater|less|than|or|length|len|gt|gte|lt|lte|defined|empty|starts|ends|with|always|never)\b|[<>!=]=?|<>/,
    number: /-?\b\d+(?:\.\d+)?\b/,
    builtin: /\benv(?=\.)/,
    variable: /\b[a-zA-Z_][\w]*(?:\.[\w*]+)+/,
    punctuation: /[:,[\]]/,
  };
}

dsl.displayName = 'dsl';
dsl.aliases = ['ability'] as string[];

export default dsl;
