import { StreamLanguage } from '@codemirror/language';

interface DslState {
  /**
   * The rest of the line after the annotation name is its value
   */
  annotationValue: boolean;
}

const KEYWORDS = /^(?:permit|allow|deny|forbidden|if|all|any|of|except|alias)\b/;
const OPERATORS =
  /^(?:is|not|equals|equal|in|contains|includes|has|greater|less|than|or|length|len|gt|gte|lt|lte|defined|empty|starts|ends|with|always|never)\b/;

/**
 * Ability DSL for the CodeMirror editor
 */
export const dslLanguage = StreamLanguage.define<DslState>({
  name: 'ability-dsl',
  startState: () => ({ annotationValue: false }),
  token(stream, state) {
    if (stream.sol()) {
      state.annotationValue = false;
    }

    if (stream.eatSpace()) {
      return null;
    }

    if (state.annotationValue) {
      stream.skipToEnd();

      return 'string';
    }

    if (stream.match(/^#.*/)) {
      return 'comment';
    }

    if (stream.match(/^@\w+/)) {
      state.annotationValue = true;

      return 'meta';
    }

    if (stream.match(/^(["'])(?:\\.|(?!\1).)*\1?/)) {
      return 'string';
    }

    if (stream.match(/^permission\.[\w.*]+/)) {
      return 'typeName';
    }

    if (stream.match(/^-?\d+(?:\.\d+)?/)) {
      return 'number';
    }

    if (stream.match(KEYWORDS)) {
      return 'keyword';
    }

    if (stream.match(/^(?:true|false|null)\b/)) {
      return 'atom';
    }

    if (stream.match(OPERATORS) || stream.match(/^(?:<>|[<>!=]=?)/)) {
      return 'operator';
    }

    if (stream.match(/^env(?=\.)/)) {
      return 'className';
    }

    if (stream.match(/^[a-zA-Z_]\w*(?:\.[\w*]+)*/)) {
      return 'variableName';
    }

    if (stream.match(/^[:,[\]]/)) {
      return 'punctuation';
    }

    stream.next();

    return null;
  },
  languageData: {
    commentTokens: { line: '#' },
  },
});
