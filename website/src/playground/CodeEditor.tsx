import React from 'react';
import styled from '@emotion/styled';
import { Theme, useTheme } from '@emotion/react';
import { basicSetup } from 'codemirror';
import { EditorView, keymap } from '@codemirror/view';
import { Compartment, EditorState, Extension } from '@codemirror/state';
import { indentWithTab } from '@codemirror/commands';
import { HighlightStyle, syntaxHighlighting } from '@codemirror/language';
import { json } from '@codemirror/lang-json';
import { setDiagnostics } from '@codemirror/lint';
import { tags } from '@lezer/highlight';

import { tokenColors } from '~/components/SyntaxHighlighter/codeStyles';
import { dslLanguage } from './dslLanguage';
import type { SourceError } from './evaluate';

export interface CodeEditorProps {
  readonly value: string;
  readonly language: 'dsl' | 'json';
  readonly error: SourceError | null;
  readonly onChange: (value: string) => void;
  readonly ariaLabel: string;
}

const Container = styled.div`
  flex: 1;
  min-height: 0;
  overflow: hidden;

  & .cm-editor {
    height: 100%;
  }
`;

const editorTheme = (theme: Theme): Extension => {
  const c = tokenColors(theme);

  return [
    EditorView.theme(
      {
        '&': {
          color: theme.color.textPrimary.toString(),
          backgroundColor: theme.color.codeBackground.toString(),
          fontSize: '0.85rem',
        },
        '&.cm-focused': { outline: 'none' },
        '.cm-scroller': { fontFamily: 'var(--font-mono)', lineHeight: '1.65' },
        '.cm-content': { caretColor: theme.color.accentPrimary.toString(), padding: '0.75rem 0' },
        '.cm-cursor, .cm-dropCursor': { borderLeftColor: theme.color.accentPrimary.toString() },
        '&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection':
          { backgroundColor: theme.color.accentPrimary.alpha(0.22).toString() },
        '.cm-gutters': {
          color: theme.color.textSecondary.alpha(0.7).toString(),
          backgroundColor: theme.color.codeBackground.toString(),
          border: 'none',
        },
        '.cm-activeLine': { backgroundColor: theme.color.accentPrimary.alpha(0.05).toString() },
        '.cm-activeLineGutter': {
          color: theme.color.accentPrimary.toString(),
          backgroundColor: 'transparent',
        },
        '.cm-matchingBracket': {
          backgroundColor: theme.color.accentPrimary.alpha(0.2).toString(),
          outline: 'none',
        },
        '.cm-foldPlaceholder': {
          color: theme.color.textSecondary.toString(),
          backgroundColor: theme.color.backgroundSecondary.toString(),
          border: 'none',
        },
        '.cm-tooltip': {
          color: theme.color.textPrimary.toString(),
          backgroundColor: theme.color.surface.toString(),
          border: `1px solid ${theme.color.border.toString()}`,
          borderRadius: '0.4rem',
        },
        '.cm-diagnostic-error': { borderLeftColor: theme.color.error.toString() },
        '.cm-lintRange-error': {
          backgroundImage: 'none',
          textDecoration: `wavy underline ${theme.color.error.toString()}`,
          textUnderlineOffset: '3px',
        },
        '.cm-panels': {
          color: theme.color.textPrimary.toString(),
          backgroundColor: theme.color.surface.toString(),
        },
      },
      { dark: theme.isDark },
    ),
    syntaxHighlighting(
      HighlightStyle.define([
        { tag: tags.comment, color: c.comment, fontStyle: 'italic' },
        { tag: tags.keyword, color: c.keyword },
        { tag: tags.operator, color: c.operator },
        { tag: tags.string, color: c.string },
        { tag: tags.number, color: c.number },
        { tag: [tags.atom, tags.bool, tags.null], color: c.number },
        { tag: tags.meta, color: c.annotation },
        { tag: tags.typeName, color: c.className },
        { tag: tags.className, color: c.className },
        { tag: tags.propertyName, color: c.property },
        { tag: tags.variableName, color: c.variable },
        { tag: tags.punctuation, color: c.punctuation },
      ]),
    ),
  ];
};

const CodeEditor: React.FC<CodeEditorProps> = props => {
  const { value, language, error, onChange, ariaLabel } = props;
  const theme = useTheme();
  const containerRef = React.useRef<HTMLDivElement>(null);
  const viewRef = React.useRef<EditorView | null>(null);
  const themeCompartment = React.useRef(new Compartment());
  const onChangeRef = React.useRef(onChange);
  onChangeRef.current = onChange;

  // Create the editor once
  React.useEffect(() => {
    if (!containerRef.current) {
      return undefined;
    }

    const view = new EditorView({
      parent: containerRef.current,
      state: EditorState.create({
        doc: value,
        extensions: [
          basicSetup,
          keymap.of([indentWithTab]),
          language === 'dsl' ? dslLanguage : json(),
          themeCompartment.current.of(editorTheme(theme)),
          EditorView.contentAttributes.of({ 'aria-label': ariaLabel }),
          EditorView.updateListener.of(update => {
            if (update.docChanged) {
              onChangeRef.current(update.state.doc.toString());
            }
          }),
        ],
      }),
    });
    viewRef.current = view;

    return () => {
      view.destroy();
      viewRef.current = null;
    };
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  React.useEffect(() => {
    viewRef.current?.dispatch({
      effects: themeCompartment.current.reconfigure(editorTheme(theme)),
    });
  }, [theme]);

  // The value is replaced from outside (example selection, reset)
  React.useEffect(() => {
    const view = viewRef.current;
    if (view && view.state.doc.toString() !== value) {
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: value } });
    }
  }, [value]);

  // Error underline and message in the editor
  React.useEffect(() => {
    const view = viewRef.current;
    if (!view) {
      return;
    }

    const { doc } = view.state;
    const diagnostics = [];

    if (error) {
      const line = doc.line(Math.min(Math.max(error.line, 1), doc.lines));
      const from = Math.min(line.from + Math.max(error.column - 1, 0), line.to);
      const to = Math.min(from + Math.max(error.length, 1), line.to);
      diagnostics.push({
        from,
        to: to > from ? to : Math.min(from + 1, doc.length),
        severity: 'error' as const,
        message: error.message,
      });
    }

    view.dispatch(setDiagnostics(view.state, diagnostics));
  }, [error, value]);

  return <Container ref={containerRef} />;
};

export default CodeEditor;
