import React from 'react';
import styled from '@emotion/styled';
import { useTheme } from '@emotion/react';
import { useNavigate } from 'react-router-dom';
import PrismLight from 'react-syntax-highlighter/dist/esm/prism-light';
import tsx from 'react-syntax-highlighter/dist/esm/languages/prism/tsx';
import typescript from 'react-syntax-highlighter/dist/esm/languages/prism/typescript';
import javascript from 'react-syntax-highlighter/dist/esm/languages/prism/javascript';
import json from 'react-syntax-highlighter/dist/esm/languages/prism/json';
import bash from 'react-syntax-highlighter/dist/esm/languages/prism/bash';

import { CheckIcon, CopyIcon, PlayIcon } from '~/components/Icons';
import { useT } from '~/translations';
import { playgroundLink } from '~/playground/share';
import dsl from './prismDsl';
import { prismStyle } from './codeStyles';

PrismLight.registerLanguage('tsx', tsx);
PrismLight.registerLanguage('typescript', typescript);
PrismLight.registerLanguage('javascript', javascript);
PrismLight.registerLanguage('json', json);
PrismLight.registerLanguage('bash', bash);
PrismLight.registerLanguage('dsl', dsl);

const languageAliases: Record<string, string> = {
  ts: 'typescript',
  js: 'javascript',
  jsx: 'tsx',
  sh: 'bash',
  shell: 'bash',
  ability: 'dsl',
};

const knownLanguages = ['tsx', 'typescript', 'javascript', 'json', 'bash', 'dsl'];

export interface SyntaxHighlighterProps {
  readonly language: string;
  readonly code: string;
}

const Frame = styled.div`
  margin: 1em 0 1.5em;
  overflow: hidden;
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.6rem;
  background-color: ${({ theme }) => theme.color.codeBackground.toString()};
`;

const Toolbar = styled.div`
  display: flex;
  align-items: center;
  gap: 0.25rem;
  height: 2.25rem;
  padding: 0 0.5rem 0 1rem;
  font-family: var(--font-mono);
  font-size: 0.75rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  border-bottom: 1px solid ${({ theme }) => theme.color.border.toString()};
  background-color: ${({ theme }) => theme.color.backgroundSecondary.alpha(0.35).toString()};
`;

const Language = styled.span`
  margin-right: auto;
`;

const ToolbarButton = styled.button`
  display: inline-flex;
  align-items: center;
  gap: 0.35rem;
  height: 1.6rem;
  padding: 0 0.5rem;
  font: inherit;
  color: inherit;
  background: transparent;
  border: 1px solid transparent;
  border-radius: 0.35rem;
  cursor: pointer;

  &:hover {
    color: ${({ theme }) => theme.color.accentPrimary.toString()};
    border-color: ${({ theme }) => theme.color.border.toString()};
  }
`;

const Wrapper = styled.div`
  display: grid;
  overflow-x: auto;
`;

const SyntaxHighlighter: React.FC<SyntaxHighlighterProps> = props => {
  const { code, language: rawLanguage } = props;
  const theme = useTheme();
  const t = useT();
  const navigate = useNavigate();
  const [copied, setCopied] = React.useState(false);
  const style = React.useMemo(() => prismStyle(theme), [theme]);

  const language = languageAliases[rawLanguage] ?? rawLanguage;
  const codeStr = String(code).replace(/^\n/, '').replace(/\n$/, '');

  React.useEffect(() => {
    if (!copied) {
      return undefined;
    }

    const timeout = setTimeout(() => setCopied(false), 1500);

    return () => clearTimeout(timeout);
  }, [copied]);

  const handleCopy = React.useCallback(() => {
    navigator.clipboard
      ?.writeText(codeStr)
      .then(() => setCopied(true))
      .catch(() => undefined);
  }, [codeStr]);

  // DSL examples with at least one policy can be opened in the playground
  const isPolicy = language === 'dsl' && /\b(permit|allow|deny|forbidden)\s+permission\./.test(codeStr);

  return (
    <Frame>
      <Toolbar>
        <Language>{rawLanguage}</Language>
        {isPolicy && (
          <ToolbarButton type="button" onClick={() => navigate(playgroundLink({ dsl: codeStr }))}>
            <PlayIcon />
            {t('code.openInPlayground')}
          </ToolbarButton>
        )}
        <ToolbarButton type="button" onClick={handleCopy}>
          {copied ? <CheckIcon /> : <CopyIcon />}
          {copied ? t('code.copied') : t('code.copy')}
        </ToolbarButton>
      </Toolbar>
      <Wrapper>
        <PrismLight
          language={knownLanguages.includes(language) ? language : 'text'}
          style={style}
          PreTag="pre"
        >
          {codeStr}
        </PrismLight>
      </Wrapper>
    </Frame>
  );
};

export default SyntaxHighlighter;
