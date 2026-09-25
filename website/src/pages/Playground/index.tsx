import React from 'react';
import styled from '@emotion/styled';
import { css } from '@emotion/react';
import { useLocation, useNavigate } from 'react-router-dom';

import Breadcrumbs from '~/components/Breadcrumbs';
import Button from '~/components/Button';
import Select from '~/components/Select';
import TextField from '~/components/TextField';
import SyntaxHighlighter from '~/components/SyntaxHighlighter';
import LoadingIndicator from '~/components/LoadingIndicator';
import { CheckIcon, ResetIcon, ShareIcon } from '~/components/Icons';
import { useUi } from '~/providers/UiProvider';
import { useT } from '~/translations';
import { evaluate, strategyNames, StrategyName } from '~/playground/evaluate';
import { defaultExample, examples } from '~/playground/examples';
import ExplainTree from '~/playground/ExplainTree';
import {
  clearStoredState,
  PlaygroundState,
  readHashState,
  readStoredState,
  shareUrl,
  writeStoredState,
} from '~/playground/share';

const CodeEditor = React.lazy(() => import(/* webpackChunkName: "editor" */ '~/playground/CodeEditor'));

type OutputTab = 'explain' | 'types' | 'json';

const Page = styled.div`
  display: flex;
  flex-direction: column;
  gap: 1rem;
  width: 100%;
  padding: 1.25rem 1.5rem 2rem;

  @media all and (max-width: 640px) {
    padding: 1rem 0.75rem 1.5rem;
  }
`;

const Toolbar = styled.div`
  display: grid;
  grid-template-columns: minmax(12rem, 16rem) minmax(12rem, 16rem) minmax(12rem, 18rem) 1fr;
  align-items: end;
  gap: 0.75rem;

  @media all and (max-width: 1100px) {
    grid-template-columns: repeat(auto-fill, minmax(14rem, 1fr));
  }
`;

const ToolbarActions = styled.div`
  display: flex;
  justify-content: flex-end;
  gap: 0.5rem;
`;

const Workspace = styled.div`
  display: grid;
  grid-template-columns: minmax(0, 1.15fr) minmax(0, 0.85fr) minmax(0, 1.2fr);
  gap: 0.75rem;
  height: max(34rem, calc(100vh - var(--header-height) - 13rem));

  @media all and (max-width: 1100px) {
    grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
    height: auto;

    & > :last-child {
      grid-column: 1 / -1;
    }
  }

  @media all and (max-width: 720px) {
    grid-template-columns: minmax(0, 1fr);
  }
`;

const Panel = styled.section<{ $isActive?: boolean }>`
  display: flex;
  flex-direction: column;
  min-height: 0;
  overflow: hidden;
  background-color: ${({ theme }) => theme.color.codeBackground.toString()};
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.75rem;
  transition:
    border-color 160ms ease-out,
    box-shadow 160ms ease-out;

  &:focus-within {
    border-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.6).toString()};
    box-shadow:
      0 0 0 1px ${({ theme }) => theme.color.accentPrimary.alpha(0.25).toString()},
      0 0 1.5rem -0.5rem ${({ theme }) => theme.color.accentPrimary.alpha(0.5).toString()};
  }

  @media all and (max-width: 1100px) {
    height: 26rem;
  }
`;

const PanelHeader = styled.header`
  display: flex;
  align-items: center;
  gap: 0.6rem;
  min-height: 2.5rem;
  padding: 0 0.9rem;
  font-size: 0.8rem;
  border-bottom: 1px solid ${({ theme }) => theme.color.border.toString()};
  background-color: ${({ theme }) => theme.color.backgroundSecondary.alpha(0.35).toString()};
`;

const PanelTitle = styled.h2`
  margin: 0;
  font-size: 0.8rem;
  font-weight: 600;
`;

const FileName = styled.span`
  font-family: var(--font-mono);
  font-size: 0.72rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const PanelMeta = styled.span`
  margin-left: auto;
  font-size: 0.72rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
`;

const Hint = styled.div`
  padding: 0.45rem 0.9rem;
  font-size: 0.72rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  border-bottom: 1px solid ${({ theme }) => theme.color.border.toString()};
`;

const Tabs = styled.div`
  display: flex;
  align-self: stretch;
  gap: 1rem;
  margin-left: auto;
`;

const Tab = styled.button<{ $isActive: boolean }>`
  position: relative;
  padding: 0;
  font: inherit;
  font-size: 0.78rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  background: none;
  border: none;
  cursor: pointer;

  &:hover {
    color: ${({ theme }) => theme.color.textPrimary.toString()};
  }

  ${({ $isActive, theme }) =>
    $isActive &&
    css`
      color: ${theme.color.textPrimary.toString()};

      &::after {
        content: '';
        position: absolute;
        left: 0;
        right: 0;
        bottom: -1px;
        height: 2px;
        border-radius: 2px;
        background: ${theme.color.accentPrimary.toString()};
        box-shadow: 0 0 8px ${theme.color.accentPrimary.alpha(0.6).toString()};
      }
    `}
`;

const OutputBody = styled.div`
  flex: 1;
  min-height: 0;
  overflow: auto;
  padding: 0.9rem;

  & > div:only-child {
    margin: 0;
    border: none;
  }
`;

const Verdict = styled.div<{ $isAllowed: boolean }>`
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
  margin: 0.9rem 0.9rem 0;
  padding: 0.75rem 0.9rem;
  border-radius: 0.6rem;
  ${({ theme, $isAllowed }) => {
    const color = $isAllowed ? theme.color.success : theme.color.error;

    return css`
      background-color: ${color.alpha(0.08).toString()};
      border: 1px solid ${color.alpha(0.35).toString()};
    `;
  }}
`;

const VerdictTitle = styled.div<{ $isAllowed: boolean }>`
  font-size: 1rem;
  font-weight: 700;
  color: ${({ theme, $isAllowed }) =>
    ($isAllowed ? theme.color.success : theme.color.error).toString()};
`;

const VerdictLine = styled.div`
  font-size: 0.8rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};

  & strong {
    font-weight: 500;
    color: ${({ theme }) => theme.color.textPrimary.toString()};
  }
`;

const ErrorBox = styled.div`
  margin: 0.9rem 0.9rem 0;
  padding: 0.75rem 0.9rem;
  font-size: 0.8rem;
  border-radius: 0.6rem;
  color: ${({ theme }) => theme.color.error.toString()};
  background-color: ${({ theme }) => theme.color.error.alpha(0.08).toString()};
  border: 1px solid ${({ theme }) => theme.color.error.alpha(0.35).toString()};
`;

const ErrorTitle = styled.div`
  margin-bottom: 0.25rem;
  font-weight: 700;
`;

const ErrorText = styled.pre`
  margin: 0;
  font-family: var(--font-mono);
  font-size: 0.76rem;
  white-space: pre-wrap;
  color: ${({ theme }) => theme.color.textPrimary.toString()};
`;

const Muted = styled.div`
  font-size: 0.85rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const fromExample = (example: typeof defaultExample): PlaygroundState => ({
  dsl: example.dsl,
  context: example.context,
  strategy: example.strategy,
  permission: example.permission,
});

/**
 * Initial state: a shared link, then the last state of the user, then the default example
 */
const initialState = (hash: string): PlaygroundState => {
  const base = fromExample(defaultExample);
  const shared = readHashState(hash);
  if (shared) {
    return {
      dsl: typeof shared.dsl === 'string' ? shared.dsl : base.dsl,
      context:
        typeof shared.context === 'string'
          ? shared.context
          : JSON.stringify({ resource: {}, environment: {} }, null, 2),
      strategy: strategyNames.includes(shared.strategy as StrategyName)
        ? (shared.strategy as StrategyName)
        : base.strategy,
      permission: typeof shared.permission === 'string' ? shared.permission : '',
    };
  }

  const stored = readStoredState();
  if (stored && typeof stored.dsl === 'string' && typeof stored.context === 'string') {
    return {
      dsl: stored.dsl,
      context: stored.context,
      strategy: strategyNames.includes(stored.strategy as StrategyName)
        ? (stored.strategy as StrategyName)
        : base.strategy,
      permission: typeof stored.permission === 'string' ? stored.permission : '',
    };
  }

  return base;
};

const Playground: React.FC = () => {
  const t = useT();
  const { locale } = useUi();
  const { hash } = useLocation();
  const navigate = useNavigate();
  const [state, setState] = React.useState<PlaygroundState>(() => initialState(hash));
  const [debounced, setDebounced] = React.useState(state);
  const [exampleId, setExampleId] = React.useState<string | null>(() =>
    readHashState(hash) || readStoredState() ? null : defaultExample.id,
  );
  const [tab, setTab] = React.useState<OutputTab>('explain');
  const [isShared, setShared] = React.useState(false);

  React.useEffect(() => {
    document.title = `${t('playground.title')} · @via-profit/ability`;
  }, [t]);

  // The state from the link is applied once, then the link is cleaned
  React.useEffect(() => {
    if (hash) {
      navigate('/playground', { replace: true });
    }
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  React.useEffect(() => {
    const timeout = setTimeout(() => setDebounced(state), 250);
    writeStoredState(state);

    return () => clearTimeout(timeout);
  }, [state]);

  React.useEffect(() => {
    if (!isShared) {
      return undefined;
    }
    const timeout = setTimeout(() => setShared(false), 2000);

    return () => clearTimeout(timeout);
  }, [isShared]);

  const firstPass = React.useMemo(
    () => evaluate(debounced.dsl, debounced.context, debounced.strategy, debounced.permission),
    [debounced],
  );

  // Without a chosen key the first key of the policies is checked
  const permission = debounced.permission.trim() || firstPass.permissions[0] || '';
  const result = React.useMemo(
    () =>
      permission === debounced.permission
        ? firstPass
        : evaluate(debounced.dsl, debounced.context, debounced.strategy, permission),
    [firstPass, permission, debounced],
  );

  const update = (patch: Partial<PlaygroundState>) => {
    setState(prev => ({ ...prev, ...patch }));
  };

  const selectExample = (id: string) => {
    const example = examples.find(item => item.id === id);
    if (example) {
      setExampleId(id);
      setState(fromExample(example));
    }
  };

  const reset = () => {
    clearStoredState();
    setExampleId(defaultExample.id);
    setState(fromExample(defaultExample));
  };

  const share = () => {
    navigator.clipboard
      ?.writeText(shareUrl({ ...state, permission }))
      .then(() => setShared(true))
      .catch(() => undefined);
  };

  const { check } = result;

  return (
    <Page>
      <Breadcrumbs items={[{ label: t('playground.title') }]} />

      <Toolbar>
        <Select
          label={t('playground.example')}
          placeholder={t('playground.customExample')}
          value={exampleId}
          options={examples.map(example => ({ value: example.id, label: example.title[locale] }))}
          onChange={selectExample}
        />
        <Select
          label={t('playground.strategy')}
          value={state.strategy}
          options={strategyNames.map(name => ({ value: name, label: name }))}
          onChange={strategy => update({ strategy })}
        />
        <TextField
          fullWidth
          label={t('playground.permission')}
          value={state.permission}
          placeholder={permission}
          list="playground-permissions"
          spellCheck={false}
          onChange={event => update({ permission: event.currentTarget.value })}
        />
        <datalist id="playground-permissions">
          {result.permissions.map(key => (
            <option key={key} value={key} />
          ))}
        </datalist>
        <ToolbarActions>
          <Button
            type="button"
            variant="outlined"
            onClick={share}
            startIcon={isShared ? <CheckIcon /> : <ShareIcon />}
          >
            {isShared ? t('playground.shared') : t('playground.share')}
          </Button>
          <Button type="button" variant="outlined" onClick={reset} startIcon={<ResetIcon />}>
            {t('playground.reset')}
          </Button>
        </ToolbarActions>
      </Toolbar>

      <Workspace>
        <Panel>
          <PanelHeader>
            <PanelTitle>{t('playground.policies')}</PanelTitle>
            <FileName>policies.dsl</FileName>
            <PanelMeta>{t('playground.policiesCount', { count: result.policies.length })}</PanelMeta>
          </PanelHeader>
          <React.Suspense fallback={<LoadingIndicator />}>
            <CodeEditor
              language="dsl"
              value={state.dsl}
              error={result.dslError}
              ariaLabel={t('playground.policies')}
              onChange={dsl => {
                setExampleId(null);
                update({ dsl });
              }}
            />
          </React.Suspense>
        </Panel>

        <Panel>
          <PanelHeader>
            <PanelTitle>{t('playground.context')}</PanelTitle>
            <FileName>context.json</FileName>
          </PanelHeader>
          <Hint>{t('playground.contextHint')}</Hint>
          <React.Suspense fallback={<LoadingIndicator />}>
            <CodeEditor
              language="json"
              value={state.context}
              error={result.contextError}
              ariaLabel={t('playground.context')}
              onChange={context => {
                setExampleId(null);
                update({ context });
              }}
            />
          </React.Suspense>
        </Panel>

        <Panel aria-live="polite">
          <PanelHeader>
            <PanelTitle>{t('playground.output')}</PanelTitle>
            <Tabs role="tablist">
              {(['explain', 'types', 'json'] as const).map(name => (
                <Tab
                  key={name}
                  type="button"
                  role="tab"
                  aria-selected={tab === name}
                  $isActive={tab === name}
                  onClick={() => setTab(name)}
                >
                  {t(`playground.tab.${name}`)}
                </Tab>
              ))}
            </Tabs>
          </PanelHeader>

          {result.dslError && (
            <ErrorBox>
              <ErrorTitle>
                {t('playground.dslError')} · {result.dslError.line}:{result.dslError.column}
              </ErrorTitle>
              <ErrorText>{result.dslError.message}</ErrorText>
            </ErrorBox>
          )}

          {result.contextError && (
            <ErrorBox>
              <ErrorTitle>{t('playground.jsonError')}</ErrorTitle>
              <ErrorText>{result.contextError.message}</ErrorText>
            </ErrorBox>
          )}

          {check && tab === 'explain' && (
            <Verdict $isAllowed={check.isAllowed}>
              <VerdictTitle $isAllowed={check.isAllowed}>
                {check.isAllowed ? t('playground.permit') : t('playground.deny')}
              </VerdictTitle>
              <VerdictLine>
                <code>{permission}</code> · {state.strategy} ·{' '}
                {t('playground.duration', {
                  time:
                    check.duration < 1
                      ? `${(check.duration * 1000).toFixed(1)} µs`
                      : `${check.duration.toFixed(2)} ms`,
                })}
              </VerdictLine>
              <VerdictLine>
                {t('playground.decisive')}:{' '}
                {check.decisive ? <strong>{check.decisive}</strong> : t('playground.noDecisive')}
              </VerdictLine>
            </Verdict>
          )}

          <OutputBody>
            {tab === 'explain' &&
              check &&
              (check.policiesForKey > 0 ? (
                <ExplainTree policies={check.explain.policies} />
              ) : (
                <Muted>{t('playground.noPolicies')}</Muted>
              ))}
            {tab === 'types' && result.typeDefs && (
              <SyntaxHighlighter language="ts" code={result.typeDefs} />
            )}
            {tab === 'json' && result.json && <SyntaxHighlighter language="json" code={result.json} />}
          </OutputBody>
        </Panel>
      </Workspace>
    </Page>
  );
};

export default Playground;
