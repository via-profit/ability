import React from 'react';
import styled from '@emotion/styled';
import { Link, useLocation } from 'react-router-dom';

import Breadcrumbs from '~/components/Breadcrumbs';
import DocsArticle from '~/components/DocsArticle';
import LoadingIndicator from '~/components/LoadingIndicator';
import RenderMarkdown from '~/components/RenderMarkdown';
import TableOfContent from '~/components/TableOfContent';
import { ChevronLeftIcon, ChevronRightIcon, EditIcon } from '~/components/Icons';
import { useDocsLayout } from '~/templates/TemplateDocs/context';
import { useUi } from '~/providers/UiProvider';
import { useT } from '~/translations';
import { docEditUrl, docs, findDocByPath } from '~/utils/docs';
import { extractHeadings, removeTableOfContents } from '~/utils/markdown';
import NotFound from '~/pages/NotFound';
import { useDocContent } from './useDocContent';

const Content = styled.div`
  display: flex;
  align-items: flex-start;
  gap: 3rem;
`;

const Notice = styled.div`
  margin-bottom: 1.25rem;
  padding: 0.6rem 0.9rem;
  font-size: 0.85rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  border: 1px dashed ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.5rem;
`;

const Error = styled(Notice)`
  color: ${({ theme }) => theme.color.error.toString()};
  border-color: ${({ theme }) => theme.color.error.alpha(0.5).toString()};
`;

const PageFooter = styled.div`
  margin-top: 3rem;
  padding-top: 1.5rem;
  border-top: 1px solid ${({ theme }) => theme.color.border.toString()};
`;

const EditLink = styled.a`
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.85rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  text-decoration: none;

  &:hover {
    color: ${({ theme }) => theme.color.accentPrimary.toString()};
  }
`;

const Pager = styled.nav`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.75rem;
  margin-top: 1.25rem;
`;

const PagerLink = styled(Link)<{ $next?: boolean }>`
  display: flex;
  flex-direction: column;
  gap: 0.15rem;
  grid-column: ${({ $next }) => ($next ? 2 : 1)};
  padding: 0.85rem 1rem;
  text-align: ${({ $next }) => ($next ? 'right' : 'left')};
  text-decoration: none;
  color: ${({ theme }) => theme.color.textPrimary.toString()};
  background-color: ${({ theme }) => theme.color.surface.toString()};
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.6rem;
  transition: border-color 120ms ease-out;

  &:hover {
    border-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.6).toString()};
  }
`;

const PagerLabel = styled.span`
  display: inline-flex;
  align-items: center;
  gap: 0.25rem;
  font-size: 0.75rem;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const PagerTitle = styled.span`
  font-weight: 500;
  color: ${({ theme }) => theme.color.accentPrimary.toString()};
`;

const DocPage: React.FC = () => {
  const { pathname } = useLocation();
  const { locale } = useUi();
  const t = useT();
  const { setHeadings } = useDocsLayout();
  const doc = findDocByPath(pathname);
  const { content, isLoading, error } = useDocContent(doc, locale);

  const markdown = React.useMemo(() => (content ? removeTableOfContents(content) : ''), [content]);
  const headings = React.useMemo(() => extractHeadings(markdown), [markdown]);

  React.useEffect(() => {
    setHeadings(headings);

    return () => setHeadings([]);
  }, [headings, setHeadings]);

  React.useEffect(() => {
    if (doc) {
      document.title = `${t(doc.title)} · @via-profit/ability`;
    }
  }, [doc, t]);

  if (!doc) {
    return <NotFound />;
  }

  const index = docs.indexOf(doc);
  const prev = docs[index - 1];
  const next = docs[index + 1];

  return (
    <>
      <Breadcrumbs
        items={[
          { label: t('breadcrumbs.docs'), to: '/docs' },
          ...(doc.id === 'getting-started' ? [] : [{ label: t(doc.title) }]),
        ]}
      />
      <Content>
        <DocsArticle>
          {doc.id === 'changelog' && locale !== 'ru' && <Notice>{t('doc.changelogLanguage')}</Notice>}
          {error && <Error>{t('doc.loadError')}</Error>}
          {isLoading && !content && <LoadingIndicator />}
          {content && <RenderMarkdown>{markdown}</RenderMarkdown>}

          {content && (
            <PageFooter>
              <EditLink href={docEditUrl(doc.id, locale)} target="_blank" rel="noopener noreferrer">
                <EditIcon />
                {t('doc.edit')}
              </EditLink>
              <Pager>
                {prev && (
                  <PagerLink to={prev.path}>
                    <PagerLabel>
                      <ChevronLeftIcon />
                      {t('doc.prev')}
                    </PagerLabel>
                    <PagerTitle>{t(prev.title)}</PagerTitle>
                  </PagerLink>
                )}
                {next && (
                  <PagerLink to={next.path} $next>
                    <PagerLabel css={{ justifyContent: 'flex-end' }}>
                      {t('doc.next')}
                      <ChevronRightIcon />
                    </PagerLabel>
                    <PagerTitle>{t(next.title)}</PagerTitle>
                  </PagerLink>
                )}
              </Pager>
            </PageFooter>
          )}
        </DocsArticle>
        <TableOfContent headings={headings} />
      </Content>
    </>
  );
};

export default DocPage;
