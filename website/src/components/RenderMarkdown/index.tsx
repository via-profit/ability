import React from 'react';
import { Link, useLocation } from 'react-router-dom';
import styled from '@emotion/styled';
import { css, Theme } from '@emotion/react';
import Markdown from 'markdown-to-jsx/react';
import H1 from '@via-profit/ui-kit/Typography/H1';
import H2 from '@via-profit/ui-kit/Typography/H2';
import H3 from '@via-profit/ui-kit/Typography/H3';
import H4 from '@via-profit/ui-kit/Typography/H4';
import H5 from '@via-profit/ui-kit/Typography/H5';
import Strong from '@via-profit/ui-kit/Typography/Strong';
import Em from '@via-profit/ui-kit/Typography/Em';
import Paragraph from '@via-profit/ui-kit/Typography/Paragraph';
import { Ul, Ol } from '@via-profit/ui-kit/Typography/List';
import Blockquote from '@via-profit/ui-kit/Typography/Blockquote';
import {
  Table,
  TableHeader,
  TableRow,
  TableCell,
  TableHeaderCell,
  TableCaption,
  TableBody,
} from '@via-profit/ui-kit/Table';

import { OpenIcon } from '~/components/Icons';
import SyntaxHighlighter from '~/components/SyntaxHighlighter';
import Mermaid from '~/components/Mermaid';
import { useT, MessageId } from '~/translations';
import { resolveMarkdownLink } from '~/utils/docs';
import slugify from '~/utils/slugify';
import scrollToAnchor, { setLocationHash } from '~/utils/scrollToAnchor';

interface Props {
  readonly children: string;
}

const linkStyles = (theme: Theme) => css`
  font-weight: 500;
  color: ${theme.color.accentPrimary.toString()};
  text-decoration: none;
  border-bottom: 1px solid ${theme.color.accentPrimary.alpha(0.3).toString()};
  transition: border-color 120ms ease-out;

  &:hover {
    border-bottom-color: ${theme.color.accentPrimary.toString()};
  }
`;

const InternalLink = styled(Link)`
  ${({ theme }) => linkStyles(theme)};
`;

const ExternalLink = styled.a`
  ${({ theme }) => linkStyles(theme)};
`;

const ExternalLinkIcon = styled(OpenIcon)`
  font-size: 0.85em;
  margin-left: 0.15em;
  margin-top: -0.11em;
  vertical-align: middle;
`;

const Img = styled.img`
  max-width: 100%;
  vertical-align: middle;
  border-radius: 0.25rem;
`;

const MarkdownStrong = styled(Strong)`
  font-weight: 600;
  color: ${({ theme }) => theme.color.textPrimary.toString()};
`;

const MarkdownEm = styled(Em)`
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const MarkdownParagraph = styled(Paragraph)`
  margin: 0 0 1em;
  font-weight: 400;
  line-height: 1.75;
  color: ${({ theme }) => theme.color.textPrimary.alpha(0.88).toString()};
`;

const listStyles = (theme: Theme) => css`
  margin: 0 0 1em;
  padding-left: 1.4em;
  font-weight: 400;
  line-height: 1.75;
  color: ${theme.color.textPrimary.alpha(0.88).toString()};

  & li::marker {
    color: ${theme.color.accentPrimary.toString()};
  }

  & li > ul,
  & li > ol {
    margin-bottom: 0;
  }
`;

const MarkdownUl = styled(Ul)`
  ${({ theme }) => listStyles(theme)};
`;

const MarkdownOl = styled(Ol)`
  ${({ theme }) => listStyles(theme)};
`;

const CodeInline = styled.code`
  font-family: var(--font-mono);
  font-size: 0.85em;
  color: ${({ theme }) => theme.color.accentPrimary.toString()};
  background-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.08).toString()};
  border: 1px solid ${({ theme }) => theme.color.accentPrimary.alpha(0.15).toString()};
  padding: 0.1em 0.4em;
  border-radius: 0.3rem;
  overflow-wrap: anywhere;
`;

const headingAnchor = css`
  scroll-margin-top: calc(var(--header-height) + 1rem);
`;

const MarkdownH1 = styled(H1)`
  ${headingAnchor};
  margin: 0 0 0.75em;
  font-size: 2.1rem;
  font-weight: 700;
  line-height: 1.2;
  letter-spacing: -0.02em;
`;

const MarkdownH2 = styled(H2)`
  ${headingAnchor};
  margin: 2.2em 0 0.8em;
  padding-top: 1.2em;
  font-size: 1.45rem;
  font-weight: 600;
  letter-spacing: -0.01em;
  border-top: 1px solid ${({ theme }) => theme.color.border.toString()};
`;

const MarkdownH3 = styled(H3)`
  ${headingAnchor};
  margin: 1.8em 0 0.6em;
  font-size: 1.15rem;
  font-weight: 600;
`;

const MarkdownH4 = styled(H4)`
  ${headingAnchor};
  margin: 1.5em 0 0.5em;
  font-size: 1rem;
  font-weight: 600;
`;

const MarkdownH5 = styled(H5)`
  ${headingAnchor};
  margin: 1.2em 0 0.5em;
  font-size: 0.95rem;
  font-weight: 600;
`;

const MarkdownBlockquote = styled(Blockquote)`
  margin: 1.25em 0;
  padding: 0.75em 1em;
  color: ${({ theme }) => theme.color.textPrimary.alpha(0.88).toString()};
  background-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.06).toString()};
  border-left: 3px solid ${({ theme }) => theme.color.accentPrimary.toString()};
  border-radius: 0 0.5rem 0.5rem 0;

  & p:last-of-type {
    margin-bottom: 0;
  }

  /* The alert title of markdown-to-jsx, the translated title is rendered instead */
  & > header {
    display: none;
  }
`;

type CalloutType = 'note' | 'tip' | 'important' | 'warning' | 'caution';

const calloutColor = (theme: Theme, type: CalloutType) => {
  switch (type) {
    case 'tip':
      return theme.color.success;
    case 'important':
      return theme.color.accentSecondary;
    case 'warning':
      return theme.color.warning;
    case 'caution':
      return theme.color.error;
    case 'note':
    default:
      return theme.color.accentPrimary;
  }
};

const Callout = styled(MarkdownBlockquote)<{ $type: CalloutType }>`
  background-color: ${({ theme, $type }) => calloutColor(theme, $type).alpha(0.07).toString()};
  border-left-color: ${({ theme, $type }) => calloutColor(theme, $type).toString()};
`;

const CalloutTitle = styled.div<{ $type: CalloutType }>`
  margin-bottom: 0.35em;
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: ${({ theme, $type }) => calloutColor(theme, $type).toString()};
`;

const TableWrapper = styled.div`
  margin: 1.25em 0 1.5em;
  overflow-x: auto;
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.6rem;
`;

const MarkdownTable = styled(Table)`
  width: 100%;
  font-size: 0.875rem;
  background: transparent;
  box-shadow: none;
  border-radius: 0;
  border-collapse: collapse;
`;

const MarkdownTableHeaderCell = styled(TableHeaderCell)`
  padding: 0.6em 1em;
  font-size: 0.8rem;
  font-weight: 600;
  text-align: left;
  white-space: nowrap;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
  background-color: ${({ theme }) => theme.color.backgroundSecondary.alpha(0.6).toString()};
  border-bottom: 1px solid ${({ theme }) => theme.color.border.toString()};
`;

const MarkdownTableRow = styled(TableRow)`
  &:not(:last-of-type) td {
    border-bottom: 1px solid ${({ theme }) => theme.color.border.toString()};
  }

  &:hover td {
    background-color: ${({ theme }) => theme.color.backgroundSecondary.alpha(0.35).toString()};
  }
`;

const MarkdownTableCell = styled(TableCell)`
  padding: 0.6em 1em;
  vertical-align: top;
  border: none;
  color: ${({ theme }) => theme.color.textPrimary.alpha(0.88).toString()};

  & ul {
    margin: 0;
    padding-left: 1.1em;
  }
`;

const Hr = styled.hr`
  margin: 2em 0;
  border: none;
  border-top: 1px solid ${({ theme }) => theme.color.border.toString()};
`;

const Details = styled.details`
  margin: 1em 0 1.5em;
  padding: 0.25rem 1rem;
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.6rem;

  & > summary {
    padding: 0.5rem 0;
    cursor: pointer;
    color: ${({ theme }) => theme.color.textPrimary.toString()};
  }

  &[open] > summary {
    margin-bottom: 0.5rem;
  }
`;

type ElementProps<T extends keyof React.JSX.IntrinsicElements> = React.JSX.IntrinsicElements[T];

const MarkdownLink: React.FC<ElementProps<'a'>> = ({ href = '', title, children }) => {
  // Anchor of the current page
  if (href.startsWith('#')) {
    const anchor = decodeURIComponent(href.slice(1));

    return (
      <InternalLink
        to={href}
        title={title}
        onClick={event => {
          event.preventDefault();
          if (scrollToAnchor(anchor)) {
            setLocationHash(anchor);
          }
        }}
      >
        {children}
      </InternalLink>
    );
  }

  if (/^https?:\/\//.test(href)) {
    return (
      <ExternalLink href={href} title={title} target="_blank" rel="noopener noreferrer">
        {children}
        <ExternalLinkIcon />
      </ExternalLink>
    );
  }

  const { internal, url } = resolveMarkdownLink(href);

  return internal ? (
    <InternalLink to={url} title={title}>
      {children}
    </InternalLink>
  ) : (
    <ExternalLink href={url} title={title} target="_blank" rel="noopener noreferrer">
      {children}
      <ExternalLinkIcon />
    </ExternalLink>
  );
};

const MarkdownBlockquoteOrCallout: React.FC<ElementProps<'blockquote'>> = props => {
  const { className, children, ...rest } = props;
  const t = useT();
  const type = String(className || '').match(/markdown-alert-(\w+)/)?.[1]?.toLowerCase() as
    | CalloutType
    | undefined;

  if (!type) {
    return <MarkdownBlockquote {...rest}>{children}</MarkdownBlockquote>;
  }

  const titles: Record<CalloutType, MessageId> = {
    note: 'callout.note',
    tip: 'callout.tip',
    important: 'callout.important',
    warning: 'callout.warning',
    caution: 'callout.caution',
  };

  return (
    <Callout $type={type} {...rest}>
      <CalloutTitle $type={type}>{t(titles[type] ?? 'callout.note')}</CalloutTitle>
      {children}
    </Callout>
  );
};

/**
 * Inline code. Code blocks are rendered by the `pre` override
 */
const MarkdownCode: React.FC<ElementProps<'code'>> = ({ children }) => (
  <CodeInline>{React.Children.toArray(children).join('').replace(/\n$/, '')}</CodeInline>
);

/**
 * Code block: `<pre><code class="language-xxx">`. A block without the language is plain text
 */
const MarkdownPre: React.FC<ElementProps<'pre'>> = ({ children }) => {
  const child = React.Children.toArray(children)[0];
  const props = React.isValidElement<{ className?: string; children?: React.ReactNode }>(child)
    ? child.props
    : { className: '', children };
  const language =
    String(props.className || '').match(/(?:^|\s)(?:language|lang)-([\w-]+)/)?.[1] ?? 'text';
  const code = React.Children.toArray(props.children).join('');

  if (language === 'mermaid') {
    return <Mermaid code={code} />;
  }

  return <SyntaxHighlighter language={language} code={code} />;
};

const options = {
  slugify: (text: string) => slugify(text),
  overrides: {
    h1: MarkdownH1,
    h2: MarkdownH2,
    h3: MarkdownH3,
    h4: MarkdownH4,
    h5: MarkdownH5,
    h6: MarkdownH5,
    img: Img,
    blockquote: MarkdownBlockquoteOrCallout,
    b: MarkdownStrong,
    strong: MarkdownStrong,
    em: MarkdownEm,
    p: MarkdownParagraph,
    ul: MarkdownUl,
    ol: MarkdownOl,
    hr: Hr,
    details: Details,
    table: ({ children, ...rest }: ElementProps<'table'>) => (
      <TableWrapper>
        <MarkdownTable {...rest}>{children}</MarkdownTable>
      </TableWrapper>
    ),
    thead: TableHeader,
    tbody: TableBody,
    tr: MarkdownTableRow,
    td: MarkdownTableCell,
    th: MarkdownTableHeaderCell,
    caption: TableCaption,
    pre: MarkdownPre,
    a: MarkdownLink,
    code: MarkdownCode,
  },
};

const RenderMarkdown: React.FC<Props> = ({ children }) => {
  const { hash } = useLocation();

  // Pages are loaded asynchronously, so the router can not scroll to the anchor from the URL
  React.useEffect(() => {
    if (hash) {
      requestAnimationFrame(() => scrollToAnchor(decodeURIComponent(hash.slice(1)), 'auto'));
    }
  }, [hash, children]);

  return <Markdown options={options}>{children}</Markdown>;
};

export default RenderMarkdown;
