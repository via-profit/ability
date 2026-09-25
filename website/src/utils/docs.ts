import type { Locale } from '~/providers/UiProvider';
import type { MessageId } from '~/translations';
import { GITHUB_URL } from './env';

export type DocId =
  | 'getting-started'
  | 'dsl'
  | 'resolver'
  | 'strategies'
  | 'types-generator'
  | 'server-and-client'
  | 'changelog';

export type DocGroup = 'start' | 'guides' | 'more';

export interface DocEntry {
  readonly id: DocId;
  readonly path: string;
  readonly group: DocGroup;
  readonly title: MessageId;
}

/**
 * Documentation pages in the order of reading. The content is taken from the `docs/` directory
 * of the repository, so the site and GitHub always show the same text
 */
export const docs: readonly DocEntry[] = [
  { id: 'getting-started', path: '/docs', group: 'start', title: 'doc.getting-started' },
  { id: 'dsl', path: '/docs/dsl', group: 'guides', title: 'doc.dsl' },
  { id: 'resolver', path: '/docs/resolver', group: 'guides', title: 'doc.resolver' },
  { id: 'strategies', path: '/docs/strategies', group: 'guides', title: 'doc.strategies' },
  { id: 'types-generator', path: '/docs/types-generator', group: 'guides', title: 'doc.types-generator' },
  { id: 'server-and-client', path: '/docs/server-and-client', group: 'guides', title: 'doc.server-and-client' },
  { id: 'changelog', path: '/docs/changelog', group: 'more', title: 'doc.changelog' },
];

export const docGroups: readonly { readonly group: DocGroup; readonly title: MessageId }[] = [
  { group: 'start', title: 'sidebar.group.start' },
  { group: 'guides', title: 'sidebar.group.guides' },
  { group: 'more', title: 'sidebar.group.more' },
];

export const findDocByPath = (pathname: string): DocEntry | null => {
  const normalized = pathname.replace(/\/+$/, '') || '/';

  return docs.find(doc => doc.path === normalized) ?? null;
};

export const findDocById = (id: string): DocEntry | null => docs.find(doc => doc.id === id) ?? null;

/**
 * Path of the markdown file in the repository
 */
export const docRepositoryFile = (id: DocId, locale: Locale): string =>
  id === 'changelog' ? 'CHANGELOG.md' : `docs/${locale}/${id}.md`;

export const docEditUrl = (id: DocId, locale: Locale): string =>
  `${GITHUB_URL}/edit/master/${docRepositoryFile(id, locale)}`;

/**
 * Loads the markdown of the page. Every page is a separate chunk
 */
export const loadDoc = async (id: DocId, locale: Locale): Promise<string> => {
  if (id === 'changelog') {
    return (await import(/* webpackChunkName: "doc-changelog" */ '@repository/CHANGELOG.md')).default;
  }

  return (
    await import(
      /* webpackChunkName: "doc-[request]" */
      /* webpackInclude: /docs[\\/](ru|en)[\\/][a-z-]+\.md$/ */
      `@repository/docs/${locale}/${id}.md`
    )
  ).default;
};

/**
 * Converts the link of the markdown document to the site route:
 * `./dsl.md#оператор` → `/docs/dsl#оператор`.
 * Links to other repository files lead to GitHub
 */
export const resolveMarkdownLink = (href: string): { readonly internal: boolean; readonly url: string } => {
  const decoded = (() => {
    try {
      return decodeURI(href);
    } catch {
      return href;
    }
  })();

  const match = decoded.match(/^(?:\.{1,2}\/)*(?:docs\/(?:ru|en)\/)?([\w-]+)\.md(#.*)?$/i);
  if (!match) {
    return { internal: false, url: decoded };
  }

  const [, name, hash = ''] = match;
  if (name.toLowerCase() === 'readme') {
    return { internal: true, url: `/docs${hash}` };
  }

  const doc = findDocById(name.toLowerCase());
  if (doc) {
    return { internal: true, url: `${doc.path}${hash}` };
  }

  return { internal: false, url: `${GITHUB_URL}/blob/master/${name}.md${hash}` };
};
