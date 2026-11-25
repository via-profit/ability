import readmeEn from '@repository/docs/en/README.md';
import readmeRu from '@repository/docs/ru/README.md';

import { getMessages } from '~/translations';
import { docs, docRepositoryFile, loadDoc } from '~/utils/docs';
import { ABILITY_VERSION } from '~/utils/env';
import { getPathLocale } from '~/utils/locale';
import type { Locale } from '~/utils/locale';

const locales = ['en', 'ru'] as const;
const languageNames = { en: 'English', ru: 'Русский' };
const readmes = { en: readmeEn, ru: readmeRu };
const guides = docs.filter(doc => doc.id !== 'changelog');
const projectName = '@via-profit/ability';
const summary =
  'A lightweight ABAC engine with a policy DSL, TypeScript type generation, and zero dependencies.';

export const discoveryContentTypes = {
  '/robots.txt': 'text/plain; charset=utf-8',
  '/sitemap.xml': 'application/xml; charset=utf-8',
  '/llms.txt': 'text/plain; charset=utf-8',
  '/llms-full.txt': 'text/plain; charset=utf-8',
} as const;

type DiscoveryPath = keyof typeof discoveryContentTypes;

export const isDiscoveryPath = (pathname: string): pathname is DiscoveryPath =>
  Object.hasOwn(discoveryContentTypes, pathname);

const configuredSite = process.env.SITE_URL ? new URL(process.env.SITE_URL) : undefined;
if (
  configuredSite &&
  (!['http:', 'https:'].includes(configuredSite.protocol) ||
    configuredSite.username ||
    configuredSite.password ||
    configuredSite.pathname !== '/' ||
    configuredSite.search ||
    configuredSite.hash)
) {
  throw new Error('SITE_URL must be an absolute HTTP(S) origin without a path, query or fragment');
}

export const getSiteOrigin = (host: string | undefined, port: number) => {
  if (configuredSite) {
    return configuredSite.origin;
  }

  // Local development uses the requested host. Behind HTTPS, configure SITE_URL explicitly.
  const requestUrl = host && /^[\w.:[\]-]+$/.test(host) ? URL.parse(`http://${host}`) : null;

  return requestUrl?.origin ?? `http://localhost:${port}`;
};

const renderSitemap = (origin: string) => {
  const paths = ['', ...docs.map(doc => doc.path), '/playground'];
  const urls = locales.flatMap(locale =>
    paths.map(path => `  <url><loc>${origin}/${locale}${path}</loc></url>`),
  );

  return [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    ...urls,
    '</urlset>',
    '',
  ].join('\n');
};

const renderLlmsIndex = (origin: string) => {
  const languageSections = locales.map(locale => {
    const messages = getMessages(locale);

    return [
      `## ${languageNames[locale]}`,
      '',
      ...guides.map(doc => `- [${messages[doc.title]}](${origin}/${locale}${doc.path}.md)`),
    ].join('\n');
  });

  return (
    [
      `# ${projectName}`,
      `> ${summary}`,
      `This documentation site was built for Ability ${ABILITY_VERSION}.`,
      ...languageSections,
      [
        '## Optional',
        '',
        `- [Complete documentation in both languages](${origin}/llms-full.txt)`,
        `- [Changelog](${origin}/ru/docs/changelog.md) (Russian)`,
      ].join('\n'),
      '',
    ]
      .join('\n\n')
      .trimEnd() + '\n'
  );
};

const renderLanguage = async (locale: Locale) => {
  const articles = await Promise.all(guides.map(doc => loadDoc(doc.id, locale)));
  const sources = [
    { file: `docs/${locale}/README.md`, content: readmes[locale] },
    ...guides.map((doc, index) => ({
      file: docRepositoryFile(doc.id, locale),
      content: articles[index],
    })),
  ];

  return [
    `## ${languageNames[locale]}`,
    ...sources.map(({ file, content }) => `### ${file}\n\n${content.trim()}`),
  ].join('\n\n---\n\n');
};

let fullDocumentation: Promise<string> | undefined;

const renderFullDocumentation = async () => {
  const [en, ru, changelog] = await Promise.all([
    renderLanguage('en'),
    renderLanguage('ru'),
    loadDoc('changelog', 'ru'),
  ]);

  return (
    [
      `# ${projectName} ${ABILITY_VERSION}`,
      `> ${summary}`,
      en,
      ru,
      `## CHANGELOG.md (Русский)\n\n${changelog.trim()}`,
    ].join('\n\n---\n\n') + '\n'
  );
};

export const renderDiscovery = (
  pathname: DiscoveryPath,
  origin: string,
): string | Promise<string> => {
  switch (pathname) {
    case '/robots.txt':
      return `User-agent: *\nAllow: /\n\nSitemap: ${origin}/sitemap.xml\n`;
    case '/sitemap.xml':
      return renderSitemap(origin);
    case '/llms.txt':
      return renderLlmsIndex(origin);
    case '/llms-full.txt':
      return (fullDocumentation ??= renderFullDocumentation());
  }
};

export const loadMarkdownPage = (pathname: string): Promise<string> | undefined => {
  const locale = getPathLocale(pathname);
  if (!locale) {
    return undefined;
  }

  const doc = docs.find(entry => pathname === `/${locale}${entry.path}.md`);

  return doc ? loadDoc(doc.id, locale) : undefined;
};
