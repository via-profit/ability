import React from 'react';
import { renderToString } from 'react-dom/server';
import { createStaticHandler, createStaticRouter, StaticRouterProvider } from 'react-router-dom';
import createCache from '@emotion/cache';
import createEmotionServer from '@emotion/server/create-instance';

import Application from '~/application/Application';
import { formatPageTitle } from '~/application/PageTitle';
import type { ThemeName } from '~/providers/UiProvider';
import { createRoutes } from '~/routes';
import { getMessages } from '~/translations';
import type { MessageId } from '~/translations';
import type { Locale } from '~/utils/locale';
import Document from './Document';

const handlers = {
  en: createStaticHandler(createRoutes('en'), { basename: '/en' }),
  ru: createStaticHandler(createRoutes('ru'), { basename: '/ru' }),
};

export const renderHtml = async (
  url: URL,
  locale: Locale,
  theme: ThemeName,
  scripts: readonly string[],
) => {
  const handler = handlers[locale];
  const context = await handler.query(new Request(url));
  if (context instanceof Response) {
    return context;
  }

  const router = createStaticRouter(handler.dataRoutes, context);
  const id = context.matches.at(-1)?.route.handle as MessageId | undefined;
  const title = formatPageTitle(id ? getMessages(locale)[id] : undefined);
  const cssCache = createCache({ key: 'app' });
  const { extractCriticalToChunks, constructStyleTagsFromChunks } = createEmotionServer(cssCache);
  const markup = renderToString(
    <Document locale={locale} theme={theme} title={title} scripts={scripts}>
      <Application cssCache={cssCache} locale={locale} theme={theme}>
        <StaticRouterProvider router={router} context={context} />
      </Application>
    </Document>,
  );
  const styles = constructStyleTagsFromChunks(extractCriticalToChunks(markup));

  return new Response(`<!DOCTYPE html>${markup.replace('</head>', () => `${styles}</head>`)}`, {
    status: context.statusCode,
    headers: { 'Content-Type': 'text/html; charset=utf-8', 'Cache-Control': 'private, no-cache' },
  });
};
