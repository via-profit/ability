import React from 'react';
import type { RouteObject } from 'react-router-dom';

import DocLoadError from '~/pages/DocPage/DocLoadError';
import TemplateBase from '~/templates/TemplateBase';
import { docs, loadDoc } from '~/utils/docs';
import type { Locale } from '~/utils/locale';

const notFound: RouteObject = {
  path: '*',
  lazy: async () => ({
    Component: (await import(/* webpackChunkName: "not-found" */ '~/pages/NotFound')).default,
  }),
  handle: 'notFound.title',
  loader: () => new Response(null, { status: 404 }),
};

export const createRoutes = (locale: Locale): RouteObject[] => [
  {
    path: '/docs',
    id: 'docs',
    lazy: async () => ({
      Component: (await import(/* webpackChunkName: "docs-layout" */ '~/templates/TemplateDocs'))
        .default,
    }),
    children: [
      ...docs.map(
        (doc): RouteObject => ({
          id: doc.id,
          ...(doc.path === '/docs' ? { index: true } : { path: doc.path.slice('/docs/'.length) }),
          lazy: async () => {
            const [{ default: DocPage }, content] = await Promise.all([
              import(/* webpackChunkName: "doc-page" */ '~/pages/DocPage'),
              loadDoc(doc.id, locale),
            ]);

            return { element: <DocPage doc={doc} content={content} /> };
          },
          ErrorBoundary: DocLoadError,
          handle: doc.title,
        }),
      ),
      { ...notFound, id: 'docs-not-found' },
    ],
  },
  {
    path: '/',
    id: 'base',
    element: <TemplateBase />,
    children: [
      {
        id: 'home',
        index: true,
        lazy: async () => ({
          Component: (await import(/* webpackChunkName: "home" */ '~/pages/Home')).default,
        }),
      },
      {
        id: 'playground',
        path: 'playground',
        handle: 'playground.title',
        lazy: async () => ({
          Component: (await import(/* webpackChunkName: "playground" */ '~/pages/Playground'))
            .default,
        }),
      },
      { ...notFound, id: 'not-found' },
    ],
  },
];
