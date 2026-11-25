import React from 'react';
import { hydrateRoot } from 'react-dom/client';
import { createBrowserRouter, matchRoutes, RouterProvider } from 'react-router-dom';
import createCache from '@emotion/cache';

import Application from '~/application/Application';
import { createRoutes } from '~/routes';
import { getPathLocale } from '~/utils/locale';

const bootstrap = async () => {
  const root = document.getElementById('app');
  const locale = getPathLocale(window.location.pathname);
  if (!root || !locale) {
    throw new Error('Cannot hydrate the Ability website without the app root and a locale');
  }

  const basename = `/${locale}`;
  const routes = createRoutes(locale);
  const matches = matchRoutes(routes, window.location, basename) ?? [];

  // Load the code of the server-rendered route tree before hydrating its HTML.
  await Promise.all(
    matches.map(async ({ route }) => {
      if (typeof route.lazy === 'function') {
        Object.assign(route, await route.lazy());
        delete route.lazy;
      }
    }),
  );

  const cssCache = createCache({ key: 'app' });
  const theme = root.dataset.theme === 'light' ? 'light' : 'dark';
  const router = createBrowserRouter(routes, { basename });

  hydrateRoot(
    root,
    <Application cssCache={cssCache} locale={locale} theme={theme}>
      <RouterProvider router={router} />
    </Application>,
  );
};

void bootstrap().catch(error => console.error('Ability hydration failed', error));
