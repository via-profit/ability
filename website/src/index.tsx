import React from 'react';
import { createRoot } from 'react-dom/client';
import { createBrowserRouter, RouterProvider } from 'react-router-dom';
import { CacheProvider } from '@emotion/react';
import createCache from '@emotion/cache';

import UiProvider from '~/providers/UiProvider';
import ThemeProvider from '~/providers/ThemeProvider';
import LocaleProvider from '~/providers/LocaleProvider';
import routes from '~/routes';
import { PUBLIC_PATH } from '~/utils/env';

const bootstrap = () => {
  const rootElement = document.getElementById('app');
  if (!rootElement) {
    throw new Error('Root element with id #app not found');
  }

  const cssCache = createCache({ key: 'app' });
  const router = createBrowserRouter(routes, {
    basename: PUBLIC_PATH.replace(/\/+$/, '') || '/',
  });

  createRoot(rootElement).render(
    <React.StrictMode>
      <CacheProvider value={cssCache}>
        <UiProvider>
          <ThemeProvider>
            <LocaleProvider>
              <RouterProvider router={router} />
            </LocaleProvider>
          </ThemeProvider>
        </UiProvider>
      </CacheProvider>
    </React.StrictMode>,
  );
};

bootstrap();
