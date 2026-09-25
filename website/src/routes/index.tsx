import React from 'react';
import type { RouteObject } from 'react-router-dom';

import LoadingIndicator from '~/components/LoadingIndicator';
import TemplateBase from '~/templates/TemplateBase';

const TemplateDocs = React.lazy(() => import(/* webpackChunkName: "docs" */ '~/templates/TemplateDocs'));
const DocPage = React.lazy(() => import(/* webpackChunkName: "docs" */ '~/pages/DocPage'));
const Home = React.lazy(() => import(/* webpackChunkName: "home" */ '~/pages/Home'));
const Playground = React.lazy(() => import(/* webpackChunkName: "playground" */ '~/pages/Playground'));
const NotFound = React.lazy(() => import(/* webpackChunkName: "not-found" */ '~/pages/NotFound'));

const lazy = (element: React.ReactNode) => (
  <React.Suspense fallback={<LoadingIndicator />}>{element}</React.Suspense>
);

export const routes: RouteObject[] = [
  {
    path: '/docs',
    element: lazy(<TemplateDocs />),
    children: [
      { index: true, element: lazy(<DocPage />) },
      { path: '*', element: lazy(<DocPage />) },
    ],
  },
  {
    path: '/',
    element: <TemplateBase />,
    children: [
      { index: true, element: lazy(<Home />) },
      { path: 'playground', element: lazy(<Playground />) },
      { path: '*', element: lazy(<NotFound />) },
    ],
  },
];

export default routes;
