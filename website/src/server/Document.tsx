import React from 'react';

import type { ThemeName } from '~/providers/UiProvider';
import type { Locale } from '~/utils/locale';

const descriptions: Record<Locale, string> = {
  en: 'Lightweight ABAC engine with DSL, TypeScript types generation and zero dependencies',
  ru: 'Легковесный движок ABAC с DSL, генерацией типов TypeScript и без зависимостей',
};

interface Props {
  readonly children: React.ReactNode;
  readonly locale: Locale;
  readonly theme: ThemeName;
  readonly title: string;
  readonly scripts: readonly string[];
}

const Document: React.FC<Props> = ({ children, locale, theme, title, scripts }) => (
  <html lang={locale}>
    <head>
      <meta charSet="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <meta name="color-scheme" content="dark light" />
      <meta name="description" content={descriptions[locale]} />
      <link rel="icon" href="/favicon.svg" type="image/svg+xml" />
      <title>{title}</title>
    </head>
    <body>
      <div id="app" data-theme={theme}>
        {children}
      </div>
      {scripts.map(src => (
        <script key={src} src={src} defer />
      ))}
    </body>
  </html>
);

export default Document;
