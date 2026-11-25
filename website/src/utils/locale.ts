export type Locale = 'ru' | 'en';

export const defaultLocale: Locale = 'en';

export const getPathLocale = (pathname: string): Locale | undefined => {
  const segment = pathname.split('/')[1];

  return segment === 'en' || segment === 'ru' ? segment : undefined;
};
