import React from 'react';

export type ThemeName = 'dark' | 'light';
export type Locale = 'ru' | 'en';

export interface UiState {
  readonly theme: ThemeName;
  readonly locale: Locale;
}

interface UiContextValue extends UiState {
  readonly setTheme: (theme: ThemeName) => void;
  readonly setLocale: (locale: Locale) => void;
}

const STORAGE_KEY = 'ability-ui';

const UiContext = React.createContext<UiContextValue | null>(null);

/**
 * Settings chosen by the user earlier. Unknown values are ignored
 */
const readStoredState = (): Partial<UiState> => {
  try {
    const stored = JSON.parse(window.localStorage.getItem(STORAGE_KEY) || '{}');

    return {
      ...(stored.theme === 'dark' || stored.theme === 'light' ? { theme: stored.theme } : {}),
      ...(stored.locale === 'ru' || stored.locale === 'en' ? { locale: stored.locale } : {}),
    };
  } catch {
    return {};
  }
};

const defaultState = (): UiState => ({
  theme: 'dark',
  locale: navigator.language.toLowerCase().startsWith('ru') ? 'ru' : 'en',
});

export const UiProvider: React.FC<{ readonly children: React.ReactNode }> = ({ children }) => {
  const [state, setState] = React.useState<UiState>(() => ({
    ...defaultState(),
    ...readStoredState(),
  }));

  React.useEffect(() => {
    try {
      window.localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    } catch {
      // storage is not available (private mode), settings live until the page reload
    }
    document.documentElement.lang = state.locale;
  }, [state]);

  const value = React.useMemo<UiContextValue>(
    () => ({
      ...state,
      setTheme: theme => setState(prev => ({ ...prev, theme })),
      setLocale: locale => setState(prev => ({ ...prev, locale })),
    }),
    [state],
  );

  return <UiContext.Provider value={value}>{children}</UiContext.Provider>;
};

export const useUi = (): UiContextValue => {
  const value = React.useContext(UiContext);
  if (!value) {
    throw new Error('useUi must be used inside UiProvider');
  }

  return value;
};

export default UiProvider;
