import React from 'react';

import type { Locale } from '~/providers/UiProvider';
import { loadDoc, DocEntry } from '~/utils/docs';

interface DocContentState {
  readonly content: string | null;
  readonly isLoading: boolean;
  readonly error: boolean;
}

/**
 * Loads the markdown of the page for the current language.
 * The previous content stays on the screen until the next one is loaded
 */
export const useDocContent = (doc: DocEntry | null, locale: Locale): DocContentState => {
  const [state, setState] = React.useState<DocContentState>({
    content: null,
    isLoading: true,
    error: false,
  });

  React.useEffect(() => {
    if (!doc) {
      return undefined;
    }

    let isActual = true;
    setState(prev => ({ ...prev, isLoading: true, error: false }));

    loadDoc(doc.id, locale)
      .then(content => {
        if (isActual) {
          setState({ content, isLoading: false, error: false });
        }
      })
      .catch(() => {
        if (isActual) {
          setState({ content: null, isLoading: false, error: true });
        }
      });

    return () => {
      isActual = false;
    };
  }, [doc, locale]);

  return state;
};
