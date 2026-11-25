import React from 'react';
import { useMatches } from 'react-router-dom';

import { useT } from '~/translations';
import type { MessageId } from '~/translations';

export const formatPageTitle = (sectionTitle?: string) =>
  sectionTitle ? `${sectionTitle} · @via-profit/ability` : '@via-profit/ability';

const PageTitle = () => {
  const id = useMatches().at(-1)?.handle as MessageId | undefined;
  const t = useT();
  const title = formatPageTitle(id ? t(id) : undefined);

  React.useEffect(() => {
    document.title = title;
  }, [title]);

  return null;
};

export default PageTitle;
