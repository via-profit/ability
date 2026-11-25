import React from 'react';
import styled from '@emotion/styled';
import { useLocation } from 'react-router-dom';

import { useUi } from '~/providers/UiProvider';
import { useT } from '~/translations';
import { IconLink } from './IconButton';

const Label = styled.span`
  font-family: var(--font-mono);
  font-size: 0.75rem;
  font-weight: 500;
  letter-spacing: 0.04em;
`;

const LocaleSwitcher: React.FC = () => {
  const { locale } = useUi();
  const { pathname, search, hash } = useLocation();
  const [isMounted, setMounted] = React.useState(false);
  React.useEffect(() => setMounted(true), []);

  const target = locale === 'ru' ? 'en' : 'ru';
  const t = useT();
  const title = t('header.language');

  return (
    <IconLink
      href={`/${target}${pathname}${search}${isMounted ? hash : ''}`}
      hrefLang={target}
      title={title}
      aria-label={title}
    >
      <Label>{target.toUpperCase()}</Label>
    </IconLink>
  );
};

export default LocaleSwitcher;
