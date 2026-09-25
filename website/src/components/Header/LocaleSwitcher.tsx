import React from 'react';
import styled from '@emotion/styled';

import { useUi } from '~/providers/UiProvider';
import { useT } from '~/translations';
import IconButton from './IconButton';

const Label = styled.span`
  font-family: var(--font-mono);
  font-size: 0.75rem;
  font-weight: 500;
  letter-spacing: 0.04em;
`;

const LocaleSwitcher: React.FC = () => {
  const { locale, setLocale } = useUi();
  const t = useT();
  const title = t('header.language');

  return (
    <IconButton
      type="button"
      title={title}
      aria-label={title}
      onClick={() => setLocale(locale === 'ru' ? 'en' : 'ru')}
    >
      <Label>{locale === 'ru' ? 'EN' : 'RU'}</Label>
    </IconButton>
  );
};

export default LocaleSwitcher;
