import React from 'react';
import styled from '@emotion/styled';
import UiKitLoadingIndicator from '@via-profit/ui-kit/LoadingIndicator';

const Container = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 12rem;
  flex: 1;
  color: ${({ theme }) => theme.color.accentPrimary.toString()};
`;

/**
 * Loading indicator of the pages and lazy parts
 */
const LoadingIndicator: React.FC = () => (
  <Container>
    <UiKitLoadingIndicator />
  </Container>
);

export default LoadingIndicator;
