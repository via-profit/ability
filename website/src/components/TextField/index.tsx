import styled from '@emotion/styled';
import UiKitTextField from '@via-profit/ui-kit/TextField';

/**
 * Text field of the site based on the ui-kit TextField
 */
const TextField = styled(UiKitTextField)`
  min-width: 0;

  & input {
    font-family: var(--font-mono);
    font-size: 0.85rem;
  }
`;

export default TextField;
