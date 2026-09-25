import React from 'react';
import styled from '@emotion/styled';
import Selectbox, { SelectboxItem } from '@via-profit/ui-kit/Selectbox';

export interface SelectOption<T extends string> {
  readonly value: T;
  readonly label: string;
}

export interface SelectProps<T extends string> {
  readonly label: string;
  readonly value: T | null;
  readonly options: readonly SelectOption<T>[];
  readonly onChange: (value: T) => void;
  readonly className?: string;
  /**
   * Text of the select without a value
   */
  readonly placeholder?: string;
}

const Container = styled.div`
  min-width: 0;
`;

/**
 * Select of the site based on the ui-kit Selectbox
 */
const Select = <T extends string>(props: SelectProps<T>) => {
  const { label, value, options, onChange, className, placeholder } = props;
  const [isOpen, setOpen] = React.useState(false);
  const selected = options.find(option => option.value === value) ?? null;

  return (
    <Container className={className}>
      <Selectbox
        fullWidth
        label={label}
        items={options}
        value={selected}
        isOpen={isOpen}
        onRequestOpen={() => setOpen(true)}
        onRequestClose={() => setOpen(false)}
        onChange={option => {
          if (option) {
            onChange(option.value);
          }
          setOpen(false);
        }}
        selectedItemToString={option => option.label}
        notSetLabel={placeholder}
      >
        {({ item }, itemProps) => (
          <SelectboxItem {...itemProps} key={item.value}>
            {item.label}
          </SelectboxItem>
        )}
      </Selectbox>
    </Container>
  );
};

export default Select;
