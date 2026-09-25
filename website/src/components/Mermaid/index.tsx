import React from 'react';
import styled from '@emotion/styled';
import { useTheme } from '@emotion/react';

const Container = styled.div`
  display: flex;
  justify-content: center;
  margin: 1.25em 0 1.5em;
  padding: 1.5rem;
  overflow-x: auto;
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.75rem;
  background-color: ${({ theme }) => theme.color.codeBackground.toString()};

  & svg {
    max-width: 100%;
    height: auto;
  }
`;

let counter = 0;

/**
 * Mermaid diagram. The library is heavy, so it is loaded only on the pages with diagrams
 */
const Mermaid: React.FC<{ readonly code: string }> = ({ code }) => {
  const theme = useTheme();
  const [svg, setSvg] = React.useState<string | null>(null);

  React.useEffect(() => {
    let isActual = true;

    import(/* webpackChunkName: "mermaid" */ 'mermaid')
      .then(async ({ default: mermaid }) => {
        mermaid.initialize({
          startOnLoad: false,
          theme: theme.isDark ? 'dark' : 'default',
          fontFamily: 'Inter, system-ui, sans-serif',
        });
        counter += 1;
        const result = await mermaid.render(`mermaid-${counter}`, code);
        if (isActual) {
          setSvg(result.svg);
        }
      })
      .catch(() => setSvg(null));

    return () => {
      isActual = false;
    };
  }, [code, theme.isDark]);

  if (!svg) {
    return <Container as="pre">{code}</Container>;
  }

  return <Container dangerouslySetInnerHTML={{ __html: svg }} />;
};

export default Mermaid;
