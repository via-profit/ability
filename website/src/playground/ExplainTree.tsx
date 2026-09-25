import React from 'react';
import styled from '@emotion/styled';
import { Theme } from '@emotion/react';
import type { AbilityExplainJSON } from '@via-profit/ability';

const statusColor = (theme: Theme, match: string) => {
  switch (match) {
    case 'match':
      return theme.color.success;
    case 'mismatch':
      return theme.color.error;
    case 'except-mismatch':
      return theme.color.warning;
    default:
      return theme.color.textSecondary;
  }
};

const statusLabel: Record<string, string> = {
  match: 'MATCH',
  mismatch: 'MISMATCH',
  'except-mismatch': 'EXCEPT',
  disabled: 'DISABLED',
  pending: 'SKIPPED',
};

const typeLabel: Record<string, string> = {
  policy: 'policy',
  ruleSet: 'group',
  rule: 'rule',
};

const List = styled.ul`
  margin: 0;
  padding: 0;
  list-style: none;

  & & {
    margin-left: 0.6rem;
    padding-left: 0.9rem;
    border-left: 1px solid ${({ theme }) => theme.color.border.toString()};
  }
`;

const Node = styled.li<{ $muted: boolean }>`
  margin: 0.2rem 0;
  opacity: ${({ $muted }) => ($muted ? 0.55 : 1)};
`;

const Row = styled.div`
  display: flex;
  align-items: baseline;
  flex-wrap: wrap;
  gap: 0.25rem 0.5rem;
  font-size: 0.82rem;
  line-height: 1.5;
`;

const Status = styled.span<{ $match: string }>`
  flex: 0 0 auto;
  min-width: 4.6rem;
  padding: 0 0.35rem;
  font-family: var(--font-mono);
  font-size: 0.66rem;
  font-weight: 600;
  line-height: 1.25rem;
  text-align: center;
  color: ${({ theme, $match }) => statusColor(theme, $match).toString()};
  background-color: ${({ theme, $match }) => statusColor(theme, $match).alpha(0.12).toString()};
  border: 1px solid ${({ theme, $match }) => statusColor(theme, $match).alpha(0.35).toString()};
  border-radius: 0.3rem;
`;

const Type = styled.span`
  font-family: var(--font-mono);
  font-size: 0.7rem;
  text-transform: uppercase;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const Name = styled.span`
  font-weight: 500;
  color: ${({ theme }) => theme.color.textPrimary.toString()};
  overflow-wrap: anywhere;
`;

const Debug = styled.code`
  font-family: var(--font-mono);
  font-size: 0.76rem;
  color: ${({ theme }) => theme.color.accentPrimary.toString()};
  overflow-wrap: anywhere;
`;

const ExplainNode: React.FC<{ readonly node: AbilityExplainJSON }> = ({ node }) => (
  <Node $muted={node.match === 'pending' || node.match === 'disabled'}>
    <Row>
      <Status $match={node.match}>{statusLabel[node.match] ?? node.match}</Status>
      <Type>{typeLabel[node.type] ?? node.type}</Type>
      <Name>{node.name}</Name>
      {node.debugInfo && <Debug>{node.debugInfo}</Debug>}
    </Row>
    {node.children.length > 0 && (
      <List>
        {node.children.map((child, index) => (
          <ExplainNode key={`${child.name}-${index}`} node={child} />
        ))}
      </List>
    )}
  </Node>
);

/**
 * Tree of the check: policies, groups and rules with their states
 */
const ExplainTree: React.FC<{ readonly policies: readonly AbilityExplainJSON[] }> = ({ policies }) => (
  <List>
    {policies.map((policy, index) => (
      <ExplainNode key={`${policy.name}-${index}`} node={policy} />
    ))}
  </List>
);

export default ExplainTree;
