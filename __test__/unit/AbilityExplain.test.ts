import { AbilityMatch } from '../../src/core/AbilityMatch';
import { AbilityExplain } from '../../src/core/AbilityExplain';

describe('AbilityExplain', () => {
  it('should format the explanation tree as a string', () => {
    const child = new AbilityExplain({
      type: 'rule',
      name: 'Rule',
      match: AbilityMatch.match,
      debugInfo: 'subject condition resource',
    });
    const explanation = new AbilityExplain(
      {
        type: 'policy',
        name: 'Policy',
        match: AbilityMatch.mismatch,
      },
      [child],
    );

    expect(explanation.toString()).toBe(
      '[MISMATCH ✗]   POLICY    Policy\n' +
        '   └─ [MATCH ✓]      RULE      Rule (subject condition resource)',
    );
  });

  it('should serialize the explanation tree to JSON', () => {
    const child = new AbilityExplain({
      type: 'rule',
      name: 'Rule',
      match: AbilityMatch.match,
      debugInfo: 'subject condition resource',
    });
    const explanation = new AbilityExplain(
      {
        type: 'policy',
        name: 'Policy',
        match: AbilityMatch.mismatch,
      },
      [child],
    );
    expect(explanation.toJSON()).toEqual({
      type: 'policy',
      name: 'Policy',
      match: AbilityMatch.mismatch,
      children: [
        {
          type: 'rule',
          name: 'Rule',
          match: AbilityMatch.match,
          debugInfo: 'subject condition resource',
          children: [],
        },
      ],
    });
    expect(JSON.stringify(explanation)).toBe(JSON.stringify(explanation.toJSON()));
  });
});
