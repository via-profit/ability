import {
  AbilityDSLParser,
  AbilityDSLSyntaxError,
  AbilityJSONParser,
  AbilityResolver,
  AbilityTypeGenerator,
  AllMustPermitStrategy,
  AnyPermitStrategy,
  DenyOverridesStrategy,
  FirstMatchStrategy,
  OnlyOneApplicableStrategy,
  PermitOverridesStrategy,
  PriorityStrategy,
  SequentialLastMatchStrategy,
} from '@via-profit/ability';
import type { AbilityPolicy, AbilityResultExplainJSON } from '@via-profit/ability';

export const strategies = {
  DenyOverridesStrategy,
  PermitOverridesStrategy,
  FirstMatchStrategy,
  SequentialLastMatchStrategy,
  PriorityStrategy,
  AllMustPermitStrategy,
  OnlyOneApplicableStrategy,
  AnyPermitStrategy,
} as const;

export type StrategyName = keyof typeof strategies;

export const strategyNames = Object.keys(strategies) as StrategyName[];

export interface SourceError {
  readonly message: string;
  readonly line: number;
  readonly column: number;
  readonly length: number;
}

export interface EvaluationResult {
  readonly policies: readonly AbilityPolicy[];
  /**
   * Permission keys of the policies without wildcards
   */
  readonly permissions: readonly string[];
  readonly dslError: SourceError | null;
  readonly contextError: SourceError | null;
  readonly check: {
    readonly isAllowed: boolean;
    readonly decisive: string | null;
    readonly policiesForKey: number;
    readonly explain: AbilityResultExplainJSON;
    readonly explainText: string;
    /**
     * Average time of one check in milliseconds
     */
    readonly duration: number;
  } | null;
  readonly typeDefs: string;
  readonly json: string;
}

/**
 * Position of the JSON syntax error. Browsers report either «line X column Y» or «position N»
 */
const jsonErrorPosition = (message: string, source: string) => {
  const lineColumn = message.match(/line (\d+) column (\d+)/);
  if (lineColumn) {
    return { line: Number(lineColumn[1]), column: Number(lineColumn[2]) };
  }

  const position = message.match(/position (\d+)/);
  if (position) {
    const before = source.slice(0, Number(position[1])).split('\n');

    return { line: before.length, column: before[before.length - 1].length + 1 };
  }

  return { line: 1, column: 1 };
};

const parseDSL = (dsl: string) => {
  try {
    return { policies: new AbilityDSLParser(dsl).parse(), error: null };
  } catch (err) {
    if (err instanceof AbilityDSLSyntaxError) {
      const lengthMatch = err.context.match(/\|\s*(~+)/);

      return {
        policies: [],
        error: {
          message: err.details,
          line: err.line,
          column: err.column,
          length: lengthMatch ? lengthMatch[1].length : 1,
        },
      };
    }

    return {
      policies: [],
      error: { message: err instanceof Error ? err.message : String(err), line: 1, column: 1, length: 1 },
    };
  }
};

const parseContext = (context: string) => {
  try {
    const value = JSON.parse(context);
    if (typeof value !== 'object' || value === null || Array.isArray(value)) {
      throw new Error('The context must be an object: { "resource": {...}, "environment": {...} }');
    }

    return {
      resource: (value.resource ?? {}) as Record<string, unknown>,
      environment: (value.environment ?? undefined) as Record<string, unknown> | undefined,
      error: null,
    };
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);

    return {
      resource: null,
      environment: undefined,
      error: { message, ...jsonErrorPosition(message, context), length: 1 },
    };
  }
};

/**
 * Parses the policies, checks the permission and collects everything the playground shows
 */
export const evaluate = (
  dsl: string,
  context: string,
  strategyName: StrategyName,
  permission: string,
): EvaluationResult => {
  const { policies, error: dslError } = parseDSL(dsl);
  const { resource, environment, error: contextError } = parseContext(context);

  const permissions = [...new Set(policies.map(policy => policy.permission))]
    .filter(key => !key.split('.').includes('*'))
    .sort();

  let check: EvaluationResult['check'] = null;

  if (!dslError && !contextError && resource && permission.trim()) {
    const Strategy = (strategies[strategyName] ?? DenyOverridesStrategy) as typeof DenyOverridesStrategy;
    const resolver = new AbilityResolver(policies, Strategy);
    const key = permission.trim().replace(/^permission\./, '');
    const resolve = () => (resolver.resolve as (...args: unknown[]) => ReturnType<typeof resolver.resolve>)(
      key,
      resource,
      environment,
    );

    const result = resolve();

    // Average of several runs: a single check is faster than the timer precision of the browser
    const runs = 200;
    const start = performance.now();
    for (let i = 0; i < runs; i++) {
      resolve();
    }
    const duration = (performance.now() - start) / runs;

    check = {
      isAllowed: result.isAllowed(),
      decisive: result.decisive()?.name ?? null,
      policiesForKey: result.strategy.policies.length,
      explain: result.explainToJSON(),
      explainText: result.explainToString(),
      duration,
    };
  }

  return {
    policies,
    permissions,
    dslError,
    contextError,
    check,
    typeDefs: dslError ? '' : new AbilityTypeGenerator(policies).generateTypeDefs(),
    json: dslError ? '' : JSON.stringify(AbilityJSONParser.toJSON(policies), null, 2),
  };
};
