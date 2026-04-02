import AbilityPolicy from '~/core/AbilityPolicy';
import AbilityCondition from '~/core/AbilityCondition';
import AbilityRule from '~/core/AbilityRule';

export type Primitive = string | number | boolean | null | undefined;
export type NestedDict<T = Primitive> = {
  [key: string]: NestedDict<T> | T;
};

export type ResourceObject = Record<string, unknown>;
export type ResourcesMap = Record<string, ResourceObject>;

export class AbilityTypeGenerator {
  readonly policies: readonly AbilityPolicy[];

  constructor(policies: readonly AbilityPolicy[]) {
    this.policies = policies;
  }
  /**
   * Generates TypeScript type definitions based on the provided policies.
   * @returns A generated type definitions.
   */
  public generateTypeDefs(): string {
    // Structure to store types: { [action]: { [subjectPath]: type } }
    const typeStructure: Record<string, Record<string, string>> = {};

    // Iterate through all policies
    this.policies.forEach(policy => {
      const action = policy.permission;

      // Initialize object for action if it doesn't exist
      if (!typeStructure[action]) {
        typeStructure[action] = {};
      }

      // Iterate through all ruleSets in the policy
      policy.ruleSet.forEach(ruleSet => {
        // Iterate through all rules in the ruleSet
        ruleSet.rules.forEach(rule => {
          const subjectPath = rule.subject;
          const existingType = typeStructure[action][subjectPath];
          const ruleType = this.determineTypeFromRule(rule);

          if (!ruleType) {
            return;
          }
          if (existingType && existingType !== ruleType) {
            // If a type already exists for this path, create a union
            typeStructure[action][subjectPath] = `${existingType} | ${ruleType}`;
          } else {
            typeStructure[action][subjectPath] = ruleType;
          }
        });
      });
    });

    // Transform flat structure into nested structure for easier use
    const nestedStructure = this.buildNestedStructure(typeStructure);

    return this.formatTypeDefinitions(nestedStructure);
  }

  /**
   * Determines TypeScript type based on the rule
   * @param rule - The rule to analyze
   * @returns TypeScript type as string
   */
  private determineTypeFromRule(rule: AbilityRule): string | null {
    if (
      rule.condition.isEqual(AbilityCondition.never) ||
      rule.condition.isEqual(AbilityCondition.always)
    ) {
      return null;
    }
    // Numeric comparisons - always number
    if (
      rule.condition.isEqual(AbilityCondition.greater_than) ||
      rule.condition.isEqual(AbilityCondition.less_than) ||
      rule.condition.isEqual(AbilityCondition.greater_or_equal) ||
      rule.condition.isEqual(AbilityCondition.less_or_equal)
    ) {
      return 'number';
    }

    // Array operations
    if (
      rule.condition.isEqual(AbilityCondition.in) ||
      rule.condition.isEqual(AbilityCondition.not_in)
    ) {
      return this.getArrayType(rule.resource);
    }

    // Equality/Inequality operations
    if (
      rule.condition.isEqual(AbilityCondition.equals) ||
      rule.condition.isEqual(AbilityCondition.not_equals)
    ) {
      return this.getPrimitiveType(rule.resource);
    }

    return 'any';
  }

  /**
   * Gets TypeScript type for array values
   * @param resource - The resource value to analyze
   * @returns TypeScript array type as string
   */
  private getArrayType(resource: unknown): string {
    if (Array.isArray(resource)) {
      if (resource.length === 0) {
        return 'any[]';
      }

      // Determine types of array elements
      const elementTypes = new Set(resource.map(item => this.getPrimitiveType(item)));
      const elementType =
        elementTypes.size === 1
          ? Array.from(elementTypes)[0]
          : `(${Array.from(elementTypes).join(' | ')})`;

      return `readonly ${elementType}[]`;
    }

    // If resource is not an array but condition is in/not_in,
    // it expects an array of such elements
    return `readonly ${this.getPrimitiveType(resource)}[]`;
  }

  /**
   * Gets primitive TypeScript type for a value
   * @param value - The value to analyze
   * @returns TypeScript primitive type as string
   */
  private getPrimitiveType(value: unknown): string {
    if (value === null) {
      return 'null';
    }
    if (value === undefined) {
      return 'undefined';
    }

    switch (typeof value) {
      case 'string':
        return 'string';
      case 'number':
        return 'number';
      case 'boolean':
        return 'boolean';
      case 'object':
        if (Array.isArray(value)) {
          return 'array'; // special marker, handled separately
        }
        return 'object';
      default:
        return 'any';
    }
  }

  /**
   * Builds nested structure from flat paths
   * Example: 'user.profile.name' -> { user: { profile: { name: 'string' } } }
   * @param flatStructure - Flat structure with dot notation paths
   * @returns Nested object structure
   */
  private buildNestedStructure(
    flatStructure: Record<string, Record<string, string>>,
  ): Record<string, NestedDict<string>> {
    const result: Record<string, NestedDict<string>> = {};

    Object.entries(flatStructure).forEach(([action, paths]) => {
      result[action] = {};

      Object.entries(paths).forEach(([path, type]) => {
        const parts = path.split('.');
        let current = result[action];

        // Iterate through all parts except the last one
        for (let i = 0; i < parts.length - 1; i++) {
          const part = parts[i];
          const currentValue = current[part];

          if (!currentValue || typeof currentValue !== 'object') {
            const newObj: NestedDict<string> = {};
            current[part] = newObj;
            current = newObj;
          } else {
            current = currentValue;
          }
        }

        // Set type for the last part
        const lastPart = parts[parts.length - 1];
        current[lastPart] = type;
      });
    });

    return result;
  }

  /**
   * Formats type structure into a string
   * @param structure - Nested type structure
   * @returns Formatted TypeScript type definition string
   */
  private formatTypeDefinitions(structure: Record<string, NestedDict<string>>): string {
    let output = '// Automatically generated by via-profit/ability\n';
    output += '// Do not edit manually\n';
    output += 'export type Resources = {\n';

    const sortedActions = Object.keys(structure).sort();

    sortedActions.forEach(action => {
      const actionObj = structure[action];
      const isEmpty = Object.keys(actionObj).length === 0;

      if (isEmpty) {
        // Пустой объект → undefined
        output += `  ['${action}']: undefined;\n`;
      } else {
        // Непустой объект → как раньше
        output += `  ['${action}']: {\n`;
        output += this.formatNestedObject(actionObj, 4);
        output += '  };\n';
      }
    });

    output += '}\n';
    return output;
  }

  /**
   * Recursively formats nested object
   * @param obj - Object to format
   * @param indent - Current indentation level
   * @returns Formatted string
   */
  private formatNestedObject(obj: NestedDict<string>, indent: number): string {
    const spaces = ' '.repeat(indent);
    let output = '';

    // Sort keys for stable output
    const sortedKeys = Object.keys(obj).sort();

    sortedKeys.forEach(key => {
      const value = obj[key];

      if (typeof value === 'object' && value !== null) {
        // Nested object
        output += `${spaces}readonly ${key}: {\n`;
        output += this.formatNestedObject(value, indent + 2);
        output += `${spaces}};\n`;
      } else {
        // Primitive type
        output += `${spaces}readonly ${key}: ${value};\n`;
      }
    });

    return output;
  }
}

export default AbilityTypeGenerator;
