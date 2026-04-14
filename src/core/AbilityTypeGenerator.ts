import AbilityPolicy from './AbilityPolicy';
import { AbilityCondition } from './AbilityCondition';
import AbilityRule from './AbilityRule';

export type Primitive = string | number | boolean | null | undefined;
export type NestedDict<T = Primitive> = {
  [key: string]: NestedDict<T> | T;
};

export type ResourceObject = Record<string, unknown>;
export type EnvironmentObject = Record<string, unknown>;

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
    const resorceStructure: Record<string, Record<string, string>> = {};
    const environmentStructure: Record<string, Record<string, string>> = {};

    // tags
    const allTags = new Set<string>();
    // Iterate through all policies
    this.policies.forEach(policy => {
      policy.tags.forEach(tag => allTags.add(tag));

      const action = policy.permission;

      // Initialize object for action if it doesn't exist
      if (!resorceStructure[action]) {
        resorceStructure[action] = {};
      }

      // Iterate through all ruleSets in the policy
      policy.ruleSet.forEach(ruleSet => {
        // Iterate through all rules in the ruleSet
        ruleSet.rules.forEach(rule => {



          const subjectPath = rule.subject;
          const ruleType = this.determineTypeFromRule(rule);

          if (!ruleType) {
            return;
          }

          // -----------------------------
          // ENVIRONMENT HANDLING (subject)
          // -----------------------------
          if (subjectPath.startsWith('env.')) {
            const envPath = subjectPath.replace(/^env\./, '');

            if (!environmentStructure[action]) {
              environmentStructure[action] = {};
            }

            environmentStructure[action][envPath] = ruleType;
          } else {
            const existingType = resorceStructure[action][subjectPath];

            if (existingType && existingType !== ruleType) {
              resorceStructure[action][subjectPath] = `${existingType} | ${ruleType}`;
            } else {
              resorceStructure[action][subjectPath] = ruleType;
            }
          }

          // -----------------------------
          // RESOURCE PATH HANDLING (right side)
          // -----------------------------
          if (typeof rule.resource === 'string' && this.isPath(rule.resource)) {
            const resourcePath = rule.resource;

            // env.* справа
            if (resourcePath.startsWith('env.')) {
              const envPath = resourcePath.replace(/^env\./, '');

              if (!environmentStructure[action]) {
                environmentStructure[action] = {};
              }

              const existingEnvType = environmentStructure[action][envPath];
              const targetType = ruleType; // или 'unknown', если хочешь жёстко

              if (existingEnvType && existingEnvType !== targetType) {
                environmentStructure[action][envPath] = `${existingEnvType} | ${targetType}`;
              } else {
                environmentStructure[action][envPath] = targetType;
              }
            } else {
              // обычный ресурс справа
              if (!resorceStructure[action]) {
                resorceStructure[action] = {};
              }

              const existingResType = resorceStructure[action][resourcePath];
              const targetType = ruleType; // или 'unknown'

              if (existingResType && existingResType !== targetType) {
                resorceStructure[action][resourcePath] = `${existingResType} | ${targetType}`;
              } else {
                resorceStructure[action][resourcePath] = targetType;
              }
            }
          }
        });
      });
    });

    const filteredStructure: Record<string, Record<string, string>> = {};
    Object.entries(resorceStructure).forEach(([action, fields]) => {
      if (!action.endsWith('.*')) {
        filteredStructure[action] = fields;
      }
    });

    // Transform flat structure into nested structure for easier use
    const nestedStructure = this.buildNestedStructure(filteredStructure);
    const nestedEnvironment = this.buildNestedStructure(environmentStructure);

    return this.formatTypeDefinitions(nestedStructure, nestedEnvironment, allTags);
  }

  private isPath(value: unknown) : boolean {

    if (typeof value !== 'string') {
      return false;
    }

    if (value.startsWith('"') || value.startsWith("'")) {
      return false;
    }

    return value.includes('.');
  }

  /**
   * Determines TypeScript type based on the rule
   * @param rule - The rule to analyze
   * @returns TypeScript type as string
   */
  private determineTypeFromRule(rule: AbilityRule): string | null {

    if (rule.condition === AbilityCondition.never || rule.condition === AbilityCondition.always) {
      return null;
    }

    if (
      rule.condition === AbilityCondition.contains ||
      rule.condition === AbilityCondition.not_contains
    ) {
      return this.getArrayType(rule.resource);
    }

    if (
      rule.condition === AbilityCondition.length_equals ||
      rule.condition === AbilityCondition.length_greater_than ||
      rule.condition === AbilityCondition.length_less_than
    ) {
      return 'string | readonly unknown[]';
    }

    // Numeric comparisons - always number
    if (
      rule.condition === AbilityCondition.greater_than ||
      rule.condition === AbilityCondition.greater_or_equal ||
      rule.condition === AbilityCondition.less_than ||
      rule.condition === AbilityCondition.less_or_equal
    ) {
      return 'number';
    }

    // Array operations
    if (rule.condition === AbilityCondition.in || rule.condition === AbilityCondition.not_in) {
      return this.getInArrayType(rule.resource);
    }

    // Equality/Inequality operations
    if (
      rule.condition === AbilityCondition.equals ||
      rule.condition === AbilityCondition.not_equals
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
    const elementType = this.getInArrayType(resource);
    return `readonly ${elementType}[]`;
  }

  private getInArrayType(resource: unknown): string {
    if (Array.isArray(resource)) {
      if (resource.length === 0) {
        return 'unknown';
      }

      // Determine types of array elements
      const elementTypes = new Set(resource.map(item => this.getPrimitiveType(item)));

      return elementTypes.size === 1
        ? Array.from(elementTypes)[0]
        : `(${Array.from(elementTypes).join(' | ')})`;
    }

    // If resource is not an array but condition is in/not_in,
    // it expects an array of such elements
    return this.getPrimitiveType(resource);
  }

  /**
   * Gets primitive TypeScript type for a value
   * @param value - The value to analyze
   * @returns TypeScript primitive type as string
   */
  private getPrimitiveType(value: unknown): string {
    if (value === null) {
      return 'null | unknown';
    }
    if (value === undefined) {
      return 'undefined';
    }

    if (typeof value === 'string' && this.isPath(value)) {
      // This is not a string literal, but a path to another field.
      return 'unknown';
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
   * @param environment
   * @param allTags
   * @returns Formatted TypeScript type definition string
   */
  private formatTypeDefinitions(
    structure: Record<string, NestedDict<string>>,
    environment: NestedDict<string>,
    allTags: Set<string>,
  ): string {
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
        output += '  } | null | undefined;\n';
      }
    });

    output += '}\n';

    // tags
    const tagsUnion =
      allTags.size > 0
        ? Array.from(allTags)
            .sort()
            .map(tag => `'${tag}'`)
            .join(' | ')
        : 'never';
    output += `\n\nexport type PolicyTags = ${tagsUnion};\n`;

    // environments
    output += '\n\nexport type Environment = {\n';

    Object.entries(environment).forEach(([action, envObj]) => {
      const isEmpty = Object.keys(envObj).length === 0;

      if (isEmpty) {
        output += `  ['${action}']: undefined;\n`;
      } else {
        output += `  ['${action}']: {\n`;
        output += this.formatNestedObject(envObj as any, 4);
        output += '  } | null | undefined;\n';
      }
    });

    output += '}\n';

    // complex
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
        output += `${spaces}} | null | undefined;\n`;
      } else {
        // Primitive type
        const va = [String(value)];
        let v = String(value);
        if (!v.match(/unknown/)) {
          if (!v.match(/null/)) {
            va.push('null');
          }

          if (!v.match(/undefined/)) {
            va.push('undefined');
          }
        }


        output += `${spaces}readonly ${key}: ${va.join(' | ')} \n`;
      }
    });

    return output;
  }
}

export default AbilityTypeGenerator;
