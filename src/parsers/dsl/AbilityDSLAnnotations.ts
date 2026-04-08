import AbilityCode from '../../core/AbilityCode';
import { AbilityDSLToken } from './AbilityDSLToken';

// -------------------------------------------------------------------------
// #region Owner
// -------------------------------------------------------------------------
type AbilityDSLAnnotationOwnerType = 'policy' | 'ruleSet' | 'rule';
export class AbilityDSLAnnotationOwner extends AbilityCode<AbilityDSLAnnotationOwnerType> {
  public static policy = new AbilityDSLAnnotationOwner('policy');
  public static ruleSet = new AbilityDSLAnnotationOwner('ruleSet');
  public static rule = new AbilityDSLAnnotationOwner('rule');
}

// -------------------------------------------------------------------------
// #region Name + value type mapping
// -------------------------------------------------------------------------
export type AbilityDSLAnnotationNameType =
  | 'id'
  | 'name'
  | 'priority'
  | 'description'
  | 'disabled'
  | 'tags';

export type AnnotationValueMap = {
  id: string;
  name: string;
  priority: number;
  description: string;
  disabled: boolean;
  tags: readonly string[];
};

export class AbilityDSLAnnotationName extends AbilityCode<AbilityDSLAnnotationNameType> {
  public static id = new AbilityDSLAnnotationName('id');
  public static name = new AbilityDSLAnnotationName('name');
  public static priority = new AbilityDSLAnnotationName('priority');
  public static description = new AbilityDSLAnnotationName('description');
  public static disabled = new AbilityDSLAnnotationName('disabled');
  public static tags = new AbilityDSLAnnotationName('tags');

  // Helper to get the Name instance from a key
  public static fromKey<K extends keyof AnnotationValueMap>(key: K): AbilityDSLAnnotationName {
    return this[key];
  }
}

// -------------------------------------------------------------------------
// #region Annotation class (generic)
// -------------------------------------------------------------------------
export class AbilityDSLAnnotation<T extends AnnotationValueMap[keyof AnnotationValueMap]> {
  public name: AbilityDSLAnnotationName;
  public value: T | null;
  public token: AbilityDSLToken | null;

  public constructor(
    name: AbilityDSLAnnotationName,
    value: T | null,
    token: AbilityDSLToken | null,
  ) {
    this.name = name;
    this.value = value;
    this.token = token;
  }

  // Single factory method for all annotation types
  public static create<K extends keyof AnnotationValueMap>(
    key: K,
    value: AnnotationValueMap[K] | null,
    token: AbilityDSLToken | null,
  ): AbilityDSLAnnotation<AnnotationValueMap[K]> {
    return new AbilityDSLAnnotation(AbilityDSLAnnotationName.fromKey(key), value, token);
  }
}

// -------------------------------------------------------------------------
// #region Annotations container
// -------------------------------------------------------------------------
export class AbilityDSLAnnotations {
  private store = new Map<AbilityDSLAnnotationName, AbilityDSLAnnotation<any>>();

  // Generic getter
  public get<K extends keyof AnnotationValueMap>(
    key: K,
  ): AbilityDSLAnnotation<AnnotationValueMap[K]> | null {
    const name = AbilityDSLAnnotationName.fromKey(key);
    const ann = this.store.get(name);
    return (ann as AbilityDSLAnnotation<AnnotationValueMap[K]>) ?? null;
  }

  // Generic setter (null removes)
  public set<K extends keyof AnnotationValueMap>(
    key: K,
    value: AnnotationValueMap[K] | null,
    token: AbilityDSLToken | null,
  ): this {
    const name = AbilityDSLAnnotationName.fromKey(key);
    if (value !== null) {
      this.store.set(name, AbilityDSLAnnotation.create(key, value, token));
    } else {
      this.store.delete(name);
    }
    return this;
  }

  // Convenience properties (optional, for backward compatibility / nicer API)
  get id() {
    return this.get('id');
  }
  get name() {
    return this.get('name');
  }
  get description() {
    return this.get('description');
  }
  get priority() {
    return this.get('priority');
  }
  get disabled() {
    return this.get('disabled');
  }
  get tags() {
    return this.get('tags');
  }

  // Fluent setters (optional)
  setID(value: string, token: AbilityDSLToken) {
    return this.set('id', value, token);
  }
  setName(value: string, token: AbilityDSLToken) {
    return this.set('name', value, token);
  }
  setDescription(value: string, token: AbilityDSLToken) {
    return this.set('description', value, token);
  }
  setPriority(value: number, token: AbilityDSLToken) {
    return this.set('priority', value, token);
  }
  setDisabled(value: boolean, token: AbilityDSLToken) {
    return this.set('disabled', value, token);
  }
  setTags(value: readonly string[], token: AbilityDSLToken) {
    return this.set('tags', value, token);
  }

  public clear(): void {
    this.store.clear();
  }

  public clone(): AbilityDSLAnnotations {
    const cloned = new AbilityDSLAnnotations();
    for (const [name, ann] of this.store) {
      cloned.store.set(name, new AbilityDSLAnnotation(ann.name, ann.value, ann.token));
    }
    return cloned;
  }
}
