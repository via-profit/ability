import { AbilityDSLToken } from "./AbilityDSLToken";
export type AnnotationValueMap = {
  id: string;
  name: string;
  priority: number;
  description: string;
  disabled: boolean;
  tags: readonly string[];
};

export type AnnotationName = keyof AnnotationValueMap;

export type AnnotationEntry<K extends AnnotationName> = {
  key: K;
  value: AnnotationValueMap[K];
  token: AbilityDSLToken | null;
};

export class AbilityDSLAnnotations {
  private store: Record<AnnotationName, AnnotationEntry<AnnotationName> | undefined> = {
    id: undefined,
    name: undefined,
    priority: undefined,
    description: undefined,
    disabled: undefined,
    tags: undefined,
  };

  get<K extends AnnotationName>(key: K): AnnotationEntry<K> | null {
    return (this.store[key] as AnnotationEntry<K> | undefined) ?? null;
  }

  set<K extends AnnotationName>(
    key: K,
    value: AnnotationValueMap[K] | null,
    token: AbilityDSLToken | null,
  ): this {
    if (value === null) {
      this.store[key] = undefined;
    } else {
      this.store[key] = {
        key,
        value,
        token,
      } as AnnotationEntry<AnnotationName>;
    }
    return this;
  }

  clear() {
    for (const key of Object.keys(this.store) as AnnotationName[]) {
      this.store[key] = undefined;
    }
  }

  clone(): AbilityDSLAnnotations {
    const cloned = new AbilityDSLAnnotations();
    for (const key of Object.keys(this.store) as AnnotationName[]) {
      const entry = this.store[key];
      cloned.store[key] = entry
        ? { ...entry }
        : undefined;
    }
    return cloned;
  }

  // convenience getters
  get id() { return this.get('id'); }
  get name() { return this.get('name'); }
  get description() { return this.get('description'); }
  get priority() { return this.get('priority'); }
  get disabled() { return this.get('disabled'); }
  get tags() { return this.get('tags'); }

  // convenience setters
  setID(v: string, t: AbilityDSLToken) { return this.set('id', v, t); }
  setName(v: string, t: AbilityDSLToken) { return this.set('name', v, t); }
  setDescription(v: string, t: AbilityDSLToken) { return this.set('description', v, t); }
  setPriority(v: number, t: AbilityDSLToken) { return this.set('priority', v, t); }
  setDisabled(v: boolean, t: AbilityDSLToken) { return this.set('disabled', v, t); }
  setTags(v: readonly string[], t: AbilityDSLToken) { return this.set('tags', v, t); }
}