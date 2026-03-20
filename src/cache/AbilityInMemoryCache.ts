import { AbilityCacheAdapter } from '~/cache/AbilityCacheAdapter';

export class AbilityInMemoryCache implements AbilityCacheAdapter {
  private store = new Map<string, { value: unknown; expires: number }>();

  public async get<T>(key: string): Promise<T | undefined> {
    const entry = this.store.get(key);
    if (!entry) return undefined;

    if (Date.now() > entry.expires) {
      this.store.delete(key);
      return undefined;
    }
    return entry.value as T;
  }

  public async set<T>(key: string, value: T, ttlSeconds = 60): Promise<void> {
    this.store.set(key, {
      value,
      expires: Date.now() + ttlSeconds * 1000,
    });
  }

  public serialize(input: unknown): string {
    return this.fastHash(this.stableStringify(input));
  }

  public async delete(key: string): Promise<void> {
    this.store.delete(key);
  }

  public async clear(): Promise<void> {
    this.store.clear();
  }

  public async deleteByPrefix(prefix: string): Promise<void> {
    for (const key of this.store.keys()) {
      if (key.startsWith(prefix)) {
        this.store.delete(key);
      }
    }
  }

  private fastHash(str: string): string {
    let hash = 5381;
    for (let i = 0; i < str.length; i++) {
      hash = (hash * 33) ^ str.charCodeAt(i);
    }
    return (hash >>> 0).toString(36);
  }

  private stableStringify(obj: unknown): string {
    if (obj === null) return 'null';

    const type = typeof obj;

    if (type === 'string') return JSON.stringify(obj);
    if (type === 'number' || type === 'boolean') return String(obj);
    if (type === 'undefined') return 'undefined';

    if (Array.isArray(obj)) {
      let out = '[';
      for (let i = 0; i < obj.length; i++) {
        if (i > 0) out += ',';
        out += this.stableStringify(obj[i]);
      }
      return out + ']';
    }

    const keys = Object.keys(obj as Record<string, unknown>);
    keys.sort();

    let out = '{';
    for (let i = 0; i < keys.length; i++) {
      const k = keys[i];
      if (i > 0) out += ',';
      out += k + ':' + this.stableStringify((obj as never)[k]);
    }
    return out + '}';
  }
}
