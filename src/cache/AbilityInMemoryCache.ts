import { AbilityCacheAdapter } from '~/cache/AbilityCacheAdapter';

export class AbilityInMemoryCache implements AbilityCacheAdapter {
  private store = new Map<string, { value: unknown; expires: number }>();

  public async get<T>(key: string): Promise<T | undefined> {
    const entry = this.store.get(key);
    if (!entry) {
      return undefined;
    }

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

  public serialize<T = unknown>(input: T): string {
    return JSON.stringify(input);
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
}
