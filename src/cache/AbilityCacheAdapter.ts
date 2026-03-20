export interface AbilityCacheAdapter {
  get<T = unknown>(key: string): Promise<T | undefined>;
  set<T = unknown>(key: string, value: T, ttlSeconds?: number): Promise<void>;
  serialize<T = unknown>(input: T): string;
  delete(key: string): Promise<void>;
  clear(): Promise<void>;
  deleteByPrefix(prefix: string): Promise<void>;
}
