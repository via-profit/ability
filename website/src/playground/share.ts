import type { StrategyName } from './evaluate';

export interface PlaygroundState {
  readonly dsl: string;
  readonly context: string;
  readonly strategy: StrategyName;
  readonly permission: string;
}

const STORAGE_KEY = 'ability-playground';
const HASH_PREFIX = '#s=';

const toBase64Url = (text: string) =>
  btoa(String.fromCharCode(...new TextEncoder().encode(text)))
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/, '');

const fromBase64Url = (value: string) => {
  const base64 = value.replace(/-/g, '+').replace(/_/g, '/');
  const bytes = Uint8Array.from(atob(base64), char => char.charCodeAt(0));

  return new TextDecoder().decode(bytes);
};

const isState = (value: unknown): value is Partial<PlaygroundState> =>
  typeof value === 'object' && value !== null && !Array.isArray(value);

/**
 * Link to the playground with the given state (for example, from a code block of the documentation)
 */
export const playgroundLink = (state: Partial<PlaygroundState>) =>
  `/playground${HASH_PREFIX}${toBase64Url(JSON.stringify(state))}`;

export const shareUrl = (state: PlaygroundState) => {
  const { origin, pathname } = window.location;

  return `${origin}${pathname}${HASH_PREFIX}${toBase64Url(JSON.stringify(state))}`;
};

export const readHashState = (hash: string): Partial<PlaygroundState> | null => {
  if (!hash.startsWith(HASH_PREFIX)) {
    return null;
  }

  try {
    const state = JSON.parse(fromBase64Url(hash.slice(HASH_PREFIX.length)));

    return isState(state) ? state : null;
  } catch {
    return null;
  }
};

export const readStoredState = (): Partial<PlaygroundState> | null => {
  try {
    const state = JSON.parse(window.localStorage.getItem(STORAGE_KEY) || 'null');

    return isState(state) ? state : null;
  } catch {
    return null;
  }
};

export const writeStoredState = (state: PlaygroundState) => {
  try {
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
  } catch {
    // storage is not available
  }
};

export const clearStoredState = () => {
  try {
    window.localStorage.removeItem(STORAGE_KEY);
  } catch {
    // storage is not available
  }
};
