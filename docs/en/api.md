# API Reference

---

[🇬🇧 English](api.md) | [🇷🇺 Русская версия](/docs/ru/api.md)


## AbilityRule

### Purpose

The smallest unit of logic — a single comparison condition.

### Core Properties

| Property | Type | Description |
|----------|------|-------------|
| `id` | `string` | Rule identifier |
| `name` | `string` | Name |
| `subject` | `string` | Path to the subject field (dot‑notation) |
| `resource` | `string \| number \| boolean \| null \| (string \| number \| boolean \| null)[]` | Value or path to the resource |
| `condition` | `AbilityCondition` | Comparison operator |
| `state` | `AbilityMatch` | Current state after checking |

### Methods

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Checks the rule |
| `static fromJSON(config)` | `AbilityRuleConfig` | `AbilityRule` | Creates a rule from a JSON configuration |
| `static equals(subject, resource)` | `string, any` | `AbilityRule` | Equality rule |
| `static notEquals(subject, resource)` | `string, any` | `AbilityRule` | Inequality rule |
| `static contains(subject, resource)` | `string, any` | `AbilityRule` | Checks for inclusion |
| `static notContains(subject, resource)` | `string, any` | `AbilityRule` | Checks for non-inclusion |
| `static in(subject, resource)` | `string, any` | `AbilityRule` | Checks membership in an array |
| `static notIn(subject, resource)` | `string, any` | `AbilityRule` | Checks non-membership in an array |
| `static lessThan(subject, resource)` | `string, any` | `AbilityRule` | Less than |
| `static lessOrEqual(subject, resource)` | `string, any` | `AbilityRule` | Less than or equal |
| `static moreThan(subject, resource)` | `string, any` | `AbilityRule` | Greater than |
| `static moreOrEqual(subject, resource)` | `string, any` | `AbilityRule` | Greater than or equal |

> **Note:** To serialize a rule to JSON, use `AbilityJSONParser.ruleToJSON(rule)`.

---

## AbilityRuleSet

### Purpose

A group of rules with `and` or `or` logic.

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `id` | `string` | Identifier |
| `name` | `string` | Name |
| `compareMethod` | `AbilityCompare` | Comparison logic |
| `rules` | `AbilityRule[]` | List of rules |
| `state` | `AbilityMatch` | Check result of the group |

### Methods

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `addRule(rule)` | `AbilityRule` | `this` | Adds a rule |
| `addRules(list)` | `AbilityRule[]` | `this` | Adds multiple rules |
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Checks the group |
| `static fromJSON(config)` | `AbilityRuleSetConfig` | `AbilityRuleSet` | Creates a group from JSON |
| `static and(rules)` | `AbilityRule[]` | `AbilityRuleSet` | Group with `and` logic |
| `static or(rules)` | `AbilityRule[]` | `AbilityRuleSet` | Group with `or` logic |

> **Note:** To serialize a group, use `AbilityJSONParser.ruleSetToJSON(ruleSet)`.

---

## AbilityPolicy

### Purpose

Combines rule groups and defines the effect when a match occurs.

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `id` | `string` | Identifier |
| `name` | `string` | Name |
| `permission` | `string` | Permission key (supports `*`) |
| `effect` | `AbilityPolicyEffect` | `permit` or `deny` |
| `compareMethod` | `AbilityCompare` | Logic for comparing groups |
| `ruleSet` | `AbilityRuleSet[]` | Rule groups |
| `matchState` | `AbilityMatch` | Check result of the policy |

### Methods

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Checks the policy |
| `explain()` | — | `AbilityExplainPolicy` | Explanation of the result (after `check`) |
| `addRuleSet(ruleSet)` | `AbilityRuleSet` | `this` | Adds a rule group |
| `addRuleSets(ruleSets)` | `AbilityRuleSet[]` | `this` | Adds multiple rule groups |
| `toJSON()` | — | `AbilityPolicyConfig` | Exports to JSON |
| `static fromJSON(config)` | `AbilityPolicyConfig` | `AbilityPolicy` | Creates a policy from JSON |
| `static fromJSONAll(configs)` | `AbilityPolicyConfig[]` | `AbilityPolicy[]` | Bulk creation from JSON |
| `static fromDSL(dsl)` | `string` | `AbilityPolicy` | Creates a policy from DSL |

---

## AbilityResolver

### Purpose

Applies policies to a permission key and a resource, producing the final result.

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `policies` | `readonly AbilityPolicy[]` | List of policies |

### Methods

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `resolve(permission, resource, environment?)` | `string, any, object?` | `Promise<AbilityResult>` | Soft check |
| `enforce(permission, resource, environment?)` | `string, any, object?` | `Promise<void>` | Strict check, throws `AbilityError` on deny |
| `invalidatePolicy(policyId)` | `string` | `Promise<void>` | Invalidates cache for the specified policy |
| `invalidateCache()` | — | `Promise<void>` | Completely clears the cache |
| `static isInPermissionContain(permissionA, permissionB)` | `string, string` | `boolean` | Checks pattern match (with `*`) |

---

## AbilityResult

### Purpose

Encapsulates the result of applying policies.

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `policies` | `readonly AbilityPolicy[]` | Checked policies |

### Methods

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `explain()` | — | `readonly AbilityExplain[]` | Explanations for all policies |
| `getLastMatchedPolicy()` | — | `AbilityPolicy \| null` | The last matched policy |
| `isAllowed()` | — | `boolean` | Access is allowed (no deny or allowed by default) |
| `isDenied()` | — | `boolean` | Access is denied |
| `getLastEffectOfMatchedPolicy()` | — | `AbilityPolicyEffect \| null` | Effect of the last matched policy |

---

## AbilityExplain

### Purpose

Base interface for explanations.

Typically you work with:

- `AbilityExplainPolicy`
- `AbilityExplainRuleSet`
- `AbilityExplainRule`

Common method:

| Method | Description |
|--------|-------------|
| `toString(indent = 0)` | Returns a human-readable description with indentation |

---

## AbilityParser

### Purpose

Utility for generating TypeScript types based on policies.

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `static generateTypeDefs(policies)` | `AbilityPolicy[]` | `string` | Generates the `Resources` type based on all policies |

---

## AbilityJSONParser

### Purpose

Parser and serializer for the JSON representation of policies, groups, and rules.

| Method | Arguments | Returns | Description |
|--------|-----------|---------|-------------|
| `parse(configs)` | `AbilityPolicyConfig[]` | `AbilityPolicy[]` | Creates an array of policies from JSON |
| `parsePolicy(config)` | `AbilityPolicyConfig` | `AbilityPolicy` | Creates a policy from JSON |
| `parseRuleSet(config)` | `AbilityRuleSetConfig` | `AbilityRuleSet` | Creates a group from JSON |
| `parseRule(config)` | `AbilityRuleConfig` | `AbilityRule` | Creates a rule from JSON |
| `ruleToJSON(rule)` | `AbilityRule` | `AbilityRuleConfig` | Exports a rule to JSON |
| `ruleSetToJSON(ruleSet)` | `AbilityRuleSet` | `AbilityRuleSetConfig` | Exports a group to JSON |
| `policyToJSON(policy)` | `AbilityPolicy` | `AbilityPolicyConfig` | Exports a policy to JSON |
| `toJSON(policies)` | `AbilityPolicy[]` | `AbilityPolicyConfig[]` | Exports an array of policies to JSON |

---

## AbilityMatch

Check states:

- `pending`
- `match`
- `mismatch`

Methods:

| Method | Description |
|--------|-------------|
| `isEqual(other)` | Checks equality of states |

---

## AbilityCompare

Comparison methods:

- `and` — all elements must match
- `or` — at least one match is sufficient

---

## AbilityCondition

Core operations (code and literal name):

| Code | Literal | Description |
|------|---------|-------------|
| `=` | `equals` | Equals |
| `<>` | `not_equals` | Not equals |
| `>` | `greater_than` | Greater than |
| `<` | `less_than` | Less than |
| `>=` | `greater_or_equal` | Greater than or equal |
| `<=` | `less_or_equal` | Less than or equal |
| `in` | `in` | Is in array |
| `not in` | `not_in` | Is not in array |
| `contains` | `contains` | Contains (for arrays) |
| `not contains` | `not_contains` | Does not contain (for arrays) |
| `length greater than` | `length_greater_than` | Length is greater than |
| `length less than` | `length_less_than` | Length is less than |
| `length equals` | `length_equals` | Length equals |

---

## AbilityPolicyEffect

Policy effects:

- `permit` — allow
- `deny` — deny

---

## AbilityError

Exception thrown by `enforce()`.

Example:

```ts
try {
  await resolver.enforce('order.update', resource);
} catch (error) {
  if (error instanceof AbilityError) {
    console.error('Access denied by policy:', error.message);
  } else {
    throw error;
  }
}
```

---

## AbilityCache

### AbilityCacheAdapter

Cache adapter interface. Allows connecting any storage: Redis, Memcached, KeyDB, in-memory, etc.

```ts
export interface AbilityCacheAdapter {
  get<T = unknown>(key: string): Promise<T | undefined>;
  set<T = unknown>(key: string, value: T, ttlSeconds?: number): Promise<void>;
  delete?(key: string): Promise<void>;
  clear?(): Promise<void>;
}
```

### AbilityInMemoryCache

Built-in in-memory cache implementation.

```ts
import { AbilityInMemoryCache } from '@via-profit/ability';

const cache = new AbilityInMemoryCache({
  ttl: 60000, // time to live in milliseconds (default 60000)
});
```

### AbilityRedisCache

Redis adapter.

```ts
import { AbilityRedisCache } from '@via-profit/ability';
import Redis from 'ioredis';

const redis = new Redis();
const cache = new AbilityRedisCache(redis, {
  ttl: 60000, // time to live in milliseconds
  prefix: 'ability:', // key prefix
});
```
