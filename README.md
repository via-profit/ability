```markdown
# @via-profit/Ability

> A set of services that partially implement the [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control) principle.
> Allows you to describe rules, combine them into groups, form policies, and apply them to data to determine permissions.

![npm version](https://img.shields.io/npm/v/%40via-profit/ability)
![npm downloads](https://img.shields.io/npm/dm/%40via-profit/ability)
![license](https://img.shields.io/github/license/via-profit/ability)
![TypeScript](https://img.shields.io/badge/TypeScript-Ready-blue)
![status](https://img.shields.io/badge/status-active-success)
![issues](https://img.shields.io/github/issues/via-profit/ability)
![stars](https://img.shields.io/github/stars/via-profit/ability?style=social)

## Language / Язык

[//]: # (- [🇬🇧 English]&#40;/docs/en/README.md&#41;)
- [🇷🇺 Русский](/docs/ru/README.md)

## Purpose

The project was designed to cover typical access control scenarios without unnecessary complexity. We needed a lightweight ABAC engine with a simple DSL, automatic TypeScript type generation — and no external dependencies.


- [Key Features](#key-features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Core Concepts](#core-concepts)
- [DSL](./dsl.md)


## Key Features

1. Simple and expressive DSL — rules read like natural language.
2. Support for grouping rules with `all of:` / `any of:`.
3. `except` operator for describing exceptions within a policy.
4. 9 built-in strategies — DenyOverrides, PermitOverrides, FirstMatch, Priority, and others. Custom strategies can be added.
5. Cross-platform — works in Node.js and browsers.
6. TypeScript-first — automatic type generation for resources from policies.
7. Zero dependencies — lightweight and no external libraries.
8. Built-in explain — decision tree with results of each rule for debugging.
9. Serialization — export and import policies to/from JSON.


## Installation

```bash
npm install @via-profit/ability
```

## Quick Start

```typescript
import {
  AbilityResolver,
  DenyOverridesStrategy,
  ability
} from '@via-profit/ability';

const policies = ability`
  @name "Read access is allowed only for user with ID 123, if the document is published and today is Saturday"
  permit permission.document.read if all:
    
    @name "User ID is 123"
    document.ownerId equals 123
    
    @name "Document is published"
    document.status in ["published", "archived"]
    
    @name "Today must be Saturday"
    env.today.dayName is 'Saturday'
`;

const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

const environment = {
  today: {
    dayName: new Date().toLocaleDateString('en-US', { weekday: 'long' }),
  },
}

// Check the permission
const result = resolver.resolve('document.read', {
  document: {
    ownerId: 123,
    status: 'published',
  },
}, environment);

console.log(result.isAllowed()); // true

// Detailed results
console.log(result.explain());
```

## Core Concepts

| Component       | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| `AbilityPolicy` | A policy – has an `effect` (permit/deny), a `permission`, and a set of rules. |
| `AbilityRuleSet`| A group of rules combined with `all` (AND) or `any` (OR) operator.          |
| `AbilityRule`   | An elementary rule: `subject` `operator` `resource`.                        |
| `AbilityCondition` | Comparison operator: `=`, `<>`, `>`, `contains`, `in`, `length >`, etc. |
| `AbilityMatch`  | Check state: `match`, `mismatch`, `pending`, `except-mismatch`.             |
| `AbilityStrategy` | Algorithm for selecting the final effect from multiple matched policies.  |

## DSL

Ability DSL is a declarative language for describing access policies.  
It allows you to describe rules in a human-readable form and then use them at runtime to make decisions.

Ability supports two ways to create policies from DSL:

1. **Using DSL literal**
2. **Using a regular string + AbilityDSLParser**

### Creating Policies via DSL Literal

```ts
import { ability } from '@via-profit/ability';

const policies = ability`
  permit permission.document.read if all:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;
```

- the string inside `ability``…`` is parsed by AbilityDSLParser
- returns an array of policies (`AbilityPolicy[]`)

### Creating Policies via Regular String

If the literal is not available (e.g., in a dynamic environment):

```ts
import { AbilityDSLParser } from '@via-profit/ability';

const dsl = `
  permit permission.document.read if all:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;

const policies = new AbilityDSLParser(dsl).parse();
```

Both methods produce the same result.

### Typing DSL with Generics

The DSL literal can accept types:

```ts
const policies = ability<Resources, Environment, PolicyTags>`
  permit permission.document.read if all:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;
```

### What These Types Provide:

| Type           | Description                                                        |
|---------------|--------------------------------------------------------------------|
| `Resources`   | Type of resources available in DSL (`document.*`, `order.*`, etc.) |
| `Environment` | Type of environment data (`env.time.*`, `env.user.*`)              |
| `PolicyTags`  | Types of policy tags (if used)                                     |

### Generating Types from Policies

Ability can automatically generate types based on policies:

```ts
import { AbilityTypeGenerator } from '@via-profit/ability';

const typeDefs = new AbilityTypeGenerator(policies).generateTypeDefs();

fs.writeFileSync('types.gen.ts', typeDefs, { encoding: 'utf-8' });
```

This creates a file:

```
types.gen.ts
```

It will contain:

```ts
export type Resources = { ... };
export type Environment = { ... };
export type PolicyTags = "myTag1" | "myTag2" |
...
;
```

### Using Generated Types

After generating types:

```ts
import { ability, AbilityResolver, DenyOverridesStrategy } from '@via-profit/ability';
import type { Resources, Environment, PolicyTags } from './types.gen';

const policies = ability<Resources, Environment, PolicyTags>`
  permit permission.document.read if all:
    document.ownerId equals '1'
    document.status in ["published", "archived"]
`;
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

resolver.enforce('document.read', {
  document: {
    ownerId: 1, // ❌  Type number is not assignable to type string
    status: 'published'
  },
});

```

Now:

- `document.ownerId` and `document.status` are checked for existence and types
- `document.read` is validated for correctness
- operators (`equals`, `in`, etc.) are checked for type compatibility

### Basic Structure

```
# <comment-line>
@<annotation> <annotation-value>
<effect> <permission> if <all|any>:
    <all|any> of: <subject> <operator> <value|resource|env>
    <all|any> of: <subject> <operator> <value|resource|env>
    ...
    except <all|any> of:
      <subject> <operator> <value|resource|env>
      <subject> <operator> <value|resource|env>
      ...
```

- `comment-line` - comment
- `annotation` - annotation (`id`, `name`, `disabled`, `tags`, `priority`)
- `effect` – `permit` or `deny`
- `permission` – permission key with `permission.` prefix (e.g., `permission.order.update`)
- `all` / `any` – logical operator for the rule group
- `except` - start of the exception block

### Rule

A rule is the simplest structure that describes what is compared with what and how.

Each rule must start with a resource path (dot-notation), followed by a comparison operator and the value to compare against.

*Rule structure*:

```
<subject> <operator> <value|resource|env>
```

```
# Simple rule
user.role equals "admin"

# Comparison with a number
user.age >= 18

# Array membership check
user.status in ["active", "verified"]

# Working with array/string length
user.roles length greater than 2

# Null check
user.deletedAt is null

# Undefined check
user.middleName is defined

# Negation
user.banned not equals true
```

### Groups and Exceptions

A rule group is a block containing one or more rules.

```
permit permission.article.edit if all of:
    article.authorId equals user.id
    any of:
        article.status equals "draft"
        user.role equals "editor"

except any of:
    user.banned is true
```

### Annotations

```
@name "High priority"
@description "Description"
@priority 100
@disabled true
deny permission.admin.all if all:
  always
```

## Resolution Strategies

| Strategy                    | Behavior                                                        |
|-----------------------------|-----------------------------------------------------------------|
| `DenyOverridesStrategy`     | If there is at least one `deny` → `deny`, otherwise `permit` (default) |
| `PermitOverridesStrategy`   | If there is at least one `permit` → `permit`, otherwise `deny`  |
| `FirstMatchStrategy`        | Result of the first matching policy                             |
| `SequentialLastMatchStrategy` | Result of the last matching policy                            |
| `PriorityStrategy`          | Selects the policy with the highest `priority`                 |
| `AllMustPermitStrategy`     | `permit` only if **all** matching policies are `permit`        |
| `OnlyOneApplicableStrategy` | `deny` if more than one policy matches                         |
| `AnyPermitStrategy`         | `permit` if there is at least one `permit`                     |

Example of using a strategy:

```typescript
import { PriorityStrategy } from '@via-profit/ability';

const resolver = new AbilityResolver(policies, PriorityStrategy);
```

## TypeScript Type Generator

Automatically creates a `Resources` type based on all rules in the policies.

```typescript
import { AbilityTypeGenerator } from '@via-profit/ability';

const generator = new AbilityTypeGenerator(policies);
const typeDefs = generator.generateTypeDefs();

// Output: export type Resources = {
//   ['document.read']: { readonly ownerId: number; readonly status: string; };
//   ...
// }
```

The generated types can be used for strict typing of resources when calling `resolver.resolve()`.

## API Reference

### `AbilityPolicy`

```typescript
new AbilityPolicy({ id, name, permission, effect, compareMethod, priority })
  .addRuleSet(ruleSet)
  .check(resource, environment)   // -> AbilityMatch
  .explain()                      // -> AbilityExplain
```

### `AbilityRule`

```typescript
new AbilityRule({ subject, resource, condition })
// or static factories:
AbilityRule.equals('user.id', 123)
AbilityRule.contains('tags', 'admin')
```

### `AbilityResolver`

```typescript
const resolver = new AbilityResolver(policies, strategy?);
resolver.resolve(permission, resource, environment) -> AbilityResult
resolver.enforce(permission, resource, environment) // throws an error on deny
```

### `AbilityResult`

```typescript
result.isAllowed()   // boolean
result.isDenied()    // boolean
result.explain()     // AbilityExplain[]
```

## Principles

The package does not perform asynchronous operations. Preparing data for verification is the responsibility of the calling code. This makes the engine's behavior deterministic and easy to test.

## License

MIT

---

## Links

- [GitHub repository](https://github.com/via-profit/ability)
- [DSL](./dsl.md)
