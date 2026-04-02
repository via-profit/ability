# @via-profit/ability

> A set of services partially implementing the [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control) principle.
> The package allows you to describe rules, combine them into groups, form policies, and apply them to data to determine permissions.

![npm version](https://img.shields.io/npm/v/%40via-profit/ability)
![npm downloads](https://img.shields.io/npm/dm/%40via-profit/ability)
![license](https://img.shields.io/github/license/via-profit/ability)
![TypeScript](https://img.shields.io/badge/TypeScript-Ready-blue)
![status](https://img.shields.io/badge/status-active-success)
![issues](https://img.shields.io/github/issues/via-profit/ability)
![stars](https://img.shields.io/github/stars/via-profit/ability?style=social)

## Language / Язык

- [🇬🇧 English](/docs/en/README.md)
- [🇷🇺 Русский](/docs/ru/README.md)

## Purpose

The package is intended as a **lightweight and extremely simple alternative** to heavy access control systems.
No complex configurations, no dependencies – just a minimal set of tools that allows you to describe rules and policies in a very simple DSL.

Unlike classic ABAC models, where policies work on the principle *"matched → applied, didn't match → ignored"*, Ability uses a **simplified and more predictable state‑machine model**:

- **all** matching policies are executed in the order they are declared,
- each policy can **set** a state (`permit` or `deny`),
- each policy can **reset** the state if its conditions are not met,
- the final result is determined by the **last processed policy**, not just the one that matched.

## Contents

- [Quick start](#quick-start)
- [Key concepts](#key-concepts)
- [DSL](#dsl)
- [Combining policies](#combining-policies)
- [Policy Environment](#policy-environment)
- [TypeScript type generator](#typescript-type-generator)
- [Debugging policies](#debugging-policies)
- [Troubleshooting](#troubleshooting)
- [Design recommendations](#design-recommendations)
- [Examples](#examples)
- [Performance](#performance)
- [Api-Reference](./api.md)

## Quick start

Install the package, write DSL, call the parser, run the resolver.

### Installation

```bash
npm install @via-profit/ability
```

```bash
yarn add @via-profit/ability
```

```bash
pnpm add @via-profit/ability
```

### Example: deny access to `passwordHash` to everyone except the owner

Suppose we have user data:

```ts
const user = {
  id: '1',
  login: 'user-001',
  passwordHash: '...',
};
```

We need to deny reading `passwordHash` to everyone except the user themselves.

#### DSL policy

In the policy language, this looks like:

```
deny permission.user.passwordHash if any:
  viewer.id is not equals owner.id
```

**Explanation:**

- `deny` – policy effect (deny access)
- `permission.user.passwordHash` – permission key.
- `if any:` – start of conditions block
- `viewer.id is not equals owner.id` – rule: if the requester's ID does not equal the owner's ID

If `viewer.id` does not equal `owner.id`, the rule is considered satisfied, and the policy returns `deny` – access denied. If the IDs match (i.e., the user requests their own data), the rule does not trigger, and access is allowed.

_Note: The permission key is formed as `permission.` + your custom key in **dot notation**, e.g., the key `foo.bar.baz` in DSL would be `permission.foo.bar.baz`_

#### Code check

```ts
import { AbilityDSLParser, AbilityResolver } from '@via-profit/ability';

const dsl = `
deny permission.user.passwordHash if any:
  viewer.id is not equals owner.id
`;

const policies = new AbilityDSLParser(dsl).parse(); // get policies
const resolver = new AbilityResolver(policies); // create resolver

resolver.enforce('user.passwordHash', {
  viewer: { id: '1' },
  owner: { id: '2' },
}); // will throw an error – access denied
```
In `enforce`, the key is passed without the `permission.` prefix – it is automatically removed by the parser.

## Key concepts

Let's list the key points you need to know before starting to use the package:

1. **The resolver (`AbilityResolver`) works on the `Default Deny` principle.**
   If no policy has set a final state, the result will be `deny`
   ([see here](#troubleshooting)).
   To avoid unexpected `deny`, ensure there is at least one `permit` policy that can match.
   Only then add `deny` policies.

2. **Policies are processed sequentially, from top to bottom.**
   Unlike classic ABAC models, where a `mismatch` is simply ignored, Ability uses a **state‑machine model**:

- `match` sets the state (`permit` → allow, `deny` → deny),
- `mismatch` **resets the state to neutral**,
- the final result is determined by the **last processed policy**, not just the one that matched.

3. **Rules within a policy are also executed sequentially.**

4. **In a rule set with the `all` operator, execution stops at the first `mismatch`.**
   In `any` – at the first `match`.

5. **Use [DSL](#dsl) to compose policies** – it's simpler and more convenient.

6. **Use JSON to store policies on the server.**
   Policies can be exported to JSON and imported back.

7. **Follow the principle: if permission is not explicitly granted → access is denied.**
   This is a natural consequence of the `Default Deny` model and state‑machine behavior.

### Interaction model

First, you describe "raw" policies (SDL, JSON, or using classes). Then, from the "raw" data, you form ready policies (an array of policies). This is done once and allows you to have a single source of truth. Then you can run permission checks in the necessary parts of your code using the already prepared policies and resolver.

Policies, groups, and rules can be created using:

- DSL (Domain-Specific Language)
- Classes (classic approach)
- JSON

**Creating policies using DSL**

```ts
import { AbilityDSLParser } from '@via-profit/ability';

// Describe policies in Ability-DSL language
const dsl = `
  # @name Creating an order is available only to persons over 18 years old
  permit permission.order.action.create if all:
    all of:
      user.age gte 18

  # @name Editing the price is available only to the administrator
  permit permission.order.data.price if all:
    all of:
      user.roles contains 'administrator'
`;

// Define resource types for TypeScript
// Types can be generated automatically (more on that later), or described manually
// In this example, for simplicity, types are described manually
type Resources = {
  ['order.action.create']: {
    user: {
      age: number;
    }
  }
  ['order.data.price']: {
    user: {
      roles: string[];
    }
  }
}

// Use the parser to create policies
// Pass the resource type as a generic
const policies = new AbilityDSLParser<Resources>(dsl).parse(); // AbilityPolicy[]

// The parser will return an array of policies even
// if only one policy is described in the DSL
console.log(policies); // [AbilityPolicy, AbilityPolicy, ...]

// export ready policies
export default policies;
```

For more details on DSL, see the section (DSL)[#dsl]

**Creating policies using classes (classic approach)**

This approach is quite verbose, but gives you full control over policies

```ts
import { AbilityPolicy, AbilityRuleSet, AbilityRule, AbilityCompare, AbilityPolicyEffect } from '@via-profit/ability';

// Define resource types for TypeScript
// Types can be generated automatically (more on that later), or described manually
// In this example, for simplicity, types are described manually
type Resources = {
  ['order.action.create']: {
    user: {
      age: number;
    }
  }
  ['order.data.price']: {
    user: {
      roles: string[];
    }
  }
}

const policies = [
  // first policy
  new AbilityPolicy<Resources>({
    id: '1',
    name: 'Creating an order is available only to persons over 18 years old',
    compareMethod: AbilityCompare.and,
    effect: AbilityPolicyEffect.permit,
    permission: 'order.action.create',
  }).addRuleSet(
    AbilityRuleSet.and([
      // rule
      AbilityRule.moreOrEqual('user.age', 18),
    ]),
  ),

  // second policy
  new AbilityPolicy<Resources>({
    id: '2',
    name: 'Editing the price is available only to the administrator',
    compareMethod: AbilityCompare.and,
    effect: AbilityPolicyEffect.permit,
    permission: 'order.data.price',
  }).addRuleSet(
    AbilityRuleSet.and([
      // rule
      AbilityRule.contains('user.roles', 'administrator'),
    ])
  ),
];

// export ready policies
export default policies;
```

**Creating policies using JSON**

JSON allows you to store policies in a file or database, for example, in PostgreSQL, which supports working with JSON data.

Policy, group, and rule classes have methods to export to JSON, so you can form policies in any way and export them to JSON whenever you need it

```ts
import { AbilityJSONParser } from '@via-profit/ability';

// Define resource types for TypeScript
// Types can be generated automatically (more on that later), or described manually
// In this example, for simplicity, types are described manually
type Resources = {
  ['order.action.create']: {
    user: {
      age: number;
    }
  }
  ['order.data.price']: {
    user: {
      roles: string[];
    }
  }
}

// Parse JSON using AbilityJSONParser
// Pass the resource types as a generic
const policies = AbilityJSONParser.parse<Resources>([
  {
    id: '1',
    name: 'Creating an order is available only to persons over 18 years old',
    effect: 'permit',
    permission: 'order.action.create',
    compareMethod: 'and',
    ruleSet: [
      {
        compareMethod: 'and',
        rules: [
          {
            subject: 'user.age',
            resource: 18,
            condition: '>',
          }
        ]
      }
    ],
  },
  {
    id: '2',
    name: 'Editing the price is available only to the administrator',
    effect: 'permit',
    permission: 'order.data.price',
    compareMethod: 'and',
    ruleSet: [
      {
        compareMethod: 'and',
        rules: [
          {
            subject: 'user.roles',
            resource: 'administrator',
            condition: 'contains',
          }
        ]
      }
    ]
  }
]);

export default policies;
```

---

## DSL

> DSL - Domain-Specific Language

Ability DSL is a declarative language for describing access policies.
It allows you to define rules in a human-readable form using simple constructs: *policies*, *groups*, *rules*, and *annotations*.

### Policy structure

A policy consists of the following construct:

```
<effect> <permission> if <all|any>:
  <group>...
```

Where:

- **effect** – `permit` or `deny`
- **permission** – a string like `permission.foo.bar`
  (the `permission.` prefix is required in DSL but automatically removed by the parser)
- **if all:** – all groups must be true
- **if any:** – at least one group must be true
- a policy can contain one or more rule groups

**Example**

```dsl
permit permission.order.update if any:
  all of:
    user.roles contains 'admin'
    user.token is not null

  any of:
    user.roles contains 'developer'
    user.login is equals 'dev'
```

This policy means:

The `permission.order.update` permission will be granted if **at least one** of the two groups is satisfied:

1. `user.roles` contains `'admin'` **and** `user.token` is not `null`
2. `user.roles` contains `'developer'` **or** `user.login` equals `'dev'`

If multiple policies match the key, they are **all executed**, top to bottom.

Each policy:

- **match**  
  → sets a new state (`permit` → allow, `deny` → deny)

- **mismatch**  
  → **resets the state** to `neutral`  
  (i.e., cancels the result of the previous policy)

The final decision is determined by the **last processed policy**, not just the one that matched.

This means:

- a policy can **override** the previous one
- a policy can **cancel** the previous one (via mismatch)
- the order of policies in DSL is critically important

### Permission key

Permission keys are written in `dot notation` and support wildcard patterns using the `*` symbol.
This allows grouping permissions and overriding behavior for entire families of operations.

**How policy matching works**

If multiple policies match the key, **all policies are executed in order**, top to bottom.
The final decision is determined by the **last state** set during processing.

This means:

- a policy can **override** the result of the previous one
- a policy can **cancel** the result of the previous one (via mismatch)
- the order of policies in DSL is critically important

### Example of using wildcards

| Policy (permission) | key                    | Matches |
|---------------------|------------------------|---------|
| `order.*`           | `order.create`         | yes     |
| `order.*`           | `order.update`         | yes     |
| `order.*`           | `user.create`          | no      |
| `*.create`          | `order.create`         | yes     |
| `*.create`          | `user.create`          | yes     |
| `*.create`          | `order.update`         | no      |
| `user.profile.*`    | `user.profile.update`  | yes     |
| `user.profile.*`    | `user.settings.update` | no      |

### Example policy with wildcard

```ts
import { AbilityDSLParser, AbilityResolver } from '@via-profit/ability';

// DSL is incomplete and shown only for example
const dsl = `
permit permission.order.*
deny permission.order.update
`;

const policies = new AbilityDSLParser(dsl).parse();
const resolver = new AbilityResolver(policies);

resolver.enforce('order.update', resource); // will throw AbilityError
```

---

**Explanation**

The order of policies in DSL determines the final decision.

Processing goes top to bottom:

1. `permit permission.order.*`
- match → state = `allow`

2. `deny permission.order.update`
- match → state = `deny`
- the final state overwrites the previous one

Result:

```
order.update → deny
order.create → allow
order.delete → allow
order.view   → allow
```

### Comments

Lines starting with the `#` symbol are considered comments and do not affect the result of rules and policies.

---

### Annotations

Currently, only one annotation is supported – ’name’, which will be used as the name for the policy, rule group, or rule.

Annotations are set via comments:

```
# @name <name>
```

Annotations apply to the **next entity**:

- policy
- group
- rule

Example:

```dsl
# @name can order update
permit permission.order.update if any:
  # @name authorized admin
  all of:
    # @name contains role admin
    user.roles contains 'admin'
```

---

### Rule groups

A group defines how rules are combined within it:

```
all of:
  <rule>
  <rule>

any of:
  <rule>
  <rule>
```

- `all of:` – logical AND
- `any of:` – logical OR

`all of` – means the group is considered satisfied if all rules within the group matched.

`any of` – means the group is considered satisfied if at least one rule within the group matched.

Each group inside a policy will be evaluated independently of other groups. The final result will be determined by comparing the evaluation results of all groups in the policy.

Groups can have annotations:

```dsl
# @name developer group
any of:
  user.roles contains 'developer'
```

---

### Rules

A rule is an atomic condition inside a policy. It defines under what data the policy will be considered matching. Rules are used to set conditions that determine the policy's effect (`permit` or `deny`).

A rule has the form:

```
<subject> <operator> <value?> — value is not specified for all operators (e.g., is null does not require a value).
```

#### Subject

Identifier in dot notation:

```
user.roles
env.time.hour
order.total
```

#### Operators

_Synonyms are alternative forms of notation that are also supported by the parser._

**Basic comparison operators**

| DSL Operator | Synonyms | Example | Description | Types |
|--------------|----------|--------|----------|------|
| **is equals** | `=`, `==`, `equals` | `age is equals 18` | Strict equality | number, string, boolean |
| **is not equals** | `!=`, `<>`, `not equals` | `role is not equals 'admin'` | Strict inequality | number, string, boolean |
| **greater than** | `>`, `gt` | `age greater than 18` | Greater than | number, date |
| **greater than or equal** | `>=`, `gte` | `age greater than or equal 18` | Greater than or equal | number, date |
| **less than** | `<`, `lt` | `age less than 18` | Less than | number, date |
| **less than or equal** | `<=`, `lte` | `age less than or equal 18` | Less than or equal | number, date |

**Null operators**

| DSL Operator | Synonyms | Example | Description | Types |
|--------------|----------|--------|----------|------|
| **is null** | `== null`, `= null` | `middleName is null` | Value is absent | any |
| **is not null** | `!= null` | `middleName is not null` | Value is present | any |

**Operators for lists (arrays)**

| DSL Operator | Synonyms                  | Example | Description | Types |
|--------------|---------------------------|--------|----------|------|
| **in [...]** | -                         | `role in ['admin', 'manager']` | Value is in the list | number, string |
| **not in [...]** | -                         | `role not in ['banned']` | Value is not in the list | number, string |
| **contains** | `includes`, `has`         | `tags contains 'vip'` | Array contains element | array |
| **not contains** | `not includes`, `not has` | `tags not contains 'vip'` | Array does not contain element | array |

**Boolean operators**

| DSL Operator | Synonyms | Example | Description | Types |
|--------------|----------|--------|----------|------|
| **is true** | `= true` | `isActive is true` | Value is true | boolean |
| **is false** | `= false` | `isActive is false` | Value is false | boolean |

**Length operators**

| DSL Operator | Synonyms | Example | Description | Types |
|--------------|----------|--------|----------|------|
| **length equals** | `len =` | `tags length equals 3` | Length equals | array, string |
| **length greater than** | `len >` | `tags length greater than 2` | Length greater than | array, string |
| **length less than** | `len <` | `tags length less than 5` | Length less than | array, string |

**Special operators**

| DSL Operator | Synonyms | Example | Description | Types |
|--------------|----------|--------|----------|------|
| **always** | — | `always` | Condition always true. Used for global permission or simplifying logic. | special operator |
| **never** | — | `never` | Condition always false. Used for global denial or disabling a rule. | special operator |

**always**
An operator that always returns `true`.
Used for:

- global permission (`permit permission.* if all: always`)
- testing
- disabling complex conditions
- creating fallback rules

**never**
An operator that always returns `false`.
Used for:

- global denial (`deny permission.* if all: never`)
- temporarily disabling a rule
- explicit negation without conditions

#### Value

Supported:

- strings `'text'`
- numbers `42`
- booleans `true` / `false`
- `null`
- arrays `[1, 2, 3]` / `['foo', false, null, 1, 2, '999']`

Examples:

```dsl
# user age greater than 18
user.age greater than 18

# array of roles contains role 'admin'
user.roles contains 'admin'

# order tag is either 'vip' or 'priority'
order.tag in ['vip', 'priority']

# user token is not null
user.token is not null

# user login is longer than 12 characters
user.login length greater than 12
```

---

### Implicit group

If rules are written without `all of:` or `any of:`, they are combined by the policy operator:

```dsl
permit permission.order.update if all:
  user.roles contains 'admin'
  user.token is not null
```

Equivalent to:

```dsl
permit permission.order.update if all:
  all of:
    user.roles contains 'admin'
    user.token is not null
```

The implicit group always matches the policy operator (`if all` or `if any`).

---

### Full example

```dsl
# @name order update allowed
permit permission.order.update if any:

  # @name if this is admin
  all of:
    user.roles contains 'admin'
    user.token is not null

  # @name if this is developer
  any of:
    user.roles contains 'developer'
    user.login is equals 'dev'
```

## Combining policies

In a real project, you should use several policies at once.

TODO: using multiple policies

## Policy Environment

**Environment** is an object containing environment data that does not belong to either the user or the resource.
The content of the object is defined by the developer and can be any object composed of primitives.

- request time,
- IP address,
- device parameters,
- request headers,
- session context,
- any other external conditions.

The environment is passed to `resolve()` and `enforce()` as the third argument:

```ts
const environment = {
  time: {
    hour: new Date().getHours(),
  },
  ip: req.ip,
}

resolver.enforce('order.update', resource, environment);
```

### Using environment in rules

In a policy, you can refer to the environment via the path `env.*`.

Example policy that denies order updates at night (10 PM – 6 AM):

```dsl
# @name Deny updates at night
deny permission.order.update if all:
  env.time.hour less than 6 
  env.time.hour greater or equal than 22
```

**Retrieving values from environment**

If the rule specifies a path:

- `env.*` → value is taken from environment
- `user.*`, `order.*`, `profile.*` → from resource
- literal (`18`, `"admin"`, `true`) → used as is

Example:

```ts
subject: "env.geo.country"
resource: "user.country"
condition: "equal"
```

### Environment in TypeScript

The Environment type is specified at the `AbilityResolver` level:

```ts
const resolver = new AbilityResolver<Resources, Environment>(policies);
```

This allows:

- getting autocompletion in the IDE,
- checking the correctness of `env.*` paths,
- avoiding errors when passing the environment.

> If a rule uses `env.*` but the environment is not passed, the `env.*` value will be `undefined`, and the comparison will be performed as if the environment were not present at all.

## TypeScript type generator

`AbilityTypeGenerator.generateTypeDefs(policies)` generates types for TypeScript based on policies, allowing you not to worry about discrepancies between types and data in policies.

**Usage example**

Policies can be stored in DSL or JSON. This example uses a DSL file.

_policies/policies.dsl_
```
# @name Update order
permit permission.order.update if all:

  # @name Owner check
  all of:
    # @name User is owner
    user.id = order.ownerId
```

_scripts/policies.js_
```js
const fs = require('node:fs');
const path = require('node:path');
const { AbilityTypeGenerator, AbilityDSLParser } = require('@via-profit/ability');

// Prepare paths
const dslPath = path.resolve(__dirname, '../src/policies/policies.dsl');
const typeDefsPath = path.join(path.dirname(dslPath), 'policies.types.ts');

// Read DSL as string
const dsl = fs.readFileSync(dslPath, {encoding: 'utf-8'});

// Create policies
const policies = new AbilityDSLParser(dsl).parse();

// Generate TypeScript types
const typeDefs = new AbilityTypeGenerator(policies).generateTypeDefs();

// Save TypeScript types to file
fs.writeFileSync(typeDefsPath, typeDefs, {encoding: 'utf-8'});
```

_policies/index.ts_
```ts
import { AbilityDSLParser, AbilityResolver } from '@via-profit/ability';
import type { Resources } from './policies.types';
import dsl from './policies.dsl';

const policies = new AbilityDSLParser<Resources>(dsl).parse();

export const policyResolver = new AbilityResolver(new AbilityDSLParser<Resources>(dsl).parse());

export default policyResolver;
```

**Generated file (example)**

```ts
// src/ability/types.generated.ts

// Automatically generated by via-profit/ability
// Do not edit manually
export type Resources = {
  'order.update': {
    readonly user: {
      readonly id: string;
    };
    readonly order: {
      readonly ownerId: string;
    };
  };
};
```

**Usage in code**

```ts
import { policyResolver } from './policies';

resolver.enforce('order.update', {
  user: { id: 'u1' },
  order: { ownerId: 'u1' },
});
```

## Debugging policies

### Explanations

To simplify policy debugging, a special class `AbilityResult` is used, which is already included in the final calculation result. `AbilityResult` encapsulates the result of applying all matching policies to the permission key and resource.

`AbilityResult` contains:

- list of evaluated policies,
- methods to determine the final effect,
- methods to get explanations in text representation.

Example:

```ts
const result = resolver.resolve('order.update', resource);

if (result.isDenied()) {
  console.log('Access denied');
}

const explanations = result.explain(); // AbilityExplain

// console.log(explanations.toString());
```

### AbilityExplain

`AbilityExplain` and related classes (`AbilityExplainPolicy`, `AbilityExplainRuleSet`, `AbilityExplainRule`) allow you to get a human-readable explanation:

- which policy matched,
- which rule groups matched,
- which rules failed,
- what effect was applied.

Usage example:

```ts
const result = resolver.resolve('order.update', resource);
const explanations = result.explain();

console.log(explanations.toString());
```

Example output:

```
✓ policy «Deny order update for managers» is match
  ✓ ruleSet «Managers» is match
    ✓ rule «Department managers» is match
    ✗ rule «Role manager» is mismatch
  ✓ ruleSet «Not administrators» is match
    ✓ rule «No role administrator» is match
```

### Output format

Currently, only one output format is supported – text.

The output is structured as: <policy | ruleSet | rule > <name> <is match | is mismatch>

## Troubleshooting

### Decision model (Default Deny)

> Why doesn't a `deny` policy turn into `permit` if its conditions are not met?

Consider a policy that **denies** access to a user aged 16:

```ts
const dsl = `
deny permission.test if all:
  user.age is equals 16
`;

const policies = new AbilityDSLParser(dsl).parse();
const resolver = new AbilityResolver(policies);

const result = resolver.resolve('test', {
  user: { age: 16 },
});

console.log(result.isDenied());  // true  ✔
console.log(result.isAllowed()); // false ✔
```

In this case, everything is obvious:
condition satisfied → policy matches → effect `deny` → access denied.

**What happens if the conditions are `not satisfied`?**

```ts
const result = resolver.resolve('test', {
  user: { age: 12 },
});

console.log(result.isDenied());  // true  ✔
console.log(result.isAllowed()); // false ✔
```

At first glance, it might seem that if the condition is not met, the policy should "allow" access.
But that's **not the case**.

**Decision model: `Default Deny`**

`AbilityResolver` uses the classic security model:

> **If there is no matching permit policy → access denied.**

**What happens in this example:**

1. The `deny` policy exists, but its condition is **not satisfied**
   → the policy gets `mismatch` status.

2. The `deny` policy **does not apply** because the conditions did not match.

3. There is no `permit` policy.

4. Since there is no granting policy → final decision:
   **deny (by default)**.

**Summary**

- `deny` with matching conditions → **deny**
- `deny` with non-matching conditions → **deny (default deny)**
- `permit` with matching conditions → **allow**
- `permit` with non-matching conditions → **deny (default deny)**

**Conclusion**

**Access is only granted when there is an explicit permit.**

## Design recommendations

### Naming access keys

- Use hierarchical keys: `permission.order.create`, `permission.order.update.status`, `permission.user.profile.update`.
- Group by domains: `permission.user.*`, `permission.order.*`, `permission.product.*`.
- Do not mix different domains in one key.

### Data structure

- Explicitly describe `Resources` in TypeScript.
- Do not pass "extra" fields – this complicates understanding.
- Try to keep the data structure for a single `permission` stable.

### Designing policies

- Common rules – via wildcard (`permission.order.*`).
- Specific restrictions – via exact actions (`permission.order.update`).
- Use `effect: deny` for prohibitions.
- Use `effect: permit` for permissions.

### Typical mistakes

- Expecting that the absence of matching policies means deny.
- Mixing business logic and access policies.
- Too large policies with dozens of rules – it's better to split them.

### Example of use on the frontend (React)

**Hook for checking policies**

```tsx
// hooks/use-ability.ts
import { useEffect, useState } from 'react';
import { AbilityResolver } from '@via-profit/ability';
import { Resources } from './generated-types';

export function useAbility<Permission extends keyof Resources>(
  resolver: AbilityResolver<Resources>,
  permission: Permission,
  resource: Resources[Permission],
) {
  const [allowed, setAllowed] = useState<boolean | null>(null);

  useEffect(() => {
    let cancelled = false;

    async function check() {
      try {
        const result = resolver.resolve(permission, resource);
        if (!cancelled) {
          setAllowed(result.isAllowed());
        }
      } catch {
        if (!cancelled) {
          setAllowed(false);
        }
      }
    }

    check();

    return () => {
      cancelled = true;
    };
  }, [resolver, permission, resource]);

  return allowed;
}
```

**Usage in component**

```tsx
function OrderUpdateButton({ order, user }) {
  const allowed = useAbility(resolver, 'order.update', {
    user,
    order,
  });

  if (allowed === null) {
    return null; // or loading badge
  }

  if (!allowed) {
    return null;
  }

  return <button>Update order</button>;
}
```

## Examples

### Example of a complex multi-stage policy

Below is an example of a set of policies for a cinema.
It demonstrates:

- working with roles (admin, seller, manager, VIP, banned),
- time restrictions (`env.time.hour`),
- wildcard permissions (`permission.*`),
- ticket quantity restrictions,
- prohibition on selling already sold tickets,
- combination of `permit`/`deny`,
- **sequential processing of policies**,
- **state‑machine model**, where each policy can **set or reset the state**.

---

**Unlike classic ABAC systems, where `mismatch` is ignored, Ability uses a `state‑machine` model:**

- **match** → policy sets a state (`allow` or `deny`)
- **mismatch** → policy **resets the state to neutral**
- the final result is determined by the **last processed policy**

This means:

- a policy can **override** the previous one
- a policy can **cancel** the previous one (via mismatch)
- the order of policies in DSL is critically important
- the final decision does not always match the “intuitive” reading of rules from top to bottom

### Brief description of the rules

**Administrator**

- Has wildcard rights (`permission.*`)
- Can edit ticket prices

**Seller**

- Can sell tickets only during working hours (09:00–23:00)
- Cannot sell tickets if:
  - the cinema is closed,
  - the ticket is already sold

**Manager**

- Has the same rights as the seller

**Buyers**

- A user over 21 years old can buy tickets
- A VIP user can buy tickets at any time
- A banned user (`status = banned`) cannot buy tickets
- Any user cannot buy more than 6 tickets

### DSL policies

```dsl
permit permission.ticket.price.edit if all:
  user.role is equals 'admin'

permit permission.ticket.sell if all:
  user.role is equals 'seller'
  all of:
    env.time.hour greater than or equal 9
    env.time.hour less than or equal 23

permit permission.ticket.buy if all:
  user.age greater than 21

permit permission.ticket.buy if all:
  user.isVIP is true

deny permission.ticket.buy if all:
  user.status is equals 'banned'

deny permission.ticket.sell if all:
  any of:
    env.time.hour less than 9
    env.time.hour greater than 23

permit permission.ticket.sell if all:
  user.role is equals 'manager'

permit permission.* if all:
  user.role is equals 'admin'

deny permission.ticket.buy if all:
  user.ticketsCount greater than or equal 6

deny permission.ticket.sell if all:
  ticket.status is equals 'sold'
```

**Example: seller sells a ticket at 3:00 PM**

1. permit seller match → `allow`
2. deny closed mismatch → `neutral`
3. deny sold mismatch → `neutral`

Result: `neutral → deny`

**Example: VIP buys a ticket at night**

1. permit age>21 mismatch → `neutral`
2. permit VIP match → `allow`
3. deny banned mismatch → `neutral`
4. deny limit mismatch → `neutral`

Result: `neutral → deny`

**Example: administrator sells a ticket at night**

1. permit admin wildcard match → `allow`
2. deny closed match → `deny`
3. deny sold mismatch → `neutral`

Result: `neutral → deny`

### Preparing policies

```ts
import { AbilityDSLParser } from '@via-profit/ability';
import cinemaDSL from './policies/cinema.dsl';

export const policies = new AbilityDSLParser(cinemaDSL).parse();
```

### Creating a resolver

```ts
import { AbilityResolver } from '@via-profit/ability';
import { policies } from './policies';

const resolver = new AbilityResolver(policies);
```

### enforce (throws an error on deny)

```ts
resolver.enforce('ticket.buy', {
  user: { age: 25, ticketsCount: 1 },
  env: { time: { hour: 18 } },
});
```

### resolve (without exceptions)

```ts
const result = resolver.resolve('ticket.buy', {
  user: { age: 25, ticketsCount: 1 },
  env: { time: { hour: 18 } },
});

if (result.isAllowed()) {
  console.log('Purchase allowed');
} else {
  console.log('Purchase denied');
}
```

**Preparing data for the resolver**

In the examples above, simple constant objects are passed to the resolver:

```ts
resolver.enforce('ticket.buy', {
  user: { age: 25 },
  env: { time: { hour: 18 } },
});
```

This is done for clarity. In a real application, the data for the resolver should be formed dynamically – from the sources available to your server.

**User** (`user`) is usually taken from:

- JWT token
- session
- database
- authorization middleware

Example:

```ts
const user = await db.users.findById(session.userId);
```

**Environment (`env`)**

These are any external parameters that can affect access:

- current server time
- time zone
- IP address
- request headers
- system configuration

Example:

```ts
const env = {
  time: {
    hour: new Date().getHours(),
  },
  ip: req.ip,
};
```

**Resource** (e.g., `ticket`)

If the action is related to a specific object – it also needs to be loaded:

```ts
const ticket = await db.tickets.findById(req.params.ticketId);
```

**Context**

The context is the object you pass to `resolve` or `enforce`.
It contains **all the data** that policies may need:

- `user` – data about the current user
- `env` – environment data (time, IP, geography, system settings)
- `resource` or `ticket` – data about the entity on which the action is performed
- any other objects you use in the DSL

**Important to understand:**

> The context is formed for a specific action and specific policies. You don't need to store it in advance – you collect it dynamically before calling the resolver.

## Performance

The tests used policies with 10 conditions, nested fields, and environment.

**Tinybench** ([https://github.com/tinylibs/tinybench](https://github.com/tinylibs/tinybench))

| # | Task name                              | Latency avg (ns)      | Latency med (ns)      | Throughput avg (ops/s) | Throughput med (ops/s) | Samples |
|---|-----------------------------------------|------------------------|------------------------|--------------------------|--------------------------|---------|
| 0 | resolve() — no cache (heavy rules)      | 646317 ± 0.32%         | 632319 ± 8446.0        | 1555 ± 0.21%             | 1581 ± 21                | 3095    |
| 1 | resolve() — cold cache (heavy rules)    | 636363 ± 0.38%         | 623092 ± 7885.0        | 1581 ± 0.21%             | 1605 ± 20                | 3143    |
| 2 | resolve() — warm cache (heavy rules)    | 631328 ± 0.26%         | 621152 ± 6562.5        | 1590 ± 0.17%             | 1610 ± 17                | 3168    |

```
Latency (ns)
650k | ███████████████████████████████████████ resolve() — no cache
640k | █████████████████████████████████████ resolve() — cold cache
630k | ████████████████████████████████████ resolve() — warm cache
      --------------------------------------------------------------
        no cache            cold cache            warm cache
```

```
Throughput (ops/s)
1600 | ███████████████████████████████████████ resolve() — warm cache
1590 | ██████████████████████████████████████ resolve() — cold cache
1580 | █████████████████████████████████████ resolve() — no cache
      --------------------------------------------------------------
        no cache            cold cache            warm cache
```

## License

This project is licensed under the MIT License. See the [LICENSE](/LICENSE) file for details.

