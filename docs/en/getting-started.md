# Introduction

`@via-profit/ability` is a lightweight ABAC (Attribute-Based Access Control) engine with a simple DSL, TypeScript types
generation and zero dependencies. It works the same way on the server and in the browser.

The project covers typical access control scenarios without unnecessary complexity: rules are described as text, checked
in microseconds, and on denial you can get a detailed explanation of which condition failed.

## Installation

```bash
npm install @via-profit/ability
```

## Quick start

```ts
import { ability, AbilityResolver, DenyOverridesStrategy } from '@via-profit/ability';

// Describe policies in the DSL
const policies = ability`
  @name "A document can be read by its author or by anyone if it is published"
  permit permission.document.read if any:
    document.author is equals user.id
    document.status in ["published", "archived"]
`;

// Create one resolver for the whole application
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Check access: AbilityError is thrown on denial
resolver.enforce('document.read', { document, user });
```

## Core concepts

- **Policy** — a `permit` / `deny` rule for a permission key (for example, `permission.document.read`) with a set of
  conditions. See [DSL](./dsl.md).
- **Rule** — an atomic condition: `document.author is equals user.id`. The left side is always a path to a field, the
  right side is a quoted literal or an unquoted path.
- **Resource and environment** — the data passed to the check: the resource (`{ document, user }`) and the
  environment (`env.*`: time, IP address and so on).
- **Resolver** — selects policies by the key, checks them and returns the strategy decision. See
  [Resolver](./resolver.md).
- **Strategy** — decides what to do when several policies match. See [Strategies](./strategies.md).

## What's next

- [Policy language (DSL)](./dsl.md) — syntax of policies, groups, rules and operators.
- [Resolver](./resolver.md) — `enforce` and `resolve` modes, callbacks, tags and explain.
- [Types generation](./types-generator.md) — TypeScript types for resources and environment.
- [Server and client](./server-and-client.md) — how to send policies to the browser and use them in React.
