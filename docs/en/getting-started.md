# Introduction

`@via-profit/ability` is a lightweight ABAC (Attribute-Based Access Control) engine with a simple DSL, TypeScript types
generation and zero dependencies. It works the same way on the server and in the browser.

The project covers typical access control scenarios without unnecessary complexity: rules are described as text, checked
in microseconds, and on denial you can get a detailed explanation of which condition failed.

## Philosophy: Intentionally "Dumb" Policies

The core architectural principle of `@via-profit/ability` is simple: **business logic belongs in your application, not in your access control library.**

In many authorization tools, rules gradually get bloated with custom callbacks, async database calls, and complex computations. As a result, business logic leaks into the permission layer, making access checks fragile and hard to debug.

`@via-profit/ability` takes a different approach:
* **No business logic inside the DSL.** The language intentionally omits complex expressions, loops, and custom functions — providing only clear, declarative comparison operators.
* **Pre-computed context only.** Your application is responsible for fetching and preparing the attributes (e.g., resolving `isSubscriptionActive: true`). The policy engine simply evaluates this flat snapshot against your rules.
* **Predictable and trivial to test.** Policies act as pure functions with zero side effects. Testing complex permission matrices requires nothing more than passing a plain JSON object to the resolver.


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
