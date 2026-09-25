# @via-profit/ability

> Lightweight ABAC engine with DSL, TypeScript types, and zero dependencies

![npm version](https://img.shields.io/npm/v/%40via-profit/ability)
![npm downloads](https://img.shields.io/npm/dm/%40via-profit/ability)
![license](https://img.shields.io/github/license/via-profit/ability)
![TypeScript](https://img.shields.io/badge/TypeScript-Ready-blue)
![status](https://img.shields.io/badge/status-active-success)
![issues](https://img.shields.io/github/issues/via-profit/ability)
![stars](https://img.shields.io/github/stars/via-profit/ability?style=social)

> ### 🌐 Language Selection / Выбор языка
>
> * 🇷🇺 **[Документация на русском](./docs/ru/README.md)**
> * 🇬🇧 **[English documentation](./docs/en/README.md)**

## Why

The project was designed to cover standard access control scenarios without unnecessary complexity. We needed a lightweight ABAC engine that works in both server and browser environments, with a simple DSL, automatic TypeScript type generation, and no external dependencies.

## Key features

- Simple DSL
- 8 built-in resolution strategies
- TypeScript-first with generated types
- Zero dependencies
- Explain for debugging
- Works in both server and browser environments

## Installation

```bash
npm install @via-profit/ability
```

## Quick start

```ts
import { ability, AbilityResolver, DenyOverridesStrategy } from '@via-profit/ability';

// Create policies, including one policy for document.read
const policies = ability`
  @name "Allow reading a document only for authors or if it is published"
  permit permission.document.read if any:

    @name "User is the author"
    document.author is equals user.id
    
    @name "Document is published"
    document.status in ["published", "archived"]
`;

// Create the resolver
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Load your data (your implementation)
const document = await db.loadDocument();
const user = await db.loadUser();

// Check permission (throws AbilityError on deny)
resolver.enforce('document.read', { document, user });
```

## Performance

A permission check takes a few microseconds, so it can be run on every request or every render without caching the results on your side.

_Node.js v24.12.0, AMD Ryzen 7 PRO 3700 8-Core Processor, linux x64_

| Benchmark | ops/sec | avg | p99 |
|---|---:|---:|---:|
| resolve — 21 policies (10 permissions) | 773,568 | 1.3 µs | 2.1 µs |
| resolve — 201 policies (100 permissions) | 657,067 | 1.6 µs | 2.4 µs |
| resolve — 2001 policies (1 000 permissions) | 650,980 | 1.6 µs | 2.5 µs |
| enforce, deny + throw — 201 policies | 82,941 | 12.2 µs | 19.0 µs |
| resolve + explainToString() — 3 policies | 101,116 | 10.0 µs | 14.6 µs |
| resolve — 5 000 policies × 10 rules on one key (stress) | 151 | 6.64 ms | 7.84 ms |
| parse DSL — 201 policies | 339 | 2.95 ms | 3.56 ms |
| parse JSON — 201 policies | 22,306 | 47.1 µs | 83.6 µs |
| generate types — 201 policies | 730 | 1.38 ms | 1.98 ms |

How the policy set is built: for every permission key there is a `permit` policy with two groups (4 rules) and a `deny` policy with an `except` block, plus one global `permit permission.*` policy. Each `resolve` evaluates 3 matching policies and returns a real decision.

What to keep in mind:

- The policies matching a permission key are selected once and cached per key, so the check time practically does not depend on the total number of policies — it depends on the number of policies and rules for the checked key. The cache holds up to 1 024 keys and is cleared when the limit is reached; if every call uses a new key, a check is ~0.5 µs slower.
- `enforce` on deny is slower than `resolve` because it creates and throws an `Error`. If denies are frequent and expected (for example, for hiding buttons in the UI), use `resolve`.
- `explainToString()` builds a text tree — use it for debugging and logging, not in hot paths.
- Parsing DSL and generating types are one-time operations at startup or build time. On the client, use pre-built JSON: it is parsed ~60× faster than DSL.

Run the benchmarks on your machine:

```bash
npm run bench          # table
npm run bench -- --md  # Markdown table
```

## License

Distributed under the MIT license © [Via-Profit](https://via-profit.ru)

You can review the license terms in [LICENSE](./LICENSE).
