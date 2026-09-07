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
- 9 built-in resolution strategies
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
  permit permission.document.read if all:
    
    @name "User is the author"
    document.author is equals user.id
    
    @name "Document is published"
    document.status in ["published", "archived"]
`;

// Create the resolver
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Load your data (your implementation)
const document = await db.loadDocument();

// Check permission
resolver.enforce('document.read', { document });
```

## License

Distributed under the MIT license © [Via-Profit](https://via-profit.ru)

You can review the license terms in [LICENSE](./LICENSE).
