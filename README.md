# @via-profit/ability

> Легкий ABAC-движок с DSL, TypeScript-типами и нулевыми зависимостями

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

## Для чего

Проект задумывался для того, чтобы закрыть типовые сценарии контроля доступа без лишних сложностей. Нам потребовался
лёгкий ABAC-движок, работающий и на сервере и в браузере, с простым DSL, автоматической генерацией TypeScript-типов — и
без внешних зависимостей.

## Основные возможности

- Простой DSL
- 9 встроенных стратегий разрешения
- TypeScript-first с генерацией типов
- Ноль зависимостей
- Explain для отладки
- Работает и на сервере и в браузере

## Установка

```bash
npm install @via-profit/ability
```

## Быстрый старт

```ts
import { ability, AbilityResolver, DenyOverridesStrategy } from '@via-profit/ability';

// Создание политик, в частности, одной политики document.read
const policies = ability`
  @name "Разрешить чтение документа только авторам или если он опубликован"
  permit permission.document.read if all:
    
    @name "Пользователь является автором"
    document.author is equals user.id
    
    @name "Документ опубликован"
    document.status in ["published", "archived"]
`;

// Создание резолвера
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Загрузка данных (ваш вариант)
const document = await db.loadDocument();

// Проверка разрешения
resolver.enforce('document.read', { document });
```

## Лицензия

Распространяется под лицензией MIT © [Via-Profit](https://via-profit.ru)

С условиями лицензии можно ознакомиться в файле [LICENSE](./LICENSE).
