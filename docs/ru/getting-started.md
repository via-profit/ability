# Введение

`@via-profit/ability` — лёгкий ABAC-движок (Attribute-Based Access Control) с простым DSL, генерацией TypeScript-типов
и нулевыми зависимостями. Работает одинаково на сервере и в браузере.

Проект закрывает типовые сценарии контроля доступа без лишних сложностей: правила описываются текстом, проверяются за
микросекунды, а при отказе можно получить подробное объяснение, какое условие не выполнилось.

## Установка

```bash
npm install @via-profit/ability
```

## Быстрый старт

```ts
import { ability, AbilityResolver, DenyOverridesStrategy } from '@via-profit/ability';

// Описываем политики на DSL
const policies = ability`
  @name "Читать документ может автор или любой, если документ опубликован"
  permit permission.document.read if any:
    document.author is equals user.id
    document.status in ["published", "archived"]
`;

// Создаём резолвер один раз на всё приложение
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Проверяем доступ: при отказе будет выброшена ошибка AbilityError
resolver.enforce('document.read', { document, user });
```

## Основные понятия

- **Политика** — правило вида `permit` / `deny` для ключа разрешения (например, `permission.document.read`) с набором
  условий. Подробнее — в разделе [DSL](./dsl.md).
- **Правило** — атомарное условие: `document.author is equals user.id`. Слева всегда путь к полю, справа — литерал в
  кавычках или путь без кавычек.
- **Ресурс и окружение** — данные, которые передаются при проверке: ресурс (`{ document, user }`) и окружение
  (`env.*`: время, IP-адрес и т. п.).
- **Резолвер** — выбирает политики по ключу, проверяет их и отдаёт итог стратегии. Подробнее — в разделе
  [Резолвер](./resolver.md).
- **Стратегия** — решает, что делать, если совпало несколько политик. Подробнее — в разделе
  [Стратегии](./strategies.md).

## Что дальше

- [Язык политик (DSL)](./dsl.md) — синтаксис политик, групп, правил и операторов.
- [Резолвер](./resolver.md) — режимы `enforce` и `resolve`, коллбэки, теги и explain.
- [Генерация типов](./types-generator.md) — TypeScript-типы для ресурсов и окружения.
- [Сервер и клиент](./server-and-client.md) — как передать политики в браузер и использовать их в React.
