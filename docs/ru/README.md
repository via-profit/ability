# @via-profit/Ability

> Набор сервисов, частично реализующих принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)
> Пакет позволяет описывать правила, объединять их в группы, формировать политики и применять их к данным для определения разрешений.

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

## Для чего

Проект задумывался для того, чтобы закрыть типовые сценарии контроля доступа без лишних сложностей. Нам потребовался лёгкий ABAC-движок с простым DSL, автоматической генерацией TypeScript-типов — и без внешних зависимостей.

---

# @via-profit/ability

**Гибкий ABAC (Attribute-Based Access Control) движок для TypeScript/JavaScript.**  
Позволяет декларативно описывать политики доступа через встроенный DSL или JSON, комбинировать правила с помощью `all`/`any`, использовать различные стратегии принятия решений и автоматически генерировать TypeScript-типы для ресурсов.


## Основные возможности

1. Скорость — данные подготавливаются до проверки, нет асинхронных операций внутри движка.
2. Простой и выразительный DSL — правила читаются как естественный язык, поддержка группировки all of: / any of:, оператор except для описания исключений.
3. 9 встроенных стратегий — DenyOverrides, PermitOverrides, FirstMatch, Priority и другие. Возможно добавление собственных стратегий.
4. Кроссплатформенность — работает в Node.js и браузере.
5. TypeScript-first — автоматическая генерация типов ресурсов из политик.
6. Ноль зависимостей — лёгкий вес и отсутствие внешних библиотек.
7. Встроенный explain — дерево решений с результатами каждого правила для отладки.
8. Сериализация — экспорт и импорт политик в JSON.
9. Генерация DSL из политик — обратное преобразование (в разработке).
10. Лёгкая интеграция — библиотека, а не сервис; работает в браузере и Node.js.



## Установка

```bash
npm install @via-profit/ability
# или
yarn add @via-profit/ability
# или
pnpm add @via-profit/ability
```

## Краткий пример

```typescript
import { AbilityDSLParser, AbilityResolver, DenyOverridesStrategy, DenyOverridesStrategy } from '@via-profit/ability';

// DSL-описание политики
const dsl = `
  permit permission.document.read if all of:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;

// Парсим политики
const parser = new AbilityDSLParser(dsl);
const policies = parser.parse();

// Создаём резолвер (по умолчанию стратегия DenyOverrides)
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Данные для проверки
const resource = { ownerId: 123, status: 'published' };
const user = { id: 123 };

// Проверяем разрешение
const result = resolver.resolve('document.read', resource, user);
console.log(result.isAllowed()); // true

// Детализация результатов
console.log(result.explain());
```

## Основные понятия

| Компонент       | Описание                                                                 |
|----------------|--------------------------------------------------------------------------|
| `AbilityPolicy` | Политика – имеет `effect` (permit/deny), `permission` и набор правил.    |
| `AbilityRuleSet`| Группа правил, объединённых оператором `all` (AND) или `any` (OR).       |
| `AbilityRule`   | Элементарное правило: `subject` `оператор` `resource`.                   |
| `AbilityCondition` | Оператор сравнения: `=`, `<>`, `>`, `contains`, `in`, `length >` и др. |
| `AbilityMatch`  | Состояние проверки: `match`, `mismatch`, `pending`, `except-mismatch`.   |
| `AbilityStrategy` | Алгоритм выбора финального эффекта из множества сработавших политик.   |

## DSL синтаксис

### Базовая структура

```
<effect> <permission> if <all|any>:
    <all|any> of: <subject> <operator> <value>
    <all|any> of: <subject> <operator> <value>
    ...
```

- `effect` – `permit` или `deny`
- `permission` – ключ разрешения с префиксом `permission.` (например, `permission.order.update`)
- `all` / `any` – логический оператор для группы правил

### Примеры правил

```ruby
# Простое правило
user.role equals "admin"

# Сравнение с числом
user.age >= 18

# Проверка вхождения в массив
user.status in ["active", "verified"]

# Работа с длиной массива/строки
length user.roles > 2

# Проверка на null
user.deletedAt is null

# Отрицание
user.banned not equals true
```

### Группы и исключения

```ruby
permit permission.article.edit if all of:
    article.authorId equals user.id
    any of:
        article.status equals "draft"
        user.role equals "editor"

except any of:
    user.banned is true
```

### Аннотации

```ruby
@name "Высокий приоритет"
@priority 100
@disabled true
deny permission.admin.all if always:
```

## Стратегии разрешения

| Стратегия                     | Поведение                                                       |
|-------------------------------|-----------------------------------------------------------------|
| `DenyOverridesStrategy`       | Если есть хотя бы одна `deny` → `deny`, иначе `permit` (по умолч.) |
| `PermitOverridesStrategy`     | Если есть хотя бы одна `permit` → `permit`, иначе `deny`         |
| `FirstMatchStrategy`          | Результат первой сработавшей политики                           |
| `SequentialLastMatchStrategy` | Результат последней сработавшей политики                        |
| `PriorityStrategy`            | Выбирает политику с наибольшим `priority`                       |
| `AllMustPermitStrategy`       | `permit` только если **все** сработавшие политики – `permit`     |
| `OnlyOneApplicableStrategy`   | `deny`, если сработало более одной политики                     |
| `AnyPermitStrategy`           | `permit`, если есть хотя бы одна `permit`                       |

Пример использования стратегии:

```typescript
import { PriorityStrategy } from '@via-profit/ability';

const resolver = new AbilityResolver(policies, PriorityStrategy);
```

## Генератор TypeScript-типов

Автоматически создаёт тип `Resources` на основе всех правил в политиках.

```typescript
import { AbilityTypeGenerator } from '@via-profit/ability';

const generator = new AbilityTypeGenerator(policies);
const typeDefs = generator.generateTypeDefs();

// Вывод: export type Resources = {
//   ['document.read']: { readonly ownerId: number; readonly status: string; };
//   ...
// }
```

Полученные типы можно использовать для строгой типизации ресурсов при вызове `resolver.resolve()`.

## API Reference (кратко)

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
// или статические фабрики:
AbilityRule.equals('user.id', 123)
AbilityRule.contains('tags', 'admin')
```

### `AbilityResolver`

```typescript
const resolver = new AbilityResolver(policies, strategy?);
resolver.resolve(permission, resource, environment) -> AbilityResult
resolver.enforce(permission, resource, environment) // бросает ошибку при deny
```

### `AbilityResult`

```typescript
result.isAllowed()   // boolean
result.isDenied()    // boolean
result.explain()     // AbilityExplain[]
```

## Положения

Пакет не выполняет асинхронных операций. Подготовка данных для проверки — ответственность вызывающего кода. Это делает поведение движка детерминированным и легко тестируемым.



## Лицензия

MIT

---

## Ссылки

- [GitHub репозиторий](https://github.com/via-profit/ability)
- [Примеры использования](./examples) (добавить по необходимости)
- [Полная документация API](./docs) (опционально)
