# @via-profit/Ability

> Набор сервисов, частично реализующих принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)
> Позволяет описывать правила, объединять их в группы, формировать политики и применять их к данным для определения
> разрешений.

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

## Для чего

Проект задумывался для того, чтобы закрыть типовые сценарии контроля доступа без лишних сложностей. Нам потребовался лёгкий ABAC-движок с простым DSL, автоматической генерацией TypeScript-типов — и без внешних зависимостей.


- [Основные возможности](#основные-возможности)
- [Установка](#установка)
- [Быстрый старт](#быстрый-старт)
- [Основные понятия](#основные-понятия)
- [DSL](./dsl.md)


## Основные возможности

1. Простой и выразительный DSL — правила читаются как естественный язык.
2. Поддержка группировки правил all of: / any of:
3. Оператор except для описания исключений внутри политики.
4. 9 встроенных стратегий — DenyOverrides, PermitOverrides, FirstMatch, Priority и другие. Возможно добавление
   собственных стратегий.
5. Кроссплатформенность — работает в Node.js и браузере.
6. TypeScript-first — автоматическая генерация типов ресурсов из политик.
7. Ноль зависимостей — лёгкий вес и отсутствие внешних библиотек.
8. Встроенный explain — дерево решений с результатами каждого правила для отладки.
9. Сериализация — экспорт и импорт политик в JSON.


## Установка

```bash
npm install @via-profit/ability
```

## Быстрый старт

```typescript
import {
  AbilityDSLParser,
  AbilityResolver,
  DenyOverridesStrategy,
  DenyOverridesStrategy,
  ability
} from '@via-profit/ability';

const policies = ability`
  @name Разрешено чтение только
  permit permission.document.read if all:
    document.ownerId equals 123
    document.status in ["published", "archived"]
`;

const resolver = new AbilityResolver(policies, DenyOverridesStrategy);


// Проверяем разрешение
const result = resolver.resolve('document.read', {
  document: {
    ownerId: 123,
    status: 'published',
  },
});

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

## DSL

Ability DSL — это декларативный язык для описания политик доступа.  
Он позволяет описывать правила в человекочитаемом виде, а затем использовать их в рантайме для принятия решений.

Ability поддерживает два способа создания политик из DSL:

1. **Через DSL literal**
2. **Через обычную строку + AbilityDSLParser**

### Создание политик через DSL literal

```ts
import { ability } from '@via-profit/ability';

const policies = ability`
  permit permission.document.read if all:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;
```

- строка внутри `ability``…`` парсится AbilityDSLParser
- возвращается массив политик (`AbilityPolicy[]`)

### Создание политик через обычную строку

Если literal недоступен (например, в динамическом окружении):

```ts
import { AbilityDSLParser } from '@via-profit-ability';

const dsl = `
  permit permission.document.read if all:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;

const policies = new AbilityDSLParser(dsl).parse();
```

Оба способа дают одинаковый результат.

### Типизация DSL через дженерики

DSL literal может принимать типы:

```ts
const policies = ability<Resources, Environment, PolicyTags>`
  permit permission.document.read if all:
    document.ownerId equals user.id
    document.status in ["published", "archived"]
`;
```

### Что дают эти типы:

| Тип           | Описание                                                        |
|---------------|-----------------------------------------------------------------|
| `Resources`   | Тип ресурса, доступного в DSL (`document.*`, `order.*`, и т.д.) |
| `Environment` | Тип environment‑данных (`env.time.*`, `env.user.*`)             |
| `PolicyTags`  | Типы тегов политик (если используются)                          |

### Генерация типов из политик

Ability может автоматически генерировать типы на основе политик:

```ts
import { AbilityTypeGenerator } from '@via-profit/ability';

const typeDefs = new AbilityTypeGenerator(policies).generateTypeDefs();

fs.writeFileSync('types.gen.ts', typeDefs, { encoding: 'utf-8' });
```

Это создаёт файл:

```
types.gen.ts
```

В нём будут:

```ts
export type Resources = { ... };
export type Environment = { ... };
export type PolicyTags = "myTag1" | "myTag2" |
...
;
```

### Использование сгенерированных типов

После генерации типов:

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

Теперь:

- `document.ownerId` и `document.status` проверяется на существование и типы
- `document.read` проверяется на корректность
- операторы (`equals`, `in` и т.д.) проверяются на совместимость типов

### Базовая структура

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

- `comment-line` - комментарий
- `annotation` - аннотация (`id`, `name`, `diasbled`, `tags`, `priority`)
- `effect` – `permit` или `deny`
- `permission` – ключ разрешения с префиксом `permission.` (например, `permission.order.update`)
- `all` / `any` – логический оператор для группы правил
- `except` - начало блока исключений

### Правило

Правило - это простейшая структура, которая описывает что с чем и как сравнивается.

Каждое правило должно начинаться с пути ресурса (dot-notation), далее идет оператор сравнения и значение, с которым
будет сравниваться ресурс

*Структура правил*:

```
<subject> <operator> <value|resource|env>
```

```
# Простое правило
user.role equals "admin"

# Сравнение с числом
user.age >= 18

# Проверка вхождения в массив
user.status in ["active", "verified"]

# Работа с длиной массива/строки
user.roles length greater than 2

# Проверка на null
user.deletedAt is null

# Отрицание
user.banned not equals true
```

### Группы и исключения

Группа правил - это блок, содержащий одно или более правил.

```
permit permission.article.edit if all of:
    article.authorId equals user.id
    any of:
        article.status equals "draft"
        user.role equals "editor"

except any of:
    user.banned is true
```

### Аннотации

```
@name "Высокий приоритет"
@description "Описание"
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
- [DSL](./dsl.md)
- [Примеры использования](./examples) (todo: examples)
