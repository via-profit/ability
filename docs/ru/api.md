# API Reference

---

## AbilityRule

### Назначение

Минимальная единица логики — одно условие сравнения.

### Основные свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор правила |
| `name` | `string` | Название |
| `subject` | `string` | Путь к полю субъекта |
| `resource` | `string \| number \| boolean \| (string \| number)[]` | Путь или значение ресурса |
| `condition` | `AbilityCondition` | Оператор сравнения |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет правило |
| `explain()` | — | `AbilityExplainRule` | Объяснение проверки |
| `toJSON()` | — | `AbilityRuleConfig` | Экспорт в JSON |
| `static fromJSON(config)` | `AbilityRuleConfig` | `AbilityRule` | Создание из JSON |
| `static equal(subject, resource)` | `string, any` | `AbilityRule` | Упрощённый конструктор |
| `static notEqual(...)` и др. | | | Аналогично |

---

## AbilityRuleSet

### Назначение

Группа правил с логикой `and` или `or`.

### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор |
| `name` | `string` | Название |
| `compareMethod` | `AbilityCompare` | Логика сравнения |
| `rules` | `AbilityRule[]` | Список правил |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `addRule(rule)` | `AbilityRule` | `this` | Добавляет правило |
| `addRules(list)` | `AbilityRule[]` | `this` | Добавляет несколько |
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет группу |
| `explain()` | — | `AbilityExplainRuleSet` | Объяснение |
| `toJSON()` | — | `AbilityRuleSetConfig` | Экспорт |
| `static fromJSON(config)` | `AbilityRuleSetConfig` | `AbilityRuleSet` | Из JSON |
| `static and(rules)` | `AbilityRule[]` | `AbilityRuleSet` | Группа с `and` |
| `static or(rules)` | `AbilityRule[]` | `AbilityRuleSet` | Группа с `or` |

---

## AbilityPolicy

### Назначение

Объединяет группы правил и определяет эффект при совпадении.

### Свойства

| Свойство | Тип | Описание                           |
|----------|-----|------------------------------------|
| `id` | `string` | Идентификатор                      |
| `name` | `string` | Название                           |
| `permission` | `string` | Ключ разрешения (с поддержкой `*`) |
| `effect` | `AbilityPolicyEffect` | `permit` или `deny`                |
| `compareMethod` | `AbilityCompare` | Логика сравнения групп             |
| `ruleSet` | `AbilityRuleSet[]` | Группы правил                      |
| `matchState` | `AbilityMatch` | Результат проверки                 |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет политику |
| `explain()` | — | `AbilityExplainPolicy` | Объяснение |
| `toJSON()` | — | `AbilityPolicyConfig` | Экспорт |
| `static fromJSON(config)` | `AbilityPolicyConfig` | `AbilityPolicy` | Из JSON |
| `static fromJSONAll(configs)` | `AbilityPolicyConfig[]` | `AbilityPolicy[]` | Массовый парсинг |

---

## AbilityResolver

### Назначение

Применяет политики к ключу разрешения и ресурсу, формирует итоговый результат.

### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `policies` | `AbilityPolicy[]` | Список политик |

### Методы

| Метод                                         | Аргументы | Возвращает | Описание |
|-----------------------------------------------|-----------|------------|----------|
| `resolve(permission, resource, environment?)` | `string, any, object?` | `Promise<AbilityResult>` | Мягкая проверка |
| `enforce(permission, resource, environment?)` | `string, any, object?` | `Promise<void>` | Строгая проверка, выбрасывает `AbilityError` при deny |
| `invalidatePolicy(policyId)`                  | `string` | `Promise<void>` | Инвалидация кэша политики |
| `invalidateCache()`                           | — | `Promise<void>` | Полная очистка кэша |
| `static isInPermissionContain(a, b)`          | `string, string` | `boolean` | Проверка соответствия действия шаблону |

---

## AbilityResult (API)

### Назначение

Инкапсулирует результат применения политик.

### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `policies` | `readonly AbilityPolicy[]` | Проверенные политики |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `explain()` | — | `readonly AbilityExplain[]` | Объяснения по всем политикам |
| `getLastMatchedPolicy()` | — | `AbilityPolicy \| null` | Последняя сработавшая политика |
| `isAllowed()` | — | `boolean` | Итог не deny |
| `isDenied()` | — | `boolean` | Итог deny |
| `getLastEffect()` | — | `AbilityPolicyEffect \| null` | Эффект последней совпавшей политики |

---

## AbilityExplain (API)

### Назначение

Базовый интерфейс для объяснений.

Обычно вы работаете с:

- `AbilityExplainPolicy`
- `AbilityExplainRuleSet`
- `AbilityExplainRule`

Общий метод:

| Метод | Описание |
|-------|----------|
| `toString()` | Возвращает человекочитаемое описание |

---

## AbilityParser

### Назначение

Утилита для работы с конфигурациями и типами.

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `static generateTypeDefs(policies)` | `AbilityPolicy[]` | `string` | Генерация TypeScript-типов `Resources` |

---

## AbilityMatch

Состояния проверки:

- `pending`
- `match`
- `mismatch`

Методы:

| Метод | Описание |
|-------|----------|
| `isEqual(other)` | Проверка равенства состояния |

---

## AbilityCompare

Способы сравнения:

- `and` — все элементы должны совпасть
- `or` — достаточно одного совпадения

---

## AbilityCondition

Основные операции:

- `equal` (`=`)
- `not_equal` (`<>`)
- `more_than` (`>`)
- `less_than` (`<`)
- `more_or_equal` (`>=`)
- `less_or_equal` (`<=`)
- `in`
- `not_in`

---

## AbilityPolicyEffect

Эффекты политики:

- `permit` — разрешить
- `deny` — запретить

---

## AbilityError

Исключение, выбрасываемое при `enforce()`.

Пример:

```ts
try {
  await resolver.enforce('order.update', resource);
} catch (error) {
  if (error instanceof AbilityError) {
    console.error('Доступ запрещён политикой:', error.message);
  } else {
    throw error;
  }
}
```

---

## AbilityCache

### AbilityCacheAdapter

Интерфейс адаптера кэша. Позволяет подключить любое хранилище: Redis, Memcached, KeyDB, in-memory и т.д.

```ts
export interface AbilityCacheAdapter {
  get<T = unknown>(key: string): Promise<T | undefined>;
  set<T = unknown>(key: string, value: T, ttlSeconds?: number): Promise<void>;
  delete?(key: string): Promise<void>;
  clear?(): Promise<void>;
}
```

### AbilityInMemoryCache

Встроенная реализация кэша в памяти.

```ts
import { AbilityInMemoryCache } from '@via-profit/ability';

const cache = new AbilityInMemoryCache({
  ttl: 60000, // время жизни в миллисекундах (по умолчанию 60000)
});
```

### AbilityRedisCache

Адаптер для Redis.

```ts
import { AbilityRedisCache } from '@via-profit/ability';
import Redis from 'ioredis';

const redis = new Redis();
const cache = new AbilityRedisCache(redis, {
  ttl: 60000, // время жизни в миллисекундах
  prefix: 'ability:', // префикс для ключей
});
```

### AbilityJSONParser

JSON парсер, который позволяет создавать правила, группы и политики из JSON и наоборот, преобразовывать из в JSON

```ts
import { AbilityJSONParser } from '@via-profit/ability';

const rule = AbilityRule.fromJSON({
  id: 'test-id',
  name: 'Test Rule',
  subject: 'user.age',
  resource: 18,
  condition: '>' as const,
});

const json = AbilityJSONParser.ruleToJSON(rule); // {"id": "test-id", "name": "Test Rule", "subject": "user.age", "resource": 18, "condition": ">"}

```

