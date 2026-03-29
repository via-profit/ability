# API Reference

---

## Language / Язык

- [🇬🇧 English](/docs/en/api.md)
- [🇷🇺 Русский](/docs/ru/api.md)


## AbilityRule

### Назначение

Минимальная единица логики — одно условие сравнения.

### Основные свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор правила |
| `name` | `string` | Название |
| `subject` | `string` | Путь к полю субъекта (dot‑notation) |
| `resource` | `string \| number \| boolean \| null \| (string \| number \| boolean \| null)[]` | Значение или путь к ресурсу |
| `condition` | `AbilityCondition` | Оператор сравнения |
| `state` | `AbilityMatch` | Текущее состояние после проверки |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет правило |
| `static equals(subject, resource)` | `string, any` | `AbilityRule` | Правило равенства |
| `static notEquals(subject, resource)` | `string, any` | `AbilityRule` | Правило неравенства |
| `static contains(subject, resource)` | `string, any` | `AbilityRule` | Проверка вхождения |
| `static notContains(subject, resource)` | `string, any` | `AbilityRule` | Проверка отсутствия вхождения |
| `static in(subject, resource)` | `string, any` | `AbilityRule` | Проверка принадлежности массиву |
| `static notIn(subject, resource)` | `string, any` | `AbilityRule` | Проверка отсутствия в массиве |
| `static lessThan(subject, resource)` | `string, any` | `AbilityRule` | Меньше |
| `static lessOrEqual(subject, resource)` | `string, any` | `AbilityRule` | Меньше или равно |
| `static moreThan(subject, resource)` | `string, any` | `AbilityRule` | Больше |
| `static moreOrEqual(subject, resource)` | `string, any` | `AbilityRule` | Больше или равно |

> **Примечание:** Для сериализации правила в JSON используйте `AbilityJSONParser.ruleToJSON(rule)`.

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
| `state` | `AbilityMatch` | Результат проверки группы |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `addRule(rule)` | `AbilityRule` | `this` | Добавляет правило |
| `addRules(list)` | `AbilityRule[]` | `this` | Добавляет несколько правил |
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет группу |
| `static and(rules)` | `AbilityRule[]` | `AbilityRuleSet` | Группа с `and` |
| `static or(rules)` | `AbilityRule[]` | `AbilityRuleSet` | Группа с `or` |

> **Примечание:** Для сериализации группы используйте `AbilityJSONParser.ruleSetToJSON(ruleSet)`.

---

## AbilityPolicy

### Назначение

Объединяет группы правил и определяет эффект при совпадении.

### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор |
| `name` | `string` | Название |
| `permission` | `string` | Ключ разрешения (поддерживает `*`) |
| `effect` | `AbilityPolicyEffect` | `permit` или `deny` |
| `compareMethod` | `AbilityCompare` | Логика сравнения групп |
| `ruleSet` | `AbilityRuleSet[]` | Группы правил |
| `matchState` | `AbilityMatch` | Результат проверки политики |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет политику |
| `explain()` | — | `AbilityExplainPolicy` | Объяснение результата (после `check`) |
| `addRuleSet(ruleSet)` | `AbilityRuleSet` | `this` | Добавляет группу правил |
| `addRuleSets(ruleSets)` | `AbilityRuleSet[]` | `this` | Добавляет несколько групп |

---

## AbilityResolver

### Назначение

Применяет политики к ключу разрешения и ресурсу, формирует итоговый результат.

### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `policies` | `readonly AbilityPolicy[]` | Список политик |

### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `resolve(permission, resource, environment?)` | `string, any, object?` | `Promise<AbilityResult>` | Мягкая проверка |
| `enforce(permission, resource, environment?)` | `string, any, object?` | `Promise<void>` | Строгая проверка, выбрасывает `AbilityError` при deny |
| `invalidatePolicy(policyId)` | `string` | `Promise<void>` | Инвалидация кэша для указанной политики |
| `invalidateCache()` | — | `Promise<void>` | Полная очистка кэша |
| `static isInPermissionContain(permissionA, permissionB)` | `string, string` | `boolean` | Проверка соответствия шаблону (с `*`) |

---

## AbilityResult

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
| `isAllowed()` | — | `boolean` | Доступ разрешён (нет deny или разрешён по умолчанию) |
| `isDenied()` | — | `boolean` | Доступ запрещён |
| `getLastEffectOfMatchedPolicy()` | — | `AbilityPolicyEffect \| null` | Эффект последней совпавшей политики |

---

## AbilityExplain

### Назначение

Базовый интерфейс для объяснений.

Обычно вы работаете с:

- `AbilityExplainPolicy`
- `AbilityExplainRuleSet`
- `AbilityExplainRule`

Общий метод:

| Метод | Описание |
|-------|----------|
| `toString(indent = 0)` | Возвращает человекочитаемое описание с отступами |

---

## AbilityParser

### Назначение

Утилита для генерации TypeScript-типов на основе политик.

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `static generateTypeDefs(policies)` | `AbilityPolicy[]` | `string` | Генерирует тип `Resources` на основе всех политик |

---

## AbilityJSONParser

### Назначение

Парсер и сериализатор для JSON-представления политик, групп и правил.

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `parse(configs)` | `AbilityPolicyConfig[]` | `AbilityPolicy[]` | Создаёт массив политик из JSON |
| `parsePolicy(config)` | `AbilityPolicyConfig` | `AbilityPolicy` | Создаёт политику из JSON |
| `parseRuleSet(config)` | `AbilityRuleSetConfig` | `AbilityRuleSet` | Создаёт группу из JSON |
| `parseRule(config)` | `AbilityRuleConfig` | `AbilityRule` | Создаёт правило из JSON |
| `ruleToJSON(rule)` | `AbilityRule` | `AbilityRuleConfig` | Экспорт правила в JSON |
| `ruleSetToJSON(ruleSet)` | `AbilityRuleSet` | `AbilityRuleSetConfig` | Экспорт группы в JSON |
| `policyToJSON(policy)` | `AbilityPolicy` | `AbilityPolicyConfig` | Экспорт политики в JSON |
| `toJSON(policies)` | `AbilityPolicy[]` | `AbilityPolicyConfig[]` | Экспорт массива политик в JSON |

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

Основные операции (код и литеральное имя):

| Код | Литерал | Описание |
|-----|---------|----------|
| `=` | `equals` | Равно |
| `<>` | `not_equals` | Не равно |
| `>` | `greater_than` | Больше |
| `<` | `less_than` | Меньше |
| `>=` | `greater_or_equal` | Больше или равно |
| `<=` | `less_or_equal` | Меньше или равно |
| `in` | `in` | Входит в массив |
| `not in` | `not_in` | Не входит в массив |
| `contains` | `contains` | Содержит (для массивов) |
| `not contains` | `not_contains` | Не содержит (для массивов) |
| `length greater than` | `length_greater_than` | Длина больше |
| `length less than` | `length_less_than` | Длина меньше |
| `length equals` | `length_equals` | Длина равна |

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
