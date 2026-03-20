# Changelog

## [3.1.0] - 2026-03-20

### Добавлено

- Реализован кэш
  - Добавлен кэш-провайдер `AbilityCacheProvider` для реализации кастомного кэша
  - Реализован и включён по умолчанию `AbilityInMemoryCache` (кэш в памяти)
- Добавлена полноценная поддержка `environment` как третьего аргумента в:
  - `resolver.resolve(action, resource, environment)`
  - `resolver.enforce(action, resource, environment)`
- Введена возможность использовать пути вида `env.*` в правилах политик.
  - Пример: `"subject": "env.time.hour"`
- Добавлена поддержка смешанных сравнений:
  - `resource.*` ↔ `env.*`
  - литерал ↔ `env.*`
  - `env.*` ↔ литерал

### Breaking changes

Асинхронизация механизма проверки политик.
Все методы, участвующие в цепочке вычисления разрешений, теперь возвращают `Promise`.

#### Изменено

- `AbilityRule.check(resource): Promise<AbilityMatch>`  
  Ранее возвращал `AbilityMatch` синхронно.

- `AbilityRuleSet.check(resource): Promise<AbilityMatch>`  
  Теперь выполняет правила последовательно и асинхронно.

- `AbilityPolicy.check(resource): Promise<AbilityMatch>`  
  Асинхронно проверяет ruleSet в строгом порядке.

- `AbilityResolver.resolve(action, resource): Promise<AbilityResult>`  
  Теперь асинхронный метод, который дожидается выполнения всех политик.

- `AbilityResolver.enforce(action, resource): Promise<void | never>`  
  Теперь работает асинхронно.

### Миграция

1. Все вызовы `check()` должны быть обновлены:

```ts
await rule.check(resource);
await ruleSet.check(resource);
await policy.check(resource);
```

2. Все вызовы `resolver.resolve()` и `resolver.enforce()` теперь требуют `await`:

```ts
await resolver.resolve('order.update', resource);
await resolver.enforce('order.update', resource);
```

## [3.0.1] - 2026-03-19

## Добавлено

### 1. **Лицензия MIT** (`LICENSE`)

- Добавлен официальный файл лицензии MIT от Via Profit

### 2. **Класс `AbilityExplain.ts`**

- Новый класс для получения человекочитаемых объяснений результатов проверки
- Классы-наследники:
    - `AbilityExplainRule` - объяснение для правила
    - `AbilityExplainRuleSet` - объяснение для группы правил
    - `AbilityExplainPolicy` - объяснение для политики
- Метод `toString()` форматирует вывод с отступами и символами ✓/✗

---

## Обновлено

### **AbilityParser.ts** (полная переработка)

- **Было**: Базовая генерация типов
- **Стало**: Расширенная система генерации TypeScript типов
- Новые методы:
    - `determineTypeFromRule()` - определение типа на основе правила
    - `getArrayType()` - обработка массивов
    - `getPrimitiveType()` - определение примитивных типов
    - `buildNestedStructure()` - трансформация плоской структуры во вложенную
    - `formatTypeDefinitions()` - форматирование финального вывода
    - `formatNestedObject()` - рекурсивное форматирование объектов

### **AbilityRule.ts**

- `id` и `name` теперь опциональные (`?`)
- Автогенерация `id` и `name` если не предоставлены
- **Новые статические методы** (фабричные методы):
    - `equal()`, `notEqual()`, `in()`, `notIn()`
    - `lessThan()`, `lessOrEqual()`, `moreThan()`, `moreOrEqual()`

### **AbilityRuleSet.ts**

- `id` и `name` теперь опциональные
- Добавлены статические методы:
    - `and()` - создание группы с логическим И
    - `or()` - создание группы с логическим ИЛИ

### **AbilityPolicy.ts**

- Новый метод `explain()` - получение объяснения проверки
- Новый статический метод `parseAll()` - парсинг массива конфигураций
- Улучшены комментарии к полю `action`

### **AbilityResolver.ts**

- Новый метод `resolveWithExplain()` - проверка с детальным объяснением
- Возвращает массив `AbilityExplain[]` для анализа результатов

---

## Обновлена документация и примеры
