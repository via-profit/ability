# @via-profit/ability - Стратегии разрешения

[![npm version](https://img.shields.io/npm/v/@via-profit/ability)](https://www.npmjs.com/package/@via-profit/ability)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Стратегия разрешения определяет **как именно** принимать финальное решение, когда несколько политик дают противоречивые
результаты (одни разрешают, другие запрещают).

> 📚 **Связанные документы:**
> - [Резолвер](./resolver.md)
> - [DSL](./dsl.md)
> - [Генерация типов](./types-generator.md)

## Содержание

- [Что такое стратегия](#что-такое-стратегия)
- [Доступные стратегии](#доступные-стратегии)
- [Использование](#использование)
- [Создание своей стратегии](#создание-своей-стратегии)
- [Примеры сценариев](#примеры-сценариев)
- [Частые вопросы](#частые-вопросы)

---

## Что такое стратегия

Стратегия — это класс, который реализует логику принятия финального решения на основе **совпавших (matched)** политик.
Когда резолвер запускает проверку, он находит все политики, которые **совпали** (их условия выполнены) передает
стратегии, стратегия возвращает финальный результат.

Любая стратегия - это класс, который должен быть расширен от `AbilityStrategy`.

Базовый класс `AbilityStrategy` предоставляет методы для работы со списком совпавших политик:

| Метод                 | Описание                                            |
|-----------------------|-----------------------------------------------------|
| `matchedPolicies()`   | Возвращает все совпавшие политики                   |
| `hasMatched()`        | Проверяет, есть ли совпавшие политики               |
| `firstMatched()`      | Возвращает первую совпавшую политику                |
| `lastMatched()`       | Возвращает последнюю совпавшую политику             |
| `firstDenied()`       | Возвращает первую политику с эффектом `deny`        |
| `firstPermitted()`    | Возвращает первую политику с эффектом `permit`      |
| `getPermitPolicies()` | Возвращает все политики с эффектом `permit`         |
| `getDenyPolicies()`   | Возвращает все политики с эффектом `deny`           |
| `hasPermit()`         | Есть ли хотя бы одна `permit` политика              |
| `hasDeny()`           | Есть ли хотя бы одна `deny` политика                |
| `isAllowed()`         | Возвращает `true`, если финальный эффект — `permit` |
| `isDenied()`          | Возвращает `true`, если финальный эффект — `deny`   |

---

## Доступные стратегии

| Стратегия                         | Поведение                                                  | Когда использовать                                                   |
|-----------------------------------|------------------------------------------------------------|----------------------------------------------------------------------|
| **`DenyOverridesStrategy`**       | Если есть хотя бы одна `deny` → `deny`, иначе `permit`     | **По умолчанию**. Безопасный подход — запрет перевешивает разрешение |
| **`PermitOverridesStrategy`**     | Если есть хотя бы одна `permit` → `permit`, иначе `deny`   | Когда нужно дать максимальные права                                  |
| **`FirstMatchStrategy`**          | Результат первой сработавшей политики                      | Приоритет по порядку объявления политик                              |
| **`SequentialLastMatchStrategy`** | Результат последней сработавшей политики                   | Приоритет у последних объявленных политик                            |
| **`PriorityStrategy`**            | Выбирает политику с наибольшим `priority`                  | Явное управление приоритетами через `@priority`                      |
| **`AllMustPermitStrategy`**       | `permit` только если **все** совпавшие политики — `permit` | Максимально строгий доступ (консенсус)                               |
| **`OnlyOneApplicableStrategy`**   | `deny`, если сработало более одной политики                | Запрет множественных политик на один ресурс                          |
| **`AnyPermitStrategy`**           | `permit`, если есть хотя бы одна `permit`                  | Широкий доступ                                                       |

---

## Использование

### Базовое использование

```typescript
import {
  AbilityResolver,
  DenyOverridesStrategy,
  PermitOverridesStrategy,
  PriorityStrategy
} from '@via-profit/ability';

// Стандартная стратегия (по умолчанию)
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Стратегия, где разрешение перевешивает
const permissiveResolver = new AbilityResolver(policies, PermitOverridesStrategy);

// Стратегия с приоритетами
const priorityResolver = new AbilityResolver(policies, PriorityStrategy);
```

### Стратегия с приоритетами

```typescript
// DSL с указанием приоритета
const dsl = `
  @name "Обычный доступ"
  @priority 10
  permit permission.document.read if all:
    user.role equals "user"

  @name "Админский доступ"
  @priority 100
  permit permission.document.read if all:
    user.role equals "admin"
`;

const policies = new AbilityDSLParser(dsl).parse();
const resolver = new AbilityResolver(policies, PriorityStrategy);

// Будет использована политика с @priority 100
resolver.enforce('document.read', {
  document: { id: '123' }
}, {
  user: { role: 'admin' }
});
```

---

## Создание своей стратегии

Вы можете создать свою стратегию, расширив базовый класс `AbilityStrategy`:

```typescript
import {
  AbilityStrategy,
  AbilityPolicy,
  AbilityPolicyEffect,
  AbilityPolicyEffectType,
  ResourceObject,
  EnvironmentObject
} from '@via-profit/ability';

/**
 * Стратегия, которая требует, чтобы количество permit
 * было больше количества deny
 */
export class MajorityStrategy<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<Resource, Environment> {
  private decisive: AbilityPolicy<Resource, Environment> | null = null;

  evaluate(): AbilityPolicyEffectType {
    const permits = this.getPermitPolicies();
    const denies = this.getDenyPolicies();

    // Если нет совпавших политик - запрещаем
    if (!this.hasMatched()) {
      this.decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // Если permit больше чем deny
    if (permits.length > denies.length) {
      this.decisive = permits[0] || null;
      return AbilityPolicyEffect.permit;
    }

    // Если deny больше или равно permit
    this.decisive = denies[0] || null;
    return AbilityPolicyEffect.deny;
  }

  decisivePolicy(): AbilityPolicy<Resource, Environment> | null {
    return this.decisive;
  }
}

// Использование
const resolver = new AbilityResolver(policies, MajorityStrategy);
```

### Пример: Стратегия с временными окнами

```typescript
/**
 * Стратегия, которая разрешает доступ только в рабочее время
 */
export class BusinessHoursStrategy<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<Resource, Environment> {
  private decisive: AbilityPolicy<Resource, Environment> | null = null;

  evaluate(): AbilityPolicyEffectType {
    const now = new Date();
    const hour = now.getHours();
    const isBusinessHours = hour >= 9 && hour <= 18;

    // Вне рабочего времени - всегда запрет
    if (!isBusinessHours) {
      this.decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // В рабочее время - стандартная логика (deny overrides)
    if (this.hasDeny()) {
      this.decisive = this.firstDenied();
      return AbilityPolicyEffect.deny;
    }

    this.decisive = this.firstPermitted();
    return AbilityPolicyEffect.permit;
  }

  decisivePolicy(): AbilityPolicy<Resource, Environment> | null {
    return this.decisive;
  }
}
```

---

## Примеры сценариев

### Сценарий 1: Корпоративная безопасность

```typescript
// Используем DenyOverridesStrategy как наиболее безопасную
const securityResolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Любой запрет перевешивает все разрешения
securityResolver.enforce('system.delete', {
  system: { id: 'prod-1' }
}, {
  user: { role: 'developer' }
});
```

### Сценарий 2: Публичное API

```typescript
// Используем PermitOverridesStrategy для максимальной доступности
const publicResolver = new AbilityResolver(policies, PermitOverridesStrategy);

// Доступ разрешен, если есть хотя бы одно разрешение
publicResolver.enforce('api.read', {
  api: { endpoint: '/public' }
}, {
  apiKey: 'valid-key'
});
```

### Сценарий 3: Иерархия приоритетов

```typescript
// Используем PriorityStrategy для явного управления
const hierarchicalResolver = new AbilityResolver(policies, PriorityStrategy);

// В DSL указываем приоритеты:
// @priority 100 - супер-админ
// @priority 50  - модератор
// @priority 10  - обычный пользователь
```

### Сценарий 4: Строгий консенсус

```typescript
// Все политики должны согласиться
const consensusResolver = new AbilityResolver(policies, AllMustPermitStrategy);

// Доступ разрешен только если все совпавшие политики permit
consensusResolver.enforce('document.approve', {
  document: { id: '123', status: 'review' }
}, {
  reviewers: ['alice', 'bob']
});
```

---

## Частые вопросы

**Какая стратегия используется по умолчанию?**
По умолчанию используется `DenyOverridesStrategy` — она наиболее безопасна.

**Можно ли использовать несколько стратегий одновременно?**
Нет, один резолвер использует одну стратегию. Но можно создать несколько резолверов с разными стратегиями.

**Почему метод `decisivePolicy()` не вернул ничего, хотя доступ запрещен?**
Это метод, который возвращает политику, непосредственно повлиявшую на финальное решение. Если ни одна политика не
совпала, то решение о запрете принято стратегией, а значит в `decisivePolicy` будет пусто.

**Как выбрать правильную стратегию?**

- Для безопасности: `DenyOverridesStrategy` или `AllMustPermitStrategy`
- Для доступности: `PermitOverridesStrategy` или `AnyPermitStrategy`
- Для контроля: `PriorityStrategy` или `FirstMatchStrategy`
- Для строгости: `OnlyOneApplicableStrategy`
