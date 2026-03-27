# @via-profit/Ability

> Набор сервисов, частично реализующих
> принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)
> Пакет позволяет описывать правила, объединять их в группы, формировать политики и применять их к данным для определения разрешений.

---

## Содержание

- [Установка](#установка)
- [Быстрый старт](#быстрый-старт)
  - [Состав пакета](#состав-пакета)
  - [Основные принципы](#основные-принципы)
- [DSL](#dsl)
- [Правила](#правила)
  - [Создание правила](#создание-правила)
  - [Проверка правила](#проверка-правила)
  - [Типичные ошибки при работе с правилами](#типичные-ошибки-при-работе-с-правилами)
- [Группы правил](#группы-правил)
  - [Создание группы правил](#создание-группы-правил)
  - [Проверка группы правил](#проверка-группы-правил)
  - [Типичные ошибки при работе с группами](#типичные-ошибки-при-работе-с-группами)
- [Политики](#политики)
  - [Создание политики](#создание-политики)
  - [Проверка политики](#проверка-политики)
  - [Как формируется итоговый эффект](#как-формируется-итоговый-эффект)
- [Управление политиками](#управление-политиками)
  - [Зачем нужен AbilityResolver](#зачем-нужен-abilityresolver)
  - [Wildcard в экшенах](#wildcard-в-экшенах)
  - [Приоритет и множественные совпадения](#приоритет-и-множественные-совпадения)
  - [Комбинирование точных действий и wildcard](#комбинирование-точных-действий-и-wildcard)
  - [Интеграция с TypeScript](#интеграция-с-typescript)
- [Environment (контекст выполнения)](#environment-контекст-выполнения)
- [Кэш](#кэш)
- [AbilityResult и Explain](#abilityresult-и-explain)
  - [AbilityResult](#abilityresult)
  - [AbilityExplain](#abilityexplain)
- [Рекомендации по проектированию](#рекомендации-по-проектированию)
  - [Именование действий](#именование-действий)
  - [Структура данных](#структура-данных)
  - [Проектирование политик](#проектирование-политик)
  - [Типичные ошибки](#типичные-ошибки)
- [API Reference](#api-reference)
  - [AbilityRule](#abilityrule)
  - [AbilityRuleSet](#abilityruleset)
  - [AbilityPolicy](#abilitypolicy)
  - [AbilityResolver](#abilityresolver)
  - [AbilityResult](#abilityresult-api)
  - [AbilityExplain](#abilityexplain-api)
  - [AbilityParser](#abilityparser)
  - [AbilityMatch](#abilitymatch)
  - [AbilityCompare](#abilitycompare)
  - [AbilityCondition](#abilitycondition)
  - [AbilityPolicyEffect](#abilitypolicyeffect)
  - [AbilityError](#abilityerror)
  - [AbilityCache](#abilitycache)
  - [AbilityJSONParser](abilityjsonparser)

---



### Состав пакета

#### Ядро (Core)

- **AbilityRule** — класс отдельного правила
- **AbilityRuleSet** — класс группы правил
- **AbilityPolicy** — класс политики
- **AbilityResolver** — управление политиками и вычисление итогового решения
- **AbilityResult** — результат работы политик для конкретного действия
- **AbilityMatch** — константы состояния проверки (`pending`, `match`, `mismatch`)
- **AbilityCompare** — способы сравнения (`or`, `and`)
- **AbilityCondition** — операции сравнения (`equal`, `not_equal`, `more_than`, `less_than`, `in`, `not_in` и др.)
- **AbilityPolicyEffect** — эффекты политики (`deny`, `permit`)
- **AbilityParser** — парсер конфигурационных правил (JSON) и генератор TypeScript-типов
- **AbilityError** — класс ошибок
- **AbilityExplain** — вспомогательный инструмент для человекочитаемых объяснений результата

#### Кэш (Cache)

- `AbilityCacheAdapter` — интерфейс адаптера кэша
- `AbilityInMemoryCache` — встроенный кэш в памяти (используется по умолчанию)

### Основные принципы

Система строится на трёх уровнях:

1. **Правила (AbilityRule)** — минимальные условия сравнения.
2. **Группы правил (AbilityRuleSet)** — объединение правил с логикой `and` или `or`.
3. **Политики (AbilityPolicy)** — набор групп правил + ключ разрешения (`permission`) + эффект (`effect`).

Политики применяются через **AbilityResolver**, который:

- отбирает подходящие политики по `permission` (с учётом wildcard),
- выполняет их проверку,
- формирует итоговый результат в виде **AbilityResult**.

---



## Правила

Правило выполняет одно сравнение над ресурсом: сравнивает значение поля субъекта и ресурса с использованием заданного оператора.

### Создание правила

Создать правило можно:

1. Через конструктор.
2. Через парсинг JSON-конфига.

```ts
import { AbilityRule, AbilityCondition } from '@via-profit/ability';

const rule = new AbilityRule({
  id: 'rule-department-managers',
  name: 'Пользователь из отдела managers',
  subject: 'user.department', // <-- subject - это всегда строка dot.notation
  resource: 'managers',  // <-- resource может быть dot.notation строкой, либо значением примитива
  condition: AbilityCondition.equals,
});

// Сокращённая запись через статический конструктор
const rule2 = AbilityRule.equals(
  'user.department', // subject
  'managers',        // resource
);
```

Через JSON:

```ts
import { AbilityRule } from '@via-profit/ability';

const rule = AbilityRule.fromJSON({
  id: 'rule-department-managers',
  name: 'Пользователь из отдела managers',
  subject: 'user.department',
  resource: 'managers',
  condition: '=',
});
```

### Проверка правила

```ts
import { AbilityMatch } from '@via-profit/ability';

const match = await rule.check({
  user: {
    department: 'managers',
  },
});

const isMatch = match.isEqual(AbilityMatch.match); // true
```

### Типичные ошибки при работе с правилами

- Неверный путь в `subject` или `resource` (опечатки в dot-notation).
- Использование неподходящего `condition` (например, `in` для числа).
- Ожидание, что правило само выбросит исключение — оно только возвращает состояние (`match`/`mismatch`).

---

## Группы правил

Группа правил (`AbilityRuleSet`) объединяет несколько правил и возвращает один итоговый результат.

### Создание группы правил

```ts
import { AbilityRuleSet, AbilityCompare } from '@via-profit/ability';

const ruleSet = new AbilityRuleSet({
  id: 'rs1',
  name: 'Менеджеры, не администраторы',
  compareMethod: AbilityCompare.and,
}).addRules([
  AbilityRule.equal('user.department', 'managers'),
  AbilityRule.notIn('user.roles', 'administrator'),
]);

// Сокращённая запись
const ruleSet2 = AbilityRuleSet.and([
  AbilityRule.equal('user.department', 'managers'),
  AbilityRule.notIn('user.roles', 'administrator'),
]);
```

Через JSON:

```ts
import { AbilityRuleSet } from '@via-profit/ability';

const ruleSet = AbilityRuleSet.parse({
  id: 'rs1',
  name: 'Менеджеры',
  compareMethod: 'and',
  rules: [
    {
      id: 'r1',
      name: 'Отдел managers',
      subject: 'user.department',
      resource: 'managers',
      condition: '=',
    },
  ],
});
```

### Проверка группы правил

```ts
const match = await ruleSet.check({
  user: {
    department: 'managers',
    roles: ['manager'],
  },
});

const isMatch = match.isEqual(AbilityMatch.match);
```

### Типичные ошибки при работе с группами

- Неправильный выбор `compareMethod`:
  - `and` — все правила должны совпасть.
  - `or` — достаточно одного совпадения.
- Пустой список правил: группа всегда будет `mismatch`.

---

## Политики

Политика (`AbilityPolicy`) объединяет группы правил и определяет итоговый эффект (`permit` или `deny`) при совпадении.

### Создание политики

```ts
import {
  AbilityPolicy,
  AbilityPolicyEffect,
  AbilityCompare,
  AbilityRuleSet,
  AbilityRule,
  AbilityCondition,
} from '@via-profit/ability';

const policy = new AbilityPolicy({
  id: 'policy-order-update-deny-managers',
  name: 'Запрет обновления заказа для менеджеров (кроме админов)',
  permission: 'order.update',
  effect: AbilityPolicyEffect.deny,
  compareMethod: AbilityCompare.and,
  ruleSet: [
    new AbilityRuleSet({
      id: 'rs-managers',
      name: 'Менеджеры',
      compareMethod: AbilityCompare.or,
    }).addRules([
      new AbilityRule({
        id: 'r-dept',
        name: 'Отдел managers',
        subject: 'user.department',
        resource: 'managers',
        condition: AbilityCondition.equals,
      }),
      new AbilityRule({
        id: 'r-role-manager',
        name: 'Роль manager',
        subject: 'user.roles',
        resource: 'manager',
        condition: AbilityCondition.in,
      }),
    ]),
    new AbilityRuleSet({
      id: 'rs-not-admin',
      name: 'Не администраторы',
      compareMethod: AbilityCompare.and,
    }).addRules([
      AbilityRule.notIn('user.roles', 'administrator'),
    ]),
  ],
});
```

Через JSON:

```ts
import { AbilityPolicy } from '@via-profit/ability';

const policy = AbilityPolicy.parse({
  id: 'policy-order-update-deny-managers',
  name: 'Запрет доступа для менеджеров (исключение: администраторы)',
  permission: 'order.update',
  effect: 'deny',
  compareMethod: 'and',
  ruleSet: [
    {
      name: 'Менеджеры',
      compareMethod: 'or',
      rules: [
        {
          name: 'Отдел managers',
          subject: 'user.department',
          resource: 'managers',
          condition: 'in',
        },
        {
          name: 'Роль manager',
          subject: 'user.roles',
          resource: 'manager',
          condition: 'in',
        },
      ],
    },
    {
      name: 'Не администраторы',
      compareMethod: 'and',
      rules: [
        {
          name: 'Нет роли administrator',
          subject: 'user.roles',
          resource: 'administrator',
          condition: 'not in',
        },
      ],
    },
  ],
});
```

Через DSL:

```
# @name Запрет доступа для менеджеров (исключение: администраторы)
deny order.update if all:

  # @name Менеджеры
  any of:
    # @name Отдел managers
    user.department in 'managers'

    # @name Роль manager
    user.roles in 'manager'

  # @name Не администраторы
  all of:
    # @name Нет роли administrator
    user.roles not in 'administrator'
```


### Проверка политики

```ts
import { AbilityMatch } from '@via-profit/ability';

const match = await policy.check({
  user: {
    department: 'managers',
    roles: ['manager', 'coach'],
  },
});

const isMatch = match.isEqual(AbilityMatch.match);
```

### Как формируется итоговый эффект

- Политика считается **сработавшей**, если её `matchState` равен `match`.
- Если политика сработала, её `effect` (`permit` или `deny`) участвует в итоговом решении.
- При использовании `AbilityResolver` итог определяется **последней сработавшей политикой**.

---

## Управление политиками

`AbilityResolver` — центральный компонент, который:

1. Отбирает политики по `permission` (с учётом wildcard).
2. Вызывает `policy.check(resource, environment?)` для каждой.
3. Формирует итоговый результат в виде `AbilityResult`.
4. При необходимости выбрасывает `AbilityError` (метод `enforce`).

### Зачем нужен AbilityResolver

Выходя за рамки тестовых примеров мы видим, что в реальных системах количество политик может достигать сотни, а то и больше.
Для удобства управления всем этим зоопарком и придуман `AbilityResolver`.

`AbilityResolver` позволяет:

- автоматически фильтровать политики по экшенам
- централизованно вычислять итоговый эффект
- получать объяснения (Explain), почему доступ разрешён или запрещён
- кэшировать результат вычислений политик (включается вручную)

### Wildcard в экшенах

Экшены записываются в dot notation виде, но поддерживают возможность задавать общие правила (`order.*`) и/или переопределять их более специфичными (`order.update`), а так же накладывать «вето» на ранее разрешённые действия

Поддерживаются шаблоны с `*`:

| Политика (permission) | Действие | Совпадает |
|-------------------|----------|-----------|
| `order.*`         | `order.create` | да |
| `order.*`         | `order.update` | да |
| `order.*`         | `user.create`  | нет |
| `*.create`        | `order.create` | да |
| `*.create`        | `user.create`  | да |
| `*.create`        | `order.update` | нет |
| `user.profile.*`  | `user.profile.update` | да |
| `user.profile.*`  | `user.settings.update` | нет |

### Приоритет и множественные совпадения

Если под действие подходит несколько политик, **выполняются все**.  
Итог определяется **последней совпавшей политикой**:

```ts
const policies = [
  AbilityPolicy.parse({
    permission: 'order.*',
    effect: 'permit',
    compareMethod: 'and',
    ruleSet: [],
  }),
  AbilityPolicy.parse({
    permission: 'order.update',
    effect: 'deny',
    compareMethod: 'and',
    ruleSet: [],
  }),
];

await new AbilityResolver(policies).enforce('order.update', resource);
```


### Комбинирование точных действий и wildcard

```ts
const policies = [
  {
    permission: 'order.*',
    effect: 'deny', // по умолчанию запрещено
    compareMethod: 'and',
    ruleSet: [],
  },
  {
    permission: 'order.create',
    effect: 'permit', // создание разрешено
    compareMethod: 'and',
    ruleSet: [],
  },
  {
    permission: 'order.update',
    effect: 'deny',
    compareMethod: 'and',
    ruleSet: [
      // дополнительные правила
    ],
  },
];

const resolver = new AbilityResolver(AbilityPolicy.parseAll(policies));
```

### Интеграция с TypeScript

```ts
import { AbilityResolver, AbilityPolicy } from '@via-profit/ability';

type Resources = {
  'order.create': {
    readonly user: {
      readonly department: string;
      readonly roles: readonly string[];
    };
    readonly order: {
      readonly amount: number;
    };
  };
  'order.update': {
    readonly user: {
      readonly id: string;
      readonly roles: readonly string[];
    };
    readonly order: {
      readonly id: string;
      readonly status: string;
      readonly ownerId: string;
    };
  };
};

const policies = AbilityPolicy.parseAll<Resources>(configs);
const resolver = new AbilityResolver<Resources>(policies);

await resolver.enforce('order.create', {
  user: {
    department: 'managers',
    roles: ['manager'],
  },
  order: {
    amount: 5000,
  },
});

// Ошибка компиляции — не хватает полей
await resolver.enforce('order.create', {
  user: {
    department: 'managers',
  },
});
```

### Использование `generateTypeDefs` в AbilityParser

`AbilityParser.generateTypeDefs()` — это утилита, которая автоматически генерирует **TypeScript‑типы для Resources** на основе ваших JSON‑политик.

#### Как это работает

`generateTypeDefs` принимает список политик и анализирует:

- все `permission`,
- все `subject` и `resource` пути,
- строит дерево объектов,
- генерирует корректный TypeScript‑тип `Resources`.

#### Пример использования

**1. Политики в JSON**

```json
[
  {
    "id": "order-update",
    "name": "Update order",
    "permission": "order.update",
    "effect": "permit",
    "compareMethod": "and",
    "ruleSet": [
      {
        "name": "Owner check",
        "compareMethod": "and",
        "rules": [
          {
            "name": "User is owner",
            "subject": "user.id",
            "resource": "order.ownerId",
            "condition": "="
          }
        ]
      }
    ]
  }
]
```

**Скрипт генерации типов**

```ts
// scripts/generate-types.ts
import { AbilityParser } from '@via-profit/ability';
import policies from '../policies.json';
import { writeFileSync } from 'fs';

const typedefs = AbilityParser.generateTypeDefs(policies);

writeFileSync('./src/ability/types.generated.ts', typedefs, 'utf8');
```

**Сгенерированный файл (пример)**

```ts
// src/ability/types.generated.ts

// Automatically generated by via-profit/ability
// Do not edit manually
export type Resources = {
  'order.update': {
    readonly user: {
      readonly id: string;
    };
    readonly order: {
      readonly ownerId: string;
    };
  };
};
```

**Использование в коде**

```ts
import { AbilityResolver, AbilityPolicy } from '@via-profit/ability';
import type { Resources } from './ability/types.generated';

const resolver = new AbilityResolver<Resources>(
  AbilityPolicy.parseAll(policies),
);

await resolver.enforce('order.update', {
  user: { id: 'u1' },
  order: { ownerId: 'u1' },
});
```

#### Рекомендации по использованию

- Генерируйте типы **автоматически при сборке**.
- Не редактируйте `types.generated.ts` вручную.
- Храните политики в JSON — это облегчает анализ.
- Используйте `generateTypeDefs` как единственный источник истины для структуры ресурсов.

---

## Environment (контекст выполнения)

Пакет имеет поддержку полноценных **environment‑атрибутов**, что позволяет использовать в политиках данные окружения, например:

- время запроса,
- IP‑адрес,
- геолокацию,
- параметры устройства,
- заголовки запроса,
- контекст сессии,
- любые другие внешние условия.

### Что такое Environment

**Environment** — это объект, содержащий данные окружения, которые не принадлежат ни пользователю, ни ресурсу.
Содержимое объекта определяется разработчиком и может быть любым объектом состоящим из примитивов.

Примеры:

```ts
type Environment = {
  time: {
    hour: number;
  };
  ip: string;
  geo: {
    country: string;
  };
};
```

Environment передаётся в `resolve()` и `enforce()` как третий аргумент:

```ts
await resolver.resolve('order.update', resource, environment);
await resolver.enforce('order.update', resource, environment);
```

### Использование environment в правилах

В политике можно ссылаться на environment через путь `env.*`.

Пример:

```json
{
  "subject": "env.time.hour",
  "resource": 9,
  "condition": "more_or_equal"
}
```

Это правило означает:

> Разрешить действие, если текущий час ≥ 9.

### Пример политики с environment

```json
{
  "id": "deny-night-updates",
  "name": "Deny updates at night",
  "permission": "order.update",
  "effect": "deny",
  "compareMethod": "and",
  "ruleSet": [
    {
      "compareMethod": "and",
      "rules": [
        {
          "subject": "env.time.hour",
          "resource": 22,
          "condition": "more_or_equal"
        },
        {
          "subject": "env.time.hour",
          "resource": 6,
          "condition": "less_than"
        }
      ]
    }
  ]
}
```

Эта политика запрещает обновление заказов ночью (22:00–06:00).

### Как работает извлечение значений

Если в правиле указан путь:

- `env.*` → значение берётся из environment
- `user.*`, `order.*`, `profile.*` → из resource
- литерал (`18`, `"admin"`, `true`) → используется как есть

Пример:

```ts
subject: "env.geo.country"
resource: "user.country"
condition: "equal"
```

### Типизация Environment

Тип Environment задаётся на уровне `AbilityResolver`:

```ts
const resolver = new AbilityResolver<Resources, Environment>(policies);
```

Это позволяет:

- получать автодополнение в IDE,
- проверять корректность путей `env.*`,
- избегать ошибок при передаче environment.

### Поведение при отсутствии environment

Если правило использует `env.*`, но environment не передан:

```ts
await rule.check(resource, undefined);
```

то значение `env.*` будет `undefined`, и сравнение будет выполнено так, как если бы environment не было вовсе:

- `undefined >= 10` → `false`
- `undefined === true` → `false`

---

## Кэш

`AbilityResolver` имеет встроенный кэш для оптимизации производительности. По умолчанию используется `AbilityInMemoryCache` с TTL 60 секунд.

### Конфигурация кэша

```ts
import { AbilityResolver, AbilityInMemoryCache } from '@via-profit/ability';

// Использовать in-memory кэш (по умолчанию)
const resolver = new AbilityResolver(policies);

// Отключить кэш
const resolverNoCache = new AbilityResolver(policies, { cache: null });

// Кастомный TTL
const resolverCustom = new AbilityResolver(policies, {
  cache: new AbilityInMemoryCache({ ttl: 30000 }) // 30 секунд
});
```

### Redis адаптер

```ts
import { AbilityResolver, AbilityRedisCache } from '@via-profit/ability';
import Redis from 'ioredis';

const redis = new Redis();
const resolver = new AbilityResolver(policies, {
  cache: new AbilityRedisCache(redis, { ttl: 60000 })
});
```

### Создание собственного адаптера

```ts
import { AbilityCacheAdapter } from '@via-profit/ability';

class MyCustomCache implements AbilityCacheAdapter {
  async get<T>(key: string): Promise<T | undefined> {
    // ваша логика
  }
  
  async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
    // ваша логика
  }
  
  async delete?(key: string): Promise<void> {
    // опционально
  }
  
  async clear?(): Promise<void> {
    // опционально
  }
}
```

### Инвалидация кэша

```ts
// Инвалидация конкретной политики
await resolver.invalidatePolicy('order-policy-id');

// Полная очистка кэша
await resolver.invalidateCache();
```

---

## AbilityResult и Explain

### AbilityResult

`AbilityResult` — объект, инкапсулирующий итог применения всех подходящих политик к действию и ресурсу.

Он содержит:

- список проверенных политик,
- методы для определения итогового эффекта,
- методы для получения объяснений.

Пример:

```ts
const result = await resolver.resolve('order.update', resource);

if (result.isDenied()) {
  console.log('Access denied');
}

const explanations = result.explain();
```

### AbilityExplain

`AbilityExplain` и связанные классы (`AbilityExplainPolicy`, `AbilityExplainRuleSet`, `AbilityExplainRule`) позволяют получить человекочитаемое объяснение:

- какая политика сработала,
- какие группы правил совпали,
- какие правила не прошли,
- какой эффект был применён.

Пример использования:

```ts
const result = await resolver.resolve('order.update', resource);
const explanations = result.explain();

explanations.forEach(explain => {
  console.log(explain.toString());
});
```

Пример вывода:

```
✓ policy «Запрет обновления заказа для менеджеров» is match
  ✓ ruleSet «Менеджеры» is match
    ✓ rule «Отдел managers» is match
    ✗ rule «Роль manager» is mismatch
  ✓ ruleSet «Не администраторы» is match
    ✓ rule «Нет роли administrator» is match
```

---

## Рекомендации по проектированию

### Именование ключей доступа

- Используйте иерархические ключи: `permission.order.create`, `permission.order.update.status`, `permission.user.profile.update`.
- Группируйте по доменам: `permission.user.*`, `permission.order.*`, `permission.product.*`.
- Не смешивайте разные домены в одном действии.

### Структура данных

- Явно описывайте `Resources` в TypeScript.
- Не передавайте «лишние» поля — это усложняет понимание.
- Старайтесь, чтобы структура данных для одного `permission` была стабильной.

### Проектирование политик

- Общие правила — через wildcard (`permission.order.*`).
- Специфичные ограничения — через точные действия (`permission.order.update`).
- Для запретов используйте `effect: deny`.
- Для разрешений — `effect: permit`.

### Типичные ошибки

- Ожидание, что отсутствие совпавших политик означает deny — это зависит от вашей модели. Обычно отсутствие deny трактуется как deny.
- Смешивание бизнес-логики и политик доступа.
- Слишком крупные политики с десятками правил — лучше разбивать.

### Пример использования на фронтенде (React)

**Хук для проверки политик**

```tsx
// hooks/use-ability.ts
import { useEffect, useState } from 'react';
import { AbilityResolver } from '@via-profit/ability';

export function useAbility<Permission extends keyof Resources>(
  resolver: AbilityResolver<Resources>,
  permission: Permission,
  resource: Resources[Permission],
) {
  const [allowed, setAllowed] = useState<boolean | null>(null);

  useEffect(() => {
    let cancelled = false;

    async function check() {
      try {
        const result = await resolver.resolve(permission, resource);
        if (!cancelled) {
          setAllowed(result.isAllowed());
        }
      } catch {
        if (!cancelled) {
          setAllowed(false);
        }
      }
    }

    check();

    return () => {
      cancelled = true;
    };
  }, [resolver, permission, resource]);

  return allowed;
}
```

**Использование в компоненте**

```tsx
function OrderUpdateButton({ order, user }) {
  const allowed = useAbility(resolver, 'order.update', {
    user,
    order,
  });

  if (allowed === null) {
    return null; // или бейдж загрузки
  }

  if (!allowed) {
    return null;
  }

  return <button>Update order</button>;
}
```


### Рекомендации

- Используйте кэш для часто вызываемых политик.
- Устанавливайте разумный TTL в зависимости от частоты изменения данных.
- При изменении бизнес-логики не забывайте инвалидировать кэш.
- Для high-load систем используйте Redis вместо in-memory кэша.

---
