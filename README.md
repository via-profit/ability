# @via-profit/Ability

> Набор сервисов, частично реализующих
> принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)
> Пакет позволяет описывать правила, объединять их в группы, формировать политики и применять их к данным для определения разрешений.

---

## Содержание

- [Установка](#установка)
- [Обзор](#обзор)
  - [Состав пакета](#состав-пакета)
  - [Основные принципы](#основные-принципы)
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
- [Производительность](#производительность)
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

---

## Установка

```bash
npm install @via-profit/ability
```

```bash
yarn add @via-profit/ability
```

```bash
pnpm add @via-profit/ability
```

### Требования

Модуль может работать в браузере и на NodeJS сервере 

- Node.js 18+ (для серверной части)
- TypeScript 5.0+ (рекомендуется для полной типобезопасности)

---

## Обзор

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
3. **Политики (AbilityPolicy)** — набор групп правил + действие (`action`) + эффект (`effect`).

Политики применяются через **AbilityResolver**, который:

- отбирает подходящие политики по `action` (с учётом wildcard),
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
  condition: AbilityCondition.equal,
});

// Сокращённая запись через статический конструктор
const rule2 = AbilityRule.equal(
  'user.department', // subject
  'managers',        // resource
);
```

Через JSON:

```ts
import { AbilityRule } from '@via-profit/ability';

const rule = AbilityRule.parse({
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
  action: 'order.update',
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
        condition: AbilityCondition.equal,
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
  action: 'order.update',
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

1. Отбирает политики по `action` (с учётом wildcard).
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

| Политика (action) | Действие | Совпадает |
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
    action: 'order.*',
    effect: 'permit',
    compareMethod: 'and',
    ruleSet: [],
  }),
  AbilityPolicy.parse({
    action: 'order.update',
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
    action: 'order.*',
    effect: 'deny', // по умолчанию запрещено
    compareMethod: 'and',
    ruleSet: [],
  },
  {
    action: 'order.create',
    effect: 'permit', // создание разрешено
    compareMethod: 'and',
    ruleSet: [],
  },
  {
    action: 'order.update',
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

- все `action`,
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
    "action": "order.update",
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
  "action": "order.update",
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

### Именование действий

- Используйте иерархические ключи: `order.create`, `order.update.status`, `user.profile.update`.
- Группируйте по доменам: `user.*`, `order.*`, `product.*`.
- Не смешивайте разные домены в одном действии.

### Структура данных

- Явно описывайте `Resources` в TypeScript.
- Не передавайте «лишние» поля — это усложняет понимание.
- Старайтесь, чтобы структура данных для одного `action` была стабильной.

### Проектирование политик

- Общие правила — через wildcard (`order.*`).
- Специфичные ограничения — через точные действия (`order.update`).
- Для запретов используйте `effect: deny`.
- Для разрешений — `effect: permit`.

### Типичные ошибки

- Ожидание, что отсутствие совпавших политик означает deny — это зависит от вашей модели. Обычно отсутствие deny трактуется как allow.
- Смешивание бизнес-логики и политик доступа.
- Слишком крупные политики с десятками правил — лучше разбивать.

### Пример использования на фронтенде (React)

**Хук для проверки политик**

```tsx
// hooks/use-ability.ts
import { useEffect, useState } from 'react';
import { AbilityResolver } from '@via-profit/ability';

export function useAbility<Action extends keyof Resources>(
  resolver: AbilityResolver<Resources>,
  action: Action,
  resource: Resources[Action],
) {
  const [allowed, setAllowed] = useState<boolean | null>(null);

  useEffect(() => {
    let cancelled = false;

    async function check() {
      try {
        const result = await resolver.resolve(action, resource);
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
  }, [resolver, action, resource]);

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

---

## Производительность

### Сравнение с/без кэша

| Сценарий | Без кэша | С кэшем (in-memory) |
|----------|----------|---------------------|
| 1 политика, 2 правила | ~0.5ms | ~0.05ms |
| 10 политик, 50 правил | ~3ms | ~0.2ms |
| 100 политик, 500 правил | ~25ms | ~1ms |

*Результаты могут варьироваться в зависимости от сложности правил и аппаратного обеспечения.*

### Рекомендации

- Используйте кэш для часто вызываемых политик.
- Устанавливайте разумный TTL в зависимости от частоты изменения данных.
- При изменении бизнес-логики не забывайте инвалидировать кэш.
- Для high-load систем используйте Redis вместо in-memory кэша.

---

## API Reference

---

### AbilityRule

#### Назначение

Минимальная единица логики — одно условие сравнения.

#### Основные свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор правила |
| `name` | `string` | Название |
| `subject` | `string` | Путь к полю субъекта |
| `resource` | `string \| number \| boolean \| (string \| number)[]` | Путь или значение ресурса |
| `condition` | `AbilityCondition` | Оператор сравнения |

#### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет правило |
| `explain()` | — | `AbilityExplainRule` | Объяснение проверки |
| `toJSON()` | — | `AbilityRuleConfig` | Экспорт в JSON |
| `static fromJSON(config)` | `AbilityRuleConfig` | `AbilityRule` | Создание из JSON |
| `static equal(subject, resource)` | `string, any` | `AbilityRule` | Упрощённый конструктор |
| `static notEqual(...)` и др. | | | Аналогично |

---

### AbilityRuleSet

#### Назначение

Группа правил с логикой `and` или `or`.

#### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор |
| `name` | `string` | Название |
| `compareMethod` | `AbilityCompare` | Логика сравнения |
| `rules` | `AbilityRule[]` | Список правил |

#### Методы

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

### AbilityPolicy

#### Назначение

Объединяет группы правил и определяет эффект при совпадении.

#### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `id` | `string` | Идентификатор |
| `name` | `string` | Название |
| `action` | `string` | Действие (с поддержкой `*`) |
| `effect` | `AbilityPolicyEffect` | `permit` или `deny` |
| `compareMethod` | `AbilityCompare` | Логика сравнения групп |
| `ruleSet` | `AbilityRuleSet[]` | Группы правил |
| `matchState` | `AbilityMatch` | Результат проверки |

#### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `check(resource, environment?)` | `object, object?` | `Promise<AbilityMatch>` | Проверяет политику |
| `explain()` | — | `AbilityExplainPolicy` | Объяснение |
| `toJSON()` | — | `AbilityPolicyConfig` | Экспорт |
| `static fromJSON(config)` | `AbilityPolicyConfig` | `AbilityPolicy` | Из JSON |
| `static fromJSONAll(configs)` | `AbilityPolicyConfig[]` | `AbilityPolicy[]` | Массовый парсинг |

---

### AbilityResolver

#### Назначение

Применяет политики к действию и ресурсу, формирует итоговый результат.

#### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `policies` | `AbilityPolicy[]` | Список политик |

#### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `resolve(action, resource, environment?)` | `string, any, object?` | `Promise<AbilityResult>` | Мягкая проверка |
| `enforce(action, resource, environment?)` | `string, any, object?` | `Promise<void>` | Строгая проверка, выбрасывает `AbilityError` при deny |
| `invalidatePolicy(policyId)` | `string` | `Promise<void>` | Инвалидация кэша политики |
| `invalidateCache()` | — | `Promise<void>` | Полная очистка кэша |
| `static isInActionContain(a, b)` | `string, string` | `boolean` | Проверка соответствия действия шаблону |

---

### AbilityResult (API)

#### Назначение

Инкапсулирует результат применения политик.

#### Свойства

| Свойство | Тип | Описание |
|----------|-----|----------|
| `policies` | `readonly AbilityPolicy[]` | Проверенные политики |

#### Методы

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `explain()` | — | `readonly AbilityExplain[]` | Объяснения по всем политикам |
| `getLastMatchedPolicy()` | — | `AbilityPolicy \| null` | Последняя сработавшая политика |
| `isAllowed()` | — | `boolean` | Итог не deny |
| `isDenied()` | — | `boolean` | Итог deny |
| `getLastEffect()` | — | `AbilityPolicyEffect \| null` | Эффект последней совпавшей политики |

---

### AbilityExplain (API)

#### Назначение

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

### AbilityParser

#### Назначение

Утилита для работы с конфигурациями и типами.

| Метод | Аргументы | Возвращает | Описание |
|-------|-----------|------------|----------|
| `static generateTypeDefs(policies)` | `AbilityPolicy[]` | `string` | Генерация TypeScript-типов `Resources` |

---

### AbilityMatch

Состояния проверки:

- `pending`
- `match`
- `mismatch`

Методы:

| Метод | Описание |
|-------|----------|
| `isEqual(other)` | Проверка равенства состояния |

---

### AbilityCompare

Способы сравнения:

- `and` — все элементы должны совпасть
- `or` — достаточно одного совпадения

---

### AbilityCondition

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

### AbilityPolicyEffect

Эффекты политики:

- `permit` — разрешить
- `deny` — запретить

---

### AbilityError

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

### AbilityCache

#### AbilityCacheAdapter

Интерфейс адаптера кэша. Позволяет подключить любое хранилище: Redis, Memcached, KeyDB, in-memory и т.д.

```ts
export interface AbilityCacheAdapter {
  get<T = unknown>(key: string): Promise<T | undefined>;
  set<T = unknown>(key: string, value: T, ttlSeconds?: number): Promise<void>;
  delete?(key: string): Promise<void>;
  clear?(): Promise<void>;
}
```

#### AbilityInMemoryCache

Встроенная реализация кэша в памяти.

```ts
import { AbilityInMemoryCache } from '@via-profit/ability';

const cache = new AbilityInMemoryCache({
  ttl: 60000, // время жизни в миллисекундах (по умолчанию 60000)
});
```

#### AbilityRedisCache

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

---

## Лицензия

Этот проект лицензирован под лицензией MIT. Подробности в файле [LICENSE](LICENSE).
