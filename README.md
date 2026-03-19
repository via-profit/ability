# @via-profit/Ability

> Набор сервисов, частично реализующих
> принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)

Этот сервис позволяет создавать правила и политики, применять их к данным и проверять доступ на их основе.

## Содержание

- [Обзор](#overview)
    - [Состав пакета](#structure)
    - [Основные принципы](#principles)
- [Правила](#rules)
    - [Создание правила](#rule-creation)
    - [Проверка правила](#rule-check)
- [Группы правил](#rule-sets)
    - [Создание группы правил](#ruleset-creation)
    - [Проверка группы правил](#ruleset-check)
- [Политики](#policies)
  - [Создание политики](#policy-creatсion)
    - [Проверка политики](#policy-check)
- [Управление политиками](#policy-management)
- [API Reference](#api-reference)
- [Рекомендации по использованию](#best-practices)
- [Отладка политик](#debugging)


---

## Обзор <a name="overview"></a>

### Состав пакета <a name="structure"></a>

- **`AbilityRule`** — класс отдельного правила
- **`AbilityRuleSet`** — класс группы правил
- **`AbilityPolicy`** — класс политики
- **`AbilityResolver`** — управление политиками
- **`AbilityMatch`** — константы состояния правил (`pending`, `match`, `mismatch`)
- **`AbilityCompare`** — способы сравнения (`or`, `and`)
- **`AbilityCondition`** — методы вычисления (`equal`, `not_equal`, `more_than`, `less_than`, `in`, `not_in` и др.)
- **`AbilityPolicyEffect`** — эффекты политики (`deny`, `permit`)
- **`AbilityParser`** — парсер конфигурационных правил (JSON) и генератор `Typescript` типов
- **`AbilityError`** — инстанс ошибок
- **`AbilityExplain`** — вспомогательный инструмент, который позволяет получить человекочитаемое объяснение того, почему конкретное действие разрешено или запрещено текущей конфигурацией Ability

### Основные принципы <a name="principles"></a>

Работа сервиса основана на формировании **правил**, объединении их в **политики** и проверке доступа с их помощью.

Пример: необходимо **запретить доступ** пользователям, связанным с отделом менеджеров, **за исключением администраторов**.

- Менеджеры — если их отдел `managers` или есть роль `manager`
- Администраторы — пользователи с ролью `administrator`

Структура политики:

![ability-01.png](./assets/ability-01.drawio.png)

JSON-конфигурация:

```json
{
  "name": "Запрет доступа для менеджеров (исключение: администраторы)",
  "compareMethod": "and",
  "action": "order.update",
  "effect": "deny",
  "ruleSet": [
    {
      "name": "Менеджеры",
      "compareMethod": "or",
      "rules": [
        {
          "name": "Отдел managers",
          "subject": "user.department",
          "resource": "managers",
          "condition": "in"
        },
        {
          "name": "Роль manager",
          "subject": "user.roles",
          "resource": "manager",
          "condition": "in"
        }
      ]
    },
    {
      "name": "Не администраторы",
      "compareMethod": "and",
      "rules": [
        {
          "name": "Нет роли administrator",
          "subject": "user.roles",
          "resource": "administrator",
          "condition": "not in"
        }
      ]
    }
  ]
}
```

Применение политики:

```ts
const jsonConfig = { ... };
AbilityPolicy.parse(jsonConfig).check({
  user: {
    department: 'managers',
    roles: ['manager', 'coach'],
  }
});
```

---

## Правила <a name="rules"></a>

**Правила** выполняют условие проверки и возвращают результат. **Основная цель** - выполнить сравнение переданных
значений субъекта и ресурса, а затем вернуть результат такого сравнения.

### Создание правила <a name="rule-creation"></a>

Создать правило можно двумя способами: создание через конструктор класса и парсинг JSON-конфига правила.

При создании необходимо указать следующие параметры:

- **id** - `string` Уникальный идентификатор.
- **name** - `string` Название правила.
- **condition** - `AbilityCondition` Определяет условия сравнения переданных данных
- **subject** - `string` Dot notation путь в проверяемом субъекте, например: `user.name`.
- **resource** - `string | number | boolean | (string | number)[]` Dot notation путь в проверяемом ресурсе, например:
  `user.name` или значение, которое может быть строкой, числом, булеан значением или массивом строк, или чисел.

_Создание правила через конструктор класса:_

```ts
import { AbilityRule, AbilityCondition } from '@via-profit/ability';

const rule = new AbilityRule({
  id: '<rule-id>',
  name: 'Пользователь из отдела managers',
  subject: 'user.department',
  resource: 'managers',
  condition: AbilityCondition.equal
});

// сокращённая запись
const rule2 = AbilityRule.equal(
  'user.department', // subject
  'managers' // resource
);

```

_Создание правила через парсинг JSON-конфигурации:_

```ts
import { AbilityRule } from '@via-profit/ability';

const rule = AbilityRule.parse({
  "id": "<rule-id>",
  "name": "Пользователь из отдела managers",
  "subject": "user.department",
  "resource": "managers",
  "condition": "="
});

```

### Проверка правила <a name="rule-check"></a>

Для проверки правила следует вызвать метод `check` класса `AbilityRule` передав объект проверяемого ресурса. Этот метод
вернёт экземпляр класса
`AbilityMatch`, при помощи методов которого можно определить имеется ли совпадение правила и переданных значений.

```ts
import { AbilityRule } from '@via-profit/ability';

const rule = AbilityRule.parse({
  "id": "<rule-id>",
  "name": "Пользователь из отдела managers",
  "subject": "user.department",
  "resource": "managers",
  "condition": "="
});

const match = rule.check({
  user: {
    department: 'managers',
  },
});

const is = match.isEqual(AbilityMatch.match); // true

```

## Получение пояснений (AbilityExplain)

Для отладки или аудита может быть полезно понять, *почему* было вынесено то или иное суждение о правах доступа. Метод `resolveWithExplain()` политики возвращает детальную информацию о проверке.

### Использование

```ts
const config: AbilityPolicyConfig = {
  id: 'bb758c1b-1015-4894-ba25-d23156e063cf',
  name: 'Запрещает менять статус заявки с `не обработан` на `завершен` всем, кроме администраторам',
  action: 'order.status',
  effect: 'deny',
  compareMethod: 'and',
  ruleSet: [
    {
      id: '9cc009e5-0aa9-453a-a668-cb3f418ced92',
      name: 'Не администратор',
      compareMethod: 'and',
      rules: [
        {
          id: '4093cd50-e54f-4062-8053-2d3b5966fad3',
          name: 'Нет роли администраторов',
          subject: 'user.roles',
          resource: 'administrator',
          condition: 'not in',
        },
      ],
    },
    {
      id: '2f8f9d71-860b-4fa6-b395-9331f1f0848e',
      name: 'Проверка статуса `не обработан` -> `завершен`',
      compareMethod: 'and',
      rules: [
        {
          id: 'a3c7d66f-5c2d-4a24-83bc-03b0a2d9c32b',
          name: 'Текущий статус `не обработан`',
          subject: 'order.status',
          resource: 'не обработан',
          condition: '=',
        },
        {
          id: 'a3c7d66f-5c2d-4a24-83bc-03b0a2d9c32b',
          name: 'Будущий статус `завершен`',
          subject: 'feature.status',
          resource: 'завершен',
          condition: '=',
        },
      ],
    },
  ],
};

const policy = AbilityPolicy.parse<Resources>(config);
const resolver = new AbilityResolver(policy);
const explain = resolver.resolveWithExplain('order.status', {
  user: {
    roles: ['user', 'couch'],
  },
  order: {
    status: 'не обработан',
  },
  feature: {
    status: 'завершен',
  },
});

explain.forEach((e) => {
  console.debug(e.toString());
});

type Resources = {
  ['order.status']: {
    readonly user: {
      readonly roles: readonly string[];
    };
    readonly order: {
      readonly status: string;
    };
    readonly feature: {
      readonly status: string;
    };
  };
};

```

Результат `console.debug`
```
✓ policy «Запрещает менять статус заявки...» is match
✓ ruleSet «Не администратор» is match
✓ rule «Нет роли администраторов» is match
✓ ruleSet «Проверка статуса...» is match
✓ rule «Текущий статус "не обработан"» is match
✓ rule «Будущий статус "завершен"» is match
```

---

## Группы правил <a name="rule-sets"></a>

**Группы правил** необходимы для объединения нескольких правил в группу. **Основная цель** - выполнить проверку каждого
правила в группе и вернуть лишь один результат.

Создавая группу следует указывать метод сравнения (`compareMethod`), который необходим для вычисления значения всей
группы при проверке правил.

При создании необходимо указать следующие параметры:

- **id** - `string` Уникальный идентификатор.
- **name** - `string` Название группы.
- **compareMethod** - `AbilityCompare` Способ сравнения правил в группе (`or` или `and`).

_Влияние **compareMethod** на результат вычисления группы:_

- **`or`** - Результат всей группы примет значение `match`, если хотя бы одно из правил вернуло `match`.
- **`and`** - Результат всей группы примет значение `match`, если все правила вернули `match`.

### Создание группы правил <a name="ruleset-creation"></a>

Создать группу правил можно двумя способами: создание через конструктор класса и парсинг JSON-конфига группы.

_Создание группы через конструктор класса_:

```ts
import { AbilityRuleSet, AbilityCompare } from '@via-profit/ability';

const ruleSet = new AbilityRuleSet({
  id: '<set-id>',
  name: 'Название группы',
  compareMethod: AbilityCompare.and,
});

// Добавление правил в группу
ruleSet.addRules([
  new AbilityRule(...),
  new AbilityRule(...),
]);


// Сокращённая запись
const ruleSet2 = AbilityRuleSet.and([
  new AbilityRule(...),
  new AbilityRule(...),
]);

```

_Создание группы через парсинг JSON-конфига группы_:

```ts
import { AbilityRuleSet } from '@via-profit/ability';

const ruleSet = AbilityRuleSet.parse({
  'id': '<set-id>',
  'name': 'Название группы',
  'compareMethod': 'and',
  'rules': [
    {
      'id': '<rule-id>',
      'name': 'Пользователь из отдела managers',
      'subject': 'user.department',
      'resource': 'managers',
      'condition': '=',
    },
  ],
});

```

### Проверка группы правил <a name="ruleset-check"></a>

Для проверки группы правил следует вызвать метод `check` класса `AbilityRuleSet` передав объект проверяемого ресурса.
Этот метод вернёт экземпляр класса `AbilityMatch`, при помощи методов которого можно определить имеется ли совпадение
для группы и переданных значений.

```ts
import { AbilityRuleSet, AbilityCompare } from '@via-profit/ability';

const ruleSet = new AbilityRuleSet({
  id: '<set-id>',
  name: 'Название группы',
  compareMethod: AbilityCompare.and,
}).addRules([
  new AbilityRule(...),
  new AbilityRule(...),
]);

const match = rule.check({ ... });

const is = match.isEqual(AbilityMatch.match);
```

___

## Политики <a name="policies"></a>

**Политики** включают в себя группы правил. Основная цель - выполнить проверку всех вложенных групп, сравнить результат
выполнения групп и вернуть один единственный результат.

### Создание политики <a name="policy-creation"></a>

Создать политику можно двумя способами: создание через конструктор класса и парсинг JSON-конфига политики.

При создании политики необходимо указать следующие параметры:

- **id** - `string` Уникальный идентификатор.
- **name** - `string` Название политики.
- **action** - `string` Ключ политики, в формате Dot notation, определяющий схожесть политик. В названии может
  применяться символ звездочки (`*`). Политики с одинаковым экшеном обрабатываются вместе как группа политик. Экшен
  `users.account` не считается похожим с экшеном `users.account.login`, но в это же время `users.account.*` равен экшену
  `users.account.login` (из-за использования звездочки).
- **compareMethod** - `AbilityCompare` Метод сравнения групп правил, входящих в политику (`or` или `and`)
- **effect** - `AbilityPolicyEffect` Определяет итоговый результат всех вычислений (`permit` или `deny`). В слчае
  использования класса `AbilityResolver` (метод `enforce`) последний выкинет исключение `AbilityError`, если политика
  вернёт `deny`. Текст сообщения `AbilityError` будет соответствовать названию сработавшей политики. В остальных случаях
  ничего не произойдет.
- **ruleSet** - `AbilityRuleSet[]` Массив групп (см. [Группы правил](#rule-sets))

**Замечание** - Политика может быть запрещающей (`effect` = `deny`) и разрешающей (`effect` = `permit`). Если вам
необходимо ограничить какой-либо доступ, например, пользователю с недостаточными правами, то следует создавать политику
с эффектом `deny`.

_Создание политики через конструктор класса_:

```ts
import { AbilityPolicy, AbilityCondition } from '@via-profit/ability';

const policy = new AbilityPolicy({
  id: '<policy-id>',
  name: 'Пример политики',
  effect: 'deny',
  action: 'users.update',
  compareMethod: 'and',
  ruleSet: [
    new AbilityRule({
      id: '<rule-id>',
      name: 'Пользователь является владельцем заказа',
      subject: 'user.id',
      resource: 'order.owner',
      condition: AbilityCondition.equal
    })
  ]
});

```

_Создание политики через парсинг JSON-конфига_:

```ts
import { AbilityPolicy } from '@via-profit/ability';

const policy = AbilityPolicy.parse({
  "id": "bb758c1b-1015-4894-ba25-d23156e063cf",
  "name": "Status hui",
  "action": "order.status",
  "effect": "deny",
  "compareMethod": "and",
  "ruleSet": [
    {
      "id": "9cc009e5-0aa9-453a-a668-cb3f418ced92",
      "name": "Не администратор",
      "compareMethod": "and",
      "rules": [
        {
          "id": "4093cd50-e54f-4062-8053-2d3b5966fad3",
          "name": "Нет роли администраторв",
          "subject": "account.roles",
          "resource": "administrator",
          "condition": "<>"
        }
      ]
    }
  ]
});

```

### Проверка политики <a name="policy-check"></a>

Для проверки политики правил следует вызвать метод `check` класса `AbilityPolicy` передав объект проверяемого ресурса.
Этот метод вернёт экземпляр класса `AbilityMatch`, при помощи методов которого можно определить имеется ли совпадение
для группы и переданных значений.

```ts
import { AbilityPolicy } from '@via-profit/ability';

const policy = AbilityPolicy.parse({ ... });

const match = policy.check({ ... });

const is = match.isEqual(AbilityMatch.match);
```

___

Вот обновленный раздел с информацией об использовании звездочки в названиях экшенов:

---

## Управление политиками <a name="policy-management"></a>

Класс `AbilityResolver` — это основной инструмент для применения политик в рантайме. Он решает две ключевые задачи:

1. **Фильтрация политик по действию** — выбирает только те политики, которые применимы к выполняемой операции
2. **Оценка разрешений** — последовательно проверяет отобранные политики и возвращает итоговый результат (разрешено/запрещено)

### Зачем нужен AbilityResolver?

Представим, что в системе есть десятки политик, каждая на своё действие:
- `order.create` — правила создания заказа
- `order.update` — правила обновления заказа
- `order.delete` — правила удаления заказа
- `user.profile.update` — правила обновления профиля
- и так далее...

Когда пользователь пытается создать заказ, нам нужно проверить только политики, связанные с действием `order.create`, игнорируя все остальные. Именно это и делает `AbilityResolver`.

### Использование wildcard (*) в действиях

`AbilityResolver` поддерживает использование символа звездочки (`*`) в названиях действий. Это позволяет создавать политики, которые применяются к целым группам операций.

#### Правила сопоставления с wildcard:

| Политика (action) | Проверяемое действие | Результат |
|-------------------|---------------------|-----------|
| `order.*` | `order.create` | ✅ Совпадает |
| `order.*` | `order.update` | ✅ Совпадает |
| `order.*` | `order.delete` | ✅ Совпадает |
| `order.*` | `user.create` | ❌ Не совпадает |
| `*.create` | `order.create` | ✅ Совпадает |
| `*.create` | `user.create` | ✅ Совпадает |
| `*.create` | `order.update` | ❌ Не совпадает |
| `user.profile.*` | `user.profile.update` | ✅ Совпадает |
| `user.profile.*` | `user.profile.delete` | ✅ Совпадает |
| `user.profile.*` | `user.settings.update` | ❌ Не совпадает |

#### Примеры использования wildcard:

```ts
// Политика, применяемая ко всем действиям с заказами
{
  id: 'orders-audit',
  name: 'Audit all order operations',
  action: 'order.*',  // Применится к order.create, order.update, order.delete и т.д.
  effect: 'permit',
  // ... правила
}

// Политика, применяемая ко всем операциям создания
{
  id: 'create-audit',
  name: 'Audit all create operations',
  action: '*.create',  // Применится к order.create, user.create, product.create и т.д.
  effect: 'permit',
  // ... правила
}

// Политика для всех операций в модуле пользователей
{
  id: 'user-module',
  name: 'User module base policy',
  action: 'user.*.*',  // Применится к user.profile.update, user.settings.delete и т.д.
  effect: 'deny',
  // ... правила
}
```

#### Приоритет и множественное совпадение

Если несколько политик подходят под проверяемое действие, будут применены **все** подходящие политики. Результат определяется последней сработавшей политикой:

```ts
const policies = [
  AbilityPolicy.parse({
    action: 'order.*',
    effect: 'permit',
    // ... правила
  }),
  AbilityPolicy.parse({
    action: 'order.update',
    effect: 'deny',
    // ... правила
  })
];

const resolver = new AbilityResolver(policies);

// При проверке order.update сработают ОБЕ политики
// Результат будет deny, так как это эффект последней сработавшей политики,
// таким образом, каждая последующая политика считается важнее предыдущей.
// Это применительно для ситуаций, когда необходимо, что называется, наложить вето
// на принятые ранее решения вышестоящих политик
resolver.enforce('order.update', data);
```

**Каждая последующая политика считается важнее предыдущей.
Это применительно для ситуаций, когда необходимо, что называется, наложить вето
на принятые ранее решения вышестоящих политик**

#### Комбинирование точных действий и wildcard

Вы можете комбинировать точные действия и wildcard для создания гибкой системы прав:

```ts
const policies = [
  // Общее правило для всех заказов
  {
    action: 'order.*',
    effect: 'deny',  // По умолчанию запрещено
    // ...
  },
  // Исключение для создания заказа
  {
    action: 'order.create',
    effect: 'permit',  // Создавать можно
    // ...
  },
  // Дополнительная проверка для обновления
  {
    action: 'order.update',
    effect: 'deny',  // Обновление требует особых условий
    ruleSet: [
      // ... сложные правила для обновления
    ]
  }
];
```

### Как это работает

```ts
import { AbilityPolicy, AbilityResolver } from '@via-profit/ability';
import type { AbilityPolicyConfig } from '@via-profit/ability';

// Загружаем все политики системы (например, из JSON-файлов)
const configs: AbilityPolicyConfig[] = [
  {
    id: 'order-create-policy',
    name: 'Политика создания заказа',
    action: 'order.create',
    effect: 'permit',
    compareMethod: 'and',
    ruleSet: [
      // ... правила для создания заказа
    ]
  },
  {
    id: 'order-update-policy',
    name: 'Политика обновления заказа',
    action: 'order.update',
    effect: 'deny',
    compareMethod: 'and',
    ruleSet: [
      // ... правила для обновления заказа
    ]
  },
  {
    id: 'orders-base-policy',
    name: 'Базовая политика для всех операций с заказами',
    action: 'order.*',
    effect: 'deny',
    compareMethod: 'and',
    ruleSet: [
      // ... общие правила для всех заказов
    ]
  }
];

// Создаем экземпляры политик
const policies = AbilityPolicy.parseAll(configs);

// Создаем резолвер со всеми политиками
const resolver = new AbilityResolver(policies);

// При выполнении действия указываем только нужный action
// AbilityResolver автоматически отфильтрует политики и проверит их
resolver.enforce('order.create', {
  user: {
    department: 'managers',
    roles: ['manager']
  },
  order: {
    amount: 5000
  }
});
```

### Проверка соответствия действия

Метод `isInActionContain` позволяет проверить, соответствует ли действие шаблону с wildcard:

```ts
// Статический метод класса AbilityResolver
AbilityResolver.isInActionContain('order.*', 'order.create'); // true
AbilityResolver.isInActionContain('order.*', 'user.create');  // false
AbilityResolver.isInActionContain('*.update', 'order.update'); // true
AbilityResolver.isInActionContain('*.update', 'order.create'); // false
```

Этот метод используется внутри `AbilityResolver` для фильтрации политик, но может быть полезен и в пользовательском коде.

### Методы AbilityResolver

#### `enforce()` — строгая проверка

```typescript
try {
  resolver.enforce('order.update', data);
  // Доступ разрешен - продолжаем выполнение
  await updateOrder(data);
} catch (error) {
  if (error instanceof AbilityError) {
    // Доступ запрещен - error.message содержит название сработавшей политики
    console.error(`Доступ запрещен политикой: ${error.message}`);
    return;
  }
  throw error;
}
```

**Важно**: `enforce()` выбрасывает исключение, если хотя бы одна подходящая политика вернула `deny`. Если ни одна политика не сработала или все вернули `permit` — исключения не будет.

#### `resolve()` — мягкая проверка

```typescript
const result = resolver
  .resolve('order.update', data)
  .isDeny(); // true если доступ запрещен

if (result) {
  // Самостоятельно обрабатываем запрет
  return { error: 'Access denied' };
}

// Продолжаем выполнение
await updateOrder(data);
```

#### `resolveWithExplain()` — проверка с объяснением

```typescript
const explanations = resolver.resolveWithExplain('order.update', data);

explanations.forEach(explain => {
  console.log(explain.toString());
  // ✓ policy «Политика обновления заказа» is match
  //   ✗ ruleSet «Проверка владельца» is mismatch
  //   ✓ ruleSet «Проверка статуса» is match
});

if (resolver.isDeny()) {
  console.log('Доступ запрещен по причине:');
  explanations.forEach(e => console.log(e.toString()));
}
```

### Интеграция с TypeScript

Для полной типобезопасности определите интерфейс `Resources`, где ключи — это возможные действия, а значения — структура данных, требуемая для проверки:

```typescript
// Определяем типы данных для каждого действия
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
    };
    readonly order: {
      readonly id: string;
      readonly status: string;
      readonly ownerId: string;
    };
  };
  
  'user.profile.update': {
    readonly user: {
      readonly id: string;
    };
    readonly profile: {
      readonly userId: string;
    };
  };
};

// Теперь TypeScript будет проверять корректность передаваемых данных
resolver.enforce('order.create', {
  user: {
    department: 'managers',
    roles: ['manager']
  },
  order: {
    amount: 5000  // ✅ все поля на месте
  }
});

// ❌ Ошибка компиляции - не хватает required полей
resolver.enforce('order.create', {
  user: {
    department: 'managers'
    // missing roles
  }
});
```

### Как формируется итоговое решение?

1. Resolver собирает все политики, у которых `action` соответствует запрошенному (поддерживается wildcard `*`)
2. Последовательно проверяет каждую политику методом `check()`
3. Если политика вернула `match`, запоминает её `effect` (permit/deny)
4. Возвращается `effect` **последней сработавшей политики**

```typescript
// Пример с несколькими политиками на одно действие
const policies = [
  permitPolicy, // permit, но не match
  denyPolicy,   // deny и match
  permitPolicy2 // permit, но не match
];

resolver.enforce('some.action', data);
// Результат: deny (от последней сработавшей политики)
```

### Когда использовать Resolver?

- **В API endpoints** — проверка прав перед выполнением операции
- **В middleware** — централизованная проверка доступа
- **В сервисах** — защита бизнес-логики
- **В клиентском коде** — условный рендеринг UI на основе прав

Resolver делает систему прав предсказуемой, типобезопасной и легко расширяемой.

## API Reference <a name="api-reference"></a>

### Класс AbilityCode

Базовый абстрактный класс для всех кодовых значений в системе.

| Метод | Аргументы | Возвращаемое значение | Описание |
|-------|-----------|----------------------|----------|
| `isEqual()` | `compareWith: AbilityCode<T> \| null` | `boolean` | Сравнивает текущий код с другим экземпляром |
| `isNotEqual()` | `compareWith: AbilityCode<T> \| null` | `boolean` | Проверяет неравенство с другим экземпляром |

**Геттеры:**
- `code: T` - возвращает сырое значение кода (строку или число)

---

### Класс AbilityMatch

Представляет возможные состояния результата проверки правил.

**Статические свойства:**
- `AbilityMatch.pending` - ожидание проверки
- `AbilityMatch.match` - совпадение найдено
- `AbilityMatch.mismatch` - совпадение не найдено

Каждое свойство является экземпляром класса `AbilityMatch` и наследует все его методы.

---

### Класс AbilityCondition

Определяет операторы сравнения для правил доступа.

**Статические свойства:**
- `AbilityCondition.equal` - равно (`=`)
- `AbilityCondition.not_equal` - не равно (`<>`)
- `AbilityCondition.more_than` - больше (`>`)
- `AbilityCondition.less_than` - меньше (`<`)
- `AbilityCondition.less_or_equal` - меньше или равно (`<=`)
- `AbilityCondition.more_or_equal` - больше или равно (`>=`)
- `AbilityCondition.in` - входит в массив (`in`)
- `AbilityCondition.not_in` - не входит в массив (`not in`)

| Метод | Аргументы | Возвращаемое значение | Описание |
|-------|-----------|----------------------|----------|
| `fromLiteral()` | `literal: AbilityConditionLiteralType` | `AbilityCondition` | Создает экземпляр условия из литерального имени (например, 'equal' → '=') |

**Геттеры:**
- `literal: AbilityConditionLiteralType` - возвращает литеральное имя оператора ('equal', 'not_equal' и т.д.)

---

### Класс AbilityCompare

Определяет методы логического сравнения для групп правил.

**Статические свойства:**
- `AbilityCompare.and` - логическое И (все правила должны совпасть)
- `AbilityCompare.or` - логическое ИЛИ (достаточно одного совпадения)

---

### Класс AbilityPolicyEffect

Определяет эффект применения политики.

**Статические свойства:**
- `AbilityPolicyEffect.deny` - запрет доступа
- `AbilityPolicyEffect.permit` - разрешение доступа

---

### Класс AbilityRule

Представляет отдельное правило проверки доступа.

**Свойства:**
- `subject: string` - путь к значению субъекта в dot-нотации
- `resource: string | number | boolean | (string | number)[]` - значение или путь для сравнения
- `condition: AbilityCondition` - условие сравнения
- `name: string` - название правила
- `id: string` - уникальный идентификатор
- `state: AbilityMatch` - текущее состояние после вызова `check()`

| Метод | Аргументы | Возвращаемое значение | Описание |
|-------|-----------|----------------------|----------|
| `check()` | `resource: Resources \| null` | `AbilityMatch` | Проверяет правило на переданных данных, обновляет `state` и возвращает результат |
| `extractValues()` | `resourceData: Resources \| null` | `[any, any]` | Извлекает значения для сравнения из субъекта и ресурса по указанным путям |
| `getDotNotationValue()` | `resource: unknown, desc: string` | `T \| undefined` | Извлекает значение из объекта по dot-нотации (поддерживает массивы: `users[0].name`) |
| `export()` | — | `AbilityRuleConfig` | Экспортирует правило в конфигурационный объект для сериализации |
| `parse()` | `config: AbilityRuleConfig` | `AbilityRule` | Статический метод. Создает экземпляр правила из конфигурационного объекта |

**Статические фабричные методы (все возвращают `AbilityRule`):**

| Метод | Аргументы | Описание |
|-------|-----------|----------|
| `equal()` | `subject: string, resource: any` | Создает правило с условием "равно" |
| `notEqual()` | `subject: string, resource: any` | Создает правило с условием "не равно" |
| `in()` | `subject: string, resource: any` | Создает правило с условием "входит в массив" |
| `notIn()` | `subject: string, resource: any` | Создает правило с условием "не входит в массив" |
| `lessThan()` | `subject: string, resource: any` | Создает правило с условием "меньше" |
| `lessOrEqual()` | `subject: string, resource: any` | Создает правило с условием "меньше или равно" |
| `moreThan()` | `subject: string, resource: any` | Создает правило с условием "больше" |
| `moreOrEqual()` | `subject: string, resource: any` | Создает правило с условием "больше или равно" |

---

### Класс AbilityRuleSet

Группирует несколько правил для совместной проверки.

**Свойства:**
- `state: AbilityMatch` - текущее состояние группы после вызова `check()`
- `rules: AbilityRule[]` - массив правил в группе
- `compareMethod: AbilityCompare` - метод сравнения (AND/OR)
- `name: string` - название группы
- `id: string` - уникальный идентификатор

| Метод | Аргументы | Возвращаемое значение | Описание |
|-------|-----------|----------------------|----------|
| `addRule()` | `rule: AbilityRule` | `this` | Добавляет одно правило в группу (поддерживает цепочку вызовов) |
| `addRules()` | `rules: AbilityRule[]` | `this` | Добавляет массив правил в группу |
| `check()` | `resources: Resources \| null` | `AbilityMatch` | Проверяет все правила группы, применяет метод сравнения и возвращает результат |
| `export()` | — | `AbilityRuleSetConfig` | Экспортирует группу в конфигурационный объект |
| `parse()` | `config: AbilityRuleSetConfig` | `AbilityRuleSet` | Статический метод. Создает экземпляр группы из конфигурации |
| `and()` | `rules: AbilityRule[]` | `AbilityRuleSet` | Статический метод. Создает группу с логическим И |
| `or()` | `rules: AbilityRule[]` | `AbilityRuleSet` | Статический метод. Создает группу с логическим ИЛИ |

---

### Класс AbilityPolicy

Объединяет группы правил в полноценную политику доступа.

**Свойства:**
- `matchState: AbilityMatch` - состояние политики после проверки
- `ruleSet: AbilityRuleSet[]` - массив групп правил
- `effect: AbilityPolicyEffect` - эффект политики (permit/deny)
- `compareMethod: AbilityCompare` - метод сравнения групп
- `name: string` - название политики
- `id: string` - уникальный идентификатор
- `action: string` - действие, к которому применяется политика

| Метод | Аргументы                        | Возвращаемое значение | Описание |
|-------|----------------------------------|-----------------------|----------|
| `addRuleSet()` | `ruleSet: AbilityRuleSet`        | `this`                | Добавляет группу правил в политику |
| `check()` | `resources: Resources`           | `AbilityMatch`        | Проверяет все группы правил и возвращает результат |
| `explain()` | —                                | `AbilityExplain`      | Возвращает объяснение результата проверки (должен быть вызван после `check()`) |
| `export()` | —                                | `AbilityPolicyConfig` | Экспортирует политику в конфигурационный объект |
| `parse()` | `config: AbilityPolicyConfig`    | `AbilityPolicy`       | Статический метод. Создает экземпляр политики из конфигурации |
| `parseAll()` | `configs: readonly AbilityPolicyConfig[]	` | `AbilityPolicy[]`     | Статический метод. Парсит массив конфигураций политик и возвращает массив экземпляров AbilityPolicy. Удобен для загрузки нескольких политик одновременно. |

---

### Класс AbilityResolver

Управляет множеством политик и их выполнением.

| Метод | Аргументы | Возвращаемое значение | Описание |
|-------|-----------|----------------------|----------|
| `constructor()` | `policies: AbilityPolicy[] \| AbilityPolicy` | `AbilityResolver` | Создает экземпляр с одной или несколькими политиками |
| `resolve()` | `action: keyof Resources, resource: Resources[Action]` | `this` | Фильтрует политики по действию и проверяет их, возвращает себя для цепочки вызовов |
| `resolveWithExplain()` | `action: keyof Resources, resource: Resources[Action]` | `readonly AbilityExplain[]` | Выполняет проверку и возвращает массив объяснений для каждой политики |
| `enforce()` | `action: keyof Resources, resource: Resources[Action]` | `void \| never` | Выполняет проверку и выбрасывает `AbilityError` если результат Deny |
| `getEffect()` | — | `AbilityPolicyEffect \| null` | Возвращает эффект последней сработавшей политики |
| `isPermit()` | — | `boolean` | Проверяет, разрешен ли доступ |
| `isDeny()` | — | `boolean` | Проверяет, запрещен ли доступ |
| `getMatchedPolicy()` | — | `AbilityPolicy \| null` | Возвращает последнюю сработавшую политику |
| `isInActionContain()` | `actionA: string, actionB: string` | `boolean` | Статический метод. Проверяет, соответствует ли действие шаблону (поддерживает `*`) |

---

### Класс AbilityExplain

Представляет человекочитаемое объяснение результата проверки.

**Свойства:**
- `type: AbilityExplainType` - тип элемента ('policy' \| 'rule' \| 'ruleSet')
- `children: AbilityExplain[]` - дочерние элементы объяснения
- `name: string` - название элемента
- `match: AbilityMatch` - результат проверки

| Метод | Аргументы | Возвращаемое значение | Описание |
|-------|-----------|----------------------|----------|
| `toString()` | `indent: number = 0` | `string` | Форматирует объяснение в читаемый текст с отступами |

**Наследники:**
- `AbilityExplainRule` - объяснение для правила
- `AbilityExplainRuleSet` - объяснение для группы правил
- `AbilityExplainPolicy` - объяснение для политики

---

### Класс AbilityParser

Предназначен для парсинга и генерации типов из конфигураций.

Вот обновленный раздел для README.md с актуальной информацией о классе `AbilityParser`:

---

### Класс AbilityParser

Предназначен для парсинга и генерации TypeScript типов из конфигураций политик.

| Метод                | Аргументы                            | Возвращаемое значение | Описание                                                                                                                                                                                                                                                |
|----------------------|--------------------------------------|-----------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `generateTypeDefs()` | `policies: readonly AbilityPolicy[]` | `string`              | Статический метод. Генерирует TypeScript тип `Resources` на основе правил во всех политиках. Анализирует условия правил и определяет соответствующие типы (string, number, boolean, массивы). Возвращает строку с готовым TypeScript определением типа. |

#### Пример использования:

```typescript
import { AbilityParser, AbilityPolicy } from '@via-profit/ability';
import fs from 'node:fs';

// Массив политик
const policies: AbilityPolicy[] = AbilityPolicy.parseAll([
  {
    id: 'policy-1',
    name: 'Example policy',
    action: 'order.status',
    effect: 'deny',
    compareMethod: 'and',
    ruleSet: [
      {
        id: 'rule-set-1',
        name: 'Example rule set',
        compareMethod: 'and',
        rules: [
          {
            subject: 'user.roles',
            resource: ['admin'],
            condition: 'in',
          },
          {
            subject: 'order.amount',
            resource: 1000,
            condition: '<=',
          },
        ],
      },
    ],
  },
]);

// Генерация TypeScript типа
const typeDefinitions = AbilityParser.generateTypeDefs(policies);

// Запись в файл
fs.writeFileSync('./src/types/ability.ts', typeDefinitions);

// Результат в файле:
// // Automatically generated by via-profit/ability
// // Do not edit manually
//
// export type Resources = {
//   ['order.status']: {
//     readonly order: {
//       readonly amount: number;
//     };
//     readonly user: {
//       readonly roles: string[];
//     };
//   };
// }
```

---

### Классы ошибок

| Класс | Назначение |
|-------|------------|
| `AbilityError` | Общая ошибка доступа, выбрасывается при запрете в `enforce()` |
| `AbilityParserError` | Ошибка парсинга конфигурации или генерации типов |

## Рекомендации по использованию <a name="best-practices"></a>

### Именование экшенов

Используйте точечную нотацию для иерархии действий:

- `order.create` - создание заказа
- `order.update` - обновление заказа
- `order.status.update` - обновление статуса заказа

### Структура данных

Старайтесь группировать связанные данные под общими ключами:

```ts
// Хорошо
{
  user: { id: '123', roles: ['admin'] },
  order: { status: 'new', amount: 1000 }
}

// Плохо
{
  userId: '123',
  userRoles: ['admin'],
  orderStatus: 'new',
  orderAmount: 1000
}
```

### Проектирование политик

Используйте `deny` для запрещающих политик, `permit` для разрешающих.

Комбинируйте простые правила для сложной логики.

Давайте понятные имена правилам и политикам для упрощения отладки.


## Отладка политик <a name="debugging"></a>

Используйте `resolveWithExplain()` для получения детальной информации о процессе проверки:

```ts
const explanations = resolver.resolveWithExplain('order.update', data);
explanations.forEach(exp => console.log(exp.toString()));
// ✓ policy «Запрет доступа для менеджеров» is match
//   ✓ ruleSet «Менеджеры» is match
//     ✓ rule «Отдел managers» is match
//     ✗ rule «Роль manager» is mismatch
//   ✓ ruleSet «Не администраторы» is match
//     ✓ rule «Нет роли administrator» is match
```
