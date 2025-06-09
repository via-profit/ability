# @via-profit/Ability

> Набор сервисов, частично реализующих
> принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)

Этот сервис позволяет создавать правила и политики, применять их к данным и проверять доступ на их основе.

## Содержание

## Оглавление
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
    - [Создание политики](#policy-creattion)
    - [Проверка политики](#policy-check)
- [Управление политиками](#policy-management)


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
- **`AbilityParser`** — парсер конфигурационных правил (JSON)
- **`AbilityError`** — инстанс ошибок

### Основные принципы <a name="principles"></a>

Работа сервиса основана на формировании **правил**, объединении их в **политики** и проверке доступа с их помощью.

Пример: необходимо **запретить доступ** пользователям, связанным с отделом менеджеров, **за исключением администраторов**.

- Менеджеры — если их отдел `managers` или есть роль `manager`
- Администраторы — пользователи с ролью `administrator`

Структура политики:

![ability-01.png](data:image/png;base64,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)
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
  `user.name` или значение, которое может быть строкой, числом, булеан значением или массивом строк или чисел.

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

### Создание политики <a name="policy-creattion"></a>

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

## Управление политиками <a name="policy-management"></a>

Для управления политиками реализован специальный класс `AbilityResolver`.

В случае, если вам необходимо запустить лишь разовую проверку данных, то данный раздел можно опустить.

**AbilityResolver** необходим для возможности запуска проверки разных политик в разный период времени.

Политики содержат название экшена (поле `action`) определяемого разработчиком. Запуск метода `enforce` или `resolve`
отберет из всех переданных политик только те, которые попадают под указанный экшен.

```ts
import { AbilityPolicy, AbilityPolicyConfig, AbilityResolver } from './AbilityPolicy';

const config: AbilityPolicyConfig[] = [...]; // массив различных политик (JSON)
const policies: AbilityPolicy<Resources>[] = config.map(cfg => AbilityPolicy.parse(cfg)); // массив уже созданных политик

// Проверка политик  с экшеном `order.create`
// Варинат 1. Будет выброшено исключение AbilityError
// c название политики, которая вернула deny,
// либо ничего не произойдет, если ни одна из политик
// не вернет deny
new AbilityResolver(policies).enforce('order.create', {
  user: { department: 'managers' },
});

// Вариант 2.
const isDeny = new AbilityResolver(policies)
  .resolve('order.create', {
    user: { department: 'managers' },
  })
  .isDeny();

if (isDeny) {
  throw new AbilityError('Permission denied');
}


// Типы ресурсов, где каждый ключ будет являться название экшена
type Resources = {
  ['order.status']: { // <-- название экшена
    readonly account: { // <-- данные ресурса
      readonly roles: readonly string[];
    };
  };
  ['order.create']: {
    readonly user: {
      readonly department: string;
    };
  };
  ...
};

```

_Пояснение примера выше. В данном примере создается массив всех политик, а затем запускается проверка политик подходящих
по указанному экшену, а самое главное, что при помощи типа `Resources`, который необходимо формировать
вручную, **TypeScript** подскажет какие именно данные следует передать вторым аргументом (ресурс)._

