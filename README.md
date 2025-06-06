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

Пример: необходимо **запретить доступ** пользователям, связанным с отделом менеджеров, **за исключением администраторов
**.

- Менеджеры — если их отдел `managers` или есть роль `manager`
- Администраторы — пользователи с ролью `administrator`

Структура политики:

![ability-01.drawio.png](assets/ability-01.drawio.png)

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

## Управление политиками

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

