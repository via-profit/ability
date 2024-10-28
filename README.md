# @via-profit/Ability

> Набор сервисов, частичyо реализующих принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)

## Содержание

1.  [Описание и общие принципы](#overview)

2.  [Правила](#rules)

    2.1 [Синтаксис правил](#rule-syntax)

    2.2 [Примеры](#rule-recipes)

    2.3 [Класс AbilityRule](#ability-rule-class)

3.  [Политики](#policies)

    3.1 [Синтаксис правил](#policy-syntax)

    3.2 [Примеры](#policy-recipes)

    3.3 [Класс AbilityPolicy](#ability-policy-class)

4.  [Создание политики из конфига](#policy-config)

## Описание и общие принципы <a name="overview"></a>

Данный сервис позволяет создать правила или политики, а затем применить их по отношению к каким-либо данным для того чтобы проверить наличие доступа к этим данным.

Принцип работы основан на формировании правил, объединения их в политики и запуске политик и/или правил. Правила можно группировать в политики, а политики могут содержать вложенные политики.

После создания правил и политик их необходимо запустить для проверки соответствия передаваемым данным. Если переданные данные соответствуют всем правилам политики, то такая политика считается разрешенной.

## Правила <a name="rules"></a>

Предположим в системе имееются следующие данные: пользователь; отчеты.

```ts
// Пользователь
const user = {
  id: '123',
  name: 'Oleg',
  age: 26,
  departament: 'analytics',
};

// Пользовательские отчёты
const reports = [
  {
    id: '1',
    type: 'analytics',
  },
  {
    id: '2',
    type: 'expenses',
  },
];
```

Пусть необходимо создать правило, которое будет разрешать доступ пользователю только к тем отчетам, тип которых соответствует его отделу. Тогда в правило будет записано что субъект имеет поле `departament` и оно должно быть равно значению поля `type` ресурса:

```ts
import { AbilityRule } from '@via-profit/ability';

// Создание правила
const rule = new AbilityRule('Пользователь должен быть из отдела аналитики', [
  'subject.departament', // субъект и адрес поля, в которое записано название отдела
  '=', // оператор сравнения
  'resource.type', // Ресурс и адрес поля, в которое записан тип отчета
]);
```

Адрес поля `subject.departament`, представляет собой запись в формате [dot notation](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Basics#dot_notation), которая обязательно должна начинаться с `subject`, что указывает, что для сравнения данных будет использоваться поле departament именно у субъекта (в данном примере субъект - это объект пользователя).
Для сравнения двух отделов будет использоваться оператор сравнения `=`.
Адрес поля `resource.type` тоже представляет собой декларацию пути с префиксом `resource` в формате [dot notation](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Basics#dot_notation).

Теперь, для того чтобы проверить правило, необходимо выполнить метод `check` передав необходимые субъект и ресурс. В данном примере субъект - это объект с данными пользователя, а ресурс - это один из отчетов.

```ts
const report = reports[0];
const isPermit = rule.check(user, report); // true
```

Метод `check` класса `AbilityRule` вернёт `permit` в случае, если переданные **user** и **report** отвечают требованием правила и `deny` в противном случае.

Полный листинг проверки правила:

```ts
import { AbilityRule } from '@via-profit/ability';

// Создание правила
const rule = new AbilityRule('Пользователь должен быть из отдела аналитики', [
  'subject.departament', // субъект и адрес поля, в которое записано название отдела
  '=', // оператор сравнения
  'resource.type', // Ресурс и адрес поля, в которое записан тип отчета
]);

// Пользователь
const user = {
  id: '123',
  name: 'Oleg',
  age: 26,
  departament: 'analytics',
};

// Пользовательские отчёты
const reports = [
  {
    id: '1',
    type: 'analytics',
  },
  {
    id: '2',
    type: 'expenses',
  },
];

rule.check(user, reports[0]); // --> вернет true
rule.check(user, reports[1]); // --> вернет false, так как user.departament не соответствует значение поля type отчета
```

### Синтаксис правил <a name="rule-syntax"></a>

Для описания правил используется массив следующего типа:

```ts
type AbilityRuleMatches = [
  string, // dot notation путь до поля в субъекте или энвайронменте
  '=' | '<>' | '>' | '<' | '<=' | '>=' | 'in', // оператор сравнения
  string | number | boolean, // dot notation путь до поля в ресурсе или энвайронменте или просто значение
];
```

### Операторы сравнения <a name="rule-operators"></a>

`=` Прямое сравнение
`<>` Не равно
`>` Больше
`<` Меньше
`<=` Меньше или равно
`>=` Больше или равно
`in` Вхождение в массив. Позволяет проверять вхождение значения в массив

### Примеры правил <a name="rule-recipes"></a>

<details>
<summary>Пользователь старше 21 года</summary>

```ts
const user = {
  age: 18,
};

const isPermit = new AbilityRule('User age', ['subject.age', '>=', 21]).isPermit(user); // true
```

</details>

<details>
<summary>Пользователь имеет роль администратора</summary>

```ts
const user = {
  roles: ['administrator', 'manager'],
};

const isPermit = new AbilityRule('has role', ['subject.roles', 'in', 'administrator']).isPermit(
  user,
); // true
```

</details>

<details>
  <summary>Пользователь является владельцем заказа</summary>

```ts
const user = {
  id: '1',
};

const order = {
  owner: '1',
};

const isPermit = new AbilityRule('owner', ['subject.id', '=', 'resource.owner']).isPermit(
  user,
  order,
); // true
```

</details>

## Политики <a name="policies"></a>

Политики позволяют группировать правила или создавать вложенные друг в друга политики.

_Замечание: Одна политика может содержать либо правила, либо иметь вложенные политики. Одновременно иметь и правила и вложенные политики невозможно._

Предположим, необходимо создать правило, которое будет разрешать доступ к финансовому отчету только пользователю из отдела аналитики (`analytics`) и только при условии, что он является его владельцем. Таким образом нам необходимо сформировать политику, состоящую из двух обязательных правил.

_Псевдокод:_

```

Политика (разрешить), если:
  Правило 1 — пользователь.departament = analytics
 (и)
  Правило 2 — отчет.владелец = пользователь.id

```

_Реализация:_

```ts
import { Abilitypolicy, AbilityRule } from '@via-profit/ability';

const user = {
  id: '1655',
  departament: 'analytics',
};

const report = {
  id: '6',
  owner: '1655',
};

// Создание первого правила
const departamentRule = new AbilityRule('Пользователь должен быть из отдела аналитики', [
  'subject.departament',
  '=',
  'analytics',
]);

// Создание второго правила
const orderOwner = new AbilityRule('Пользователь должен быть владельцем отчета', [
  'subject.id',
  '=',
  'resource.owner',
]);

// Создание политики
const policy = new AbilityPolicy('Доступ к отчету', '1');

// Добавление правил в политику
// Второй аргумент («and») устанавливает, что
// политика будет разрешена только если переданные
// данные будут соответствовать обеим правилам сразу.
// Альтернативный вариант - «or». В таком случае, политика
// разрешится, если будет удовлетворено хотя бы одно правило
policy.addRules([departamentRule, orderOwner], 'and');

// запуск политики
// если данные, переданные в политику удовлетворяют ее правилам,
// то политика позволит выполниться коду, который следует за ней.
// в противном случае будет брошего исключение с сообщением
// о запрете доступа и перечислением правил, которые были нарушены
policy.enforce(user, report);

// какой-то код
```

### Вложенные политики

Политики могут состоять не из правил, а из других политик.

_Замечание: Одна политика может содержать либо правила, либо иметь вложенные политики. Одновременно иметь и правила и вложенные политики невозможно._

Например, Пользователь может установить статус заказа на «подтвержденный», но только в случаях, если: предыдущий статус заказа был «новый заказ», устанавливаемый статус заказа будет «подтвержденный заказ» и пользователь относится к отделу «manager» или в случае, если пользователь является старшим администратором

_Псевдокод:_

```

Политика (разрешить), если:
  Политика 1
    Правило 1 — пользователь.departament = managers
    (и)
    Правило 2 — заказ.предыдущий статус = новый заказ
    (и)
    Правило 3 — заказ.новый статус = подтвержденный заказ
  (или)
  Политика 2
   Правило 1 — пользователь.роль = administrator

```

_Реализация:_

Для создания политики будет задействован механизм парсинга конфиигурацонного файла. Подробнее см. раздел [Создание политики из конфига](#policy-config)

```ts
import { Abilitypolicy, AbilityRule } from '@via-profit/ability';

const user = {
  id: '1655',
  departament: 'manager',
  roles: ['super-admin', 'viewer'],
};

const order = {
  id: '6',
  status: 'новый заказ',
};

const policy = AbilityPolicy.parse({
  name: 'Политика',
  id: '1',
  policiesCompareMethod: 'or',
  policies: [
    {
      id: '2',
      name: 'Только менеджер может сменить статус с «новый заказ» на «подтвержденный заказ»',
      rulesCompareMethod: 'and',
      rules: [
        {
          name: 'Пользователь должен быть из отдела менеджеров',
          matches: ['subject.departament', '=', 'managers'],
        },
        {
          name: 'Предыдущий статус должен быть «новый заказ»',
          matches: ['environment.prevStatus', '=', 'новый заказ'],
        },
        {
          name: 'Устанавливаемый статус должен быть «подтвержденный заказ»',
          matches: ['environment.nextStatus', '=', 'подтвержденный заказ'],
        },
      ],
    },
    {
      id: '3',
      name: 'Пользователь должен быть старшим администратором',
      rules: [
        {
          name: 'Пользователь должен быть старшим администратором',
          matches: ['subject.rules', 'in', 'super-admin'],
        },
      ],
    },
  ],
});

policy.enforce(user, order, {
  prevStatus: order.status,
  nextStatus: 'подтвержденный заказ',
});
```

## Создание политики из конфига <a name="policy-config"></a>

Политику и правила можно создавать не только по средствам классов, но и при помощи конфигураций.

Структура конфигураций правила:

```ts
type AbilityRuleConfig = {
  readonly name: string;
  readonly effect?: AbilityRuleStatus;
  readonly matches: AbilityRuleMatches;
};

type AbilityRuleMatches = [
  `${SubjectPrefix}${string}`,
  '=' | '<>' | '>' | '<' | '<=' | '>=' | 'in'
  string | number | boolean,
];

```

Структура конфигураций политики:

```ts
type AbilityPolicyConfig = {
  readonly id: string;
  readonly name: string;
  readonly description?: string;
  readonly rulesCompareMethod?: 'or' | 'and';
  readonly policiesCompareMethod?: 'or' | 'and';
  readonly rules?: AbilityRuleConfig[] | null;
  readonly policies?: AbilityPolicyConfig[] | null;
};
```

Пример создания простой политики через конфигурацию:

```ts
import { AbilityPolicy } from '@via-profit/ability';

const policy = AbilityPolicy.parse({
  name: 'Название политики',
  id: '1',
  policiesCompareMethod: 'or',
  policies: [
    {
      id: '2',
      name: 'Название вложенной политики',
      rulesCompareMethod: 'and',
      rules: [
        { name: 'Правило 1', matches: ['subject.id', '=', 'resource.creatable'] },
        { name: 'Правило 2', matches: ['environment.prevStatus', '=', 'unknown'] },
      ],
    },
  ],
});
```


## Класс AbilityRule <a name="ability-rule-class"></a>

Класс `AbilityRule` предназначен для создания правил


## Класс AbilityPolicy <a name="ability-policy-class"></a>

Класс `AbilityPolicy` предназначен для создания политик

Метод `enforce`. В качестве текста сообщения об ошибке доступа, будет возвращено название политики, причем, только первой, политики, которая не прошла проверку.


### Синтаксис политик <a name="policy-syntax"></a>
