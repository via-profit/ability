# Domain-Specific Language

Ability DSL — это декларативный язык для описания политик доступа.  
Он позволяет определять правила в человекочитаемой форме, используя простые конструкции: *политики*, *группы*, *правила* и *аннотации*.



##  Структура политики

Политика состоит из:

```
<effect> <permission> if <all|any>:
  <group>...
```

Где:

- **effect** — `permit` или `deny`
- **permission** — строка вида `permission.foo.bar`
- **if all:** — все группы должны быть истинны
- **if any:** — хотя бы одна группа должна быть истинна

Пример:

```dsl
permit permission.order.update if any:
  all of:
    user.roles contains 'admin'
    user.token is not null

  any of:
    user.roles permission.contains 'developer'
    user.logit is equals 'dev'
```

Пример политики выше гласит - разрешение permission.order.update будет разрешено при выполнении одного из двух условий:
1. user.roles содержит 'admin' **и** user.token не null
2. user.roles содержит 'developer' **или** user.login равен 'dev'


## Комментарии

Строки, начинающиеся с символа ’#’ считаются комментариями и не влияют на результат работы правил и политик.

---

## Аннотации

В настоящий момент поддерживается только одна аннотация ’name’, которая будет использована в качестве имени для политики, либо группы правил, либо правила.

Аннотации задаются через комментарии:

```
# @name <имя>
```

Аннотации применяются к **следующей сущности**:

- политике
- группе
- правилу

Пример:

```dsl
# @name can order update
permit order.update if any:
  # @name authorized admin
  all of:
    # @name contains role admin
    user.roles contains 'admin'
```

---

## Группы правил

Группа определяет, как объединяются правила внутри неё:

```
all of:
  <rule>
  <rule>

any of:
  <rule>
  <rule>
```

- `all of:` — логическое AND
- `any of:` — логическое OR

’all of’ - значит, что группа считается выполненной, если все правила внутри группы сработали.

‘any of’ - значит, что группа считается выполненной, если хотя бы одно правило внутри группы сработало.

Каждая группа внутри политики будет вычисляться независимо от других групп. Итоговая оценка результата будет определена путем сравнения результата вычисления всех групп в политике.


Группы могут иметь аннотации:

```dsl
# @name developer group
any of:
  user.roles contains 'developer'
```

---

## Правила

Правило - это ключевая единица политик. Именно правила определяют ключ разрешения в политике. С помощью правил задаются условия по которым определяется эффективность политики (‘permit’ или ‘deny’)

Правило имеет форму:

```
<subject> <operator> <value?>
```

### Subject (субъект)

Идентификатор в dot‑нотации:

```
user.roles
env.time.hour
order.total
```

### Operators (операторы)

Поддерживаются:

Конечно, Vasily — вот аккуратная, компактная и удобная **таблица операторов DSL**, идеально подходящая для README.  
Она отражает все стандартные операторы, которые обычно встречаются в твоём DSL, и даёт короткое, понятное описание.

---

### Таблица всех операторов

**Базовые операторы сравнения**

| Оператор DSL | Синонимы | Пример | Описание | Типы |
|--------------|----------|--------|----------|------|
| **is equals** | `=`, `==`, `equals` | `age is equals 18` | Строгое равенство | number, string, boolean |
| **is not equals** | `!=`, `<>`, `not equals` | `role is not equals 'admin'` | Строгое неравенство | number, string, boolean |
| **greater than** | `>`, `gt` | `age greater than 18` | Больше | number, date |
| **greater than or equal** | `>=`, `gte` | `age greater than or equal 18` | Больше или равно | number, date |
| **less than** | `<`, `lt` | `age less than 18` | Меньше | number, date |
| **less than or equal** | `<=`, `lte` | `age less than or equal 18` | Меньше или равно | number, date |


**Null‑операторы**

| Оператор DSL | Синонимы | Пример | Описание | Типы |
|--------------|----------|--------|----------|------|
| **is null** | `== null`, `= null` | `middleName is null` | Значение отсутствует | any |
| **is not null** | `!= null` | `middleName is not null` | Значение присутствует | any |

**Операторы для списков (массивов)**

| Оператор DSL | Синонимы                  | Пример | Описание | Типы |
|--------------|---------------------------|--------|----------|------|
| **in [...]** | -                         | `role in ['admin', 'manager']` | Значение входит в список | number, string |
| **not in [...]** | -                         | `role not in ['banned']` | Значение не входит | number, string |
| **contains** | `includes`, `has`         | `tags contains 'vip'` | Массив содержит элемент | array |
| **not contains** | `not includes`, `not has` | `tags not contains 'vip'` | Массив не содержит элемент | array |


**Строковые операторы**

| Оператор DSL | Синонимы | Пример | Описание | Типы |
|--------------|----------|--------|----------|------|
| **starts with** | `begins with` | `email starts with 'admin@'` | Строка начинается с | string |
| **not starts with** | — | `email not starts with 'test'` | Строка не начинается с | string |
| **ends with** | — | `email ends with '.ru'` | Строка заканчивается на | string |
| **not ends with** | — | `email not ends with '.com'` | Строка не заканчивается на | string |
| **includes** | `contains substring` | `name includes 'lex'` | Строка содержит подстроку | string |
| **not includes** | — | `name not includes 'test'` | Строка не содержит подстроку | string |

**Булевые операторы**

| Оператор DSL | Синонимы | Пример | Описание | Типы |
|--------------|----------|--------|----------|------|
| **is true** | `= true` | `isActive is true` | Значение истинно | boolean |
| **is false** | `= false` | `isActive is false` | Значение ложно | boolean |

**Операторы длины**

| Оператор DSL | Синонимы | Пример | Описание | Типы |
|--------------|----------|--------|----------|------|
| **length equals** | `len =` | `tags length equals 3` | Длина равна | array, string |
| **length greater than** | `len >` | `tags length greater than 2` | Длина больше | array, string |
| **length less than** | `len <` | `tags length less than 5` | Длина меньше | array, string |

### Value (значение)

Поддерживаются:

- строки `'text'`
- числа `42`
- булевы `true` / `false`
- `null`
- массивы `[1, 2, 3]` / `['foo', false, null, 1, 2, '999']`

Примеры:

```DSL
# возраст пользователя больше 18
user.age greater than 18

# массив ролей содержит роль 'admin'
user.roles contains 'admin'

# тэг заказа либо 'VIP, либо 'priority'
order.tag in ['vip', 'priority']

# токен пользователя не null
user.token is not null

# логин пользователя длиннее 12 символов
user.login length greater than 12
```



---

## Неявная группа (implicit group)

Если правила идут без `all of:` или `any of:`, они объединяются оператором политики:

```dsl
permit permission.order.update if all:
  user.roles contains 'admin'
  user.token is not null
```

Эквивалентно:

```dsl
permit permission.order.update if all:
  all of:
    user.roles contains 'admin'
    user.token is not null
```

---

## Полный пример

```dsl
# @name разрешено обновление заказа
permit permission.order.update if any:

  # @name если это администратор
  all of:
    user.roles contains 'admin'
    user.token is not null

  # @name если это разработчик
  any of:
    user.roles contains 'developer'
    user.logit is equals 'dev'
```
