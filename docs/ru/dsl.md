# Domain-Specific Language

Ability DSL — это декларативный язык для описания политик доступа.  
Он позволяет определять правила в человекочитаемой форме, используя простые конструкции: *политики*, *группы*, *правила* и *аннотации*.



##  Структура политики

Политика состоит из:

```
<effect> <action> if <all|any>:
  <group>...
```

Где:

- **effect** — `permit` или `deny`
- **action** — строка вида `resource.operation`
- **if all:** — все группы должны быть истинны
- **if any:** — хотя бы одна группа должна быть истинна

Пример:

```dsl
permit order.update if any:
  all of:
    user.roles contains 'admin'
    user.token is not null

  any of:
    user.roles contains 'developer'
    user.logit equals 'dev'
```

Пример политики выше гласит - действие order.update будет разрешено при выполнении одного из двух условий:
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

Правило - это ключевая единица политик. Именно правила определяют действие в политике. С помощью правил задаются условия по которым определяется эффективность политики (‘permit’ или ‘deny’)

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

| Оператор | Пример | Значение |
|---------|--------|----------|
| `equals` / `is` | `a equals b` | `a == b` |
| `contains` | `roles contains 'admin'` | элемент в массиве |
| `in` | `role in ['admin','dev']` | член множества |
| `greater` / `greater than` | `age greater 18` | `>` |
| `greater or equal` | `age greater equal 18` | `>=` |
| `less` / `less than` | `age less 10` | `<` |
| `less or equal` | `age less equal 10` | `<=` |
| `is null` | `token is null` | `== null` |
| `is not null` | `token is not null` | `!= null` |

### Value (значение)

Поддерживаются:

- строки `'text'`
- числа `42`
- булевы `true` / `false`
- `null`
- массивы `[1, 2, 3]`

Примеры:

```DSL
# возраст пользователя больше 18
user.age greater 18

# массив ролей содержит роль 'admin'
user.roles contains 'admin'

# тэги заказа либо 'VIP, либо 'priority'
order.tags in ['vip', 'priority']

# токен пользователя не null
user.token is not null
```



---

## Неявная группа (implicit group)

Если правила идут без `all of:` или `any of:`, они объединяются оператором политики:

```dsl
permit order.update if all:
  user.roles contains 'admin'
  user.token is not null
```

Эквивалентно:

```dsl
permit order.update if all:
  all of:
    user.roles contains 'admin'
    user.token is not null
```

---

## Полный пример

```dsl
# @name разрешено обновление заказа
permit order.update if any:

  # @name если это администратор
  all of:
    user.roles contains 'admin'
    user.token is not null

  # @name если это разработчик
  any of:
    user.roles contains 'developer'
    user.logit equals 'dev'
```
