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

---

## Аннотации

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

Группы могут иметь аннотации:

```dsl
# @name developer group
any of:
  user.roles contains 'developer'
```

---

## Правила

Правило имеет форму:

```
<subject> <operator> <value?>
```

### Subject

Идентификатор в dot‑нотации:

```
user.roles
env.time.hour
order.total
```

### Operators

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

### Значения

Поддерживаются:

- строки `'text'`
- числа `42`
- булевы `true` / `false`
- `null`
- массивы `[1, 2, 3]`

Примеры:

```dsl
user.age greater 18
user.roles contains 'admin'
order.tags in ['vip', 'priority']
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
# @name can order update
# @description User may update order if admin or developer
permit order.update if any:

  # @name authorized admin
  all of:
    # @name contains role admin
    user.roles contains 'admin'
    user.token is not null

  # @name if is developer
  any of:
    user.roles contains 'developer'
    user.logit equals 'dev'
```
