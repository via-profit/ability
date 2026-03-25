# Политики

Политика — это основной блок системы. Она определяет, какое действие над каким ресурсом и при каких условиях разрешено или запрещено.

## Простая политика

Рассмотрим пример политики, которая разрешает удаление пользователя только администраторам:

```
permit user.delete if all:
  user.role equals 'admin'
```

**Разбор конструкции:**
- `permit` — эффект политики (разрешить доступ)
- `user.delete` — действие, к которому применяется правило
- `if all:` — начало блока условий (все правила внутри должны быть истинны)
- `user.role equals 'admin'` — правило: роль пользователя должна быть `admin`

### Использование в коде

```ts
import { AbilityDSLParser, AbilityResolver } from '@via-profit/ability';

const dsl = `
permit user.delete if all:
  user.role equals 'admin'
`;

const policies = new AbilityDSLParser(dsl).parse(); // получаем политики
const resolver = new AbilityResolver(policies); // создаем резолвер

// Проверка доступа
try {
  await resolver.enforce('user.delete', {
    user: { role: 'admin' },
  });
  console.log('Доступ разрешён');
} catch (error) {
  console.log('Доступ запрещён');
}
```

### Комбинация условий

Политики могут содержать несколько условий, объединённых логическими операторами:

```
permit user.delete if any:
  user.role equals 'admin'
  user.role equals 'superadmin'
```

Эта политика разрешает удаление пользователя, если его роль — `admin` или `superadmin`.

### Вложенные группы

Для более сложной логики можно использовать вложенные группы:

```
permit user.delete if any:
  all of:
    user.role contains 'admin'
    user.department equals 'security'
  any of:
    user.role contains 'superadmin'
    user.id equals request.userId
```

Здесь политика разрешает удаление пользователя, если:
- пользователь — администратор **и** работает в отделе безопасности, **или**
- пользователь — суперадминистратор, **или**
- пользователь — владелец запроса

## `Эффект` политики

TODO: Дописать про эффект политики

## `Действие` (action) политики

TODO: Дописать про дейстаие политики (action)


## `Группы правил`

TODO: Дописать про группы правил политики


## `Правила`

TODO: Дописать про правила политики

## `Резолвер`

TODO: Дописать про резолвер

