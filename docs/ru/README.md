# @via-profit/Ability

> Набор сервисов, частично реализующих
> принцип [Attribute Based Access Control](https://en.wikipedia.org/wiki/Attribute-based_access_control)
> Пакет позволяет описывать правила, объединять их в группы, формировать политики и применять их к данным для определения разрешений.

## Для чего

Пакет задуман как **лёгкая и предельно простая альтернатива** тяжёлым системам управления доступом.  
Без сложных конфигураций, без зависимостей — только минимальный набор инструментов, который позволяет описывать правила и политики в максимально простом DSL.

## Содержание

 - [Быстрый старт](#быстрый-старт)
 - [Domain-Specific Language](./dsl.md)
 - [Api-Reference](./api.md)


## Быстрый старт

Установить пакет, написать DSL, вызвать парсер, запустить резолвер.

### Установка

```bash
npm install @via-profit/ability
```

```bash
yarn add @via-profit/ability
```

```bash
pnpm add @via-profit/ability
```


### Пример: запретить доступ к `passwordHash` всем, кроме владельца

Допустим, у нас есть пользовательские данные:

```ts
const user = {
  id: '1',
  login: 'user-001',
  passwordHash: '...',
};
```

Нужно запретить чтение `passwordHash` всем, кроме самого пользователя.

#### DSL‑политика

На языке политик это выглядит так:

```
deny user.passwordHash if any:
  viewer.id is not equals owner.id
```

**Пояснение:**

- `deny` — эффект политики (запретить доступ)  
- `user.passwordHash` — действие, которое проверяем  
- `if any:` — начало блока условий
- `viewer.id is not equals owner.id` — правило: если идентификатор запрашивающего не равен идентификатору владельца

Если `viewer.id` не равен `owner.id`, правило считается выполненным, и политика возвращает `deny` — доступ запрещён. Если же идентификаторы совпадают (т.е. пользователь запрашивает свои собственные данные), правило не срабатывает, и доступ разрешается.

#### Проверка в коде

```ts
import { AbilityDSLParser, AbilityResolver } from '@via-profit/ability';

const dsl = `
deny user.passwordHash if any:
  viewer.id is not equals owner.id
`;

const policies = new AbilityDSLParser(dsl).parse(); // получение политик
const resolver = new AbilityResolver(policies); // создание резолвера

resolver.enforce('user.passwordHash', {
  viewer: { id: '1' },
  owner: { id: '2' },
}); // выбросит ошибку — доступ запрещён
```

---

## Полная документация

 - [Domain-Specific Language](./dsl.md)
 - [Api-Reference](./api.md)


## Лицензия

Этот проект лицензирован под лицензией MIT. Подробности в файле [LICENSE](LICENSE).
