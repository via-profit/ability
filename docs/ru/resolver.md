# @via-profit/ability - Резолвер

[![npm version](https://img.shields.io/npm/v/@via-profit/ability)](https://www.npmjs.com/package/@via-profit/ability)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Резолвер - это основной компонент, который запускает проверку политик. Представлен классом `AbilityResolver`

> 📚 **Связанные документы:**
> - [Стратегии](./strategies.md)
> - [DSL](./dsl.md)
> - [Генерация типов](./types-generator.md)

---

## Содержание

- [Концепция](#концепция)
- [Генерация типов](#генерация-типов)
- [Создание резолвера](#создание-резолвера)
- [Использование](#использование)
- [Параметры конструктора](#параметры-конструктора-abilityresolver)
- [Опции](#опции-abilityresolveroptions)
    - [Теги](#теги)
- [Частые вопросы](#частые-вопросы)

AbilityResolver работает в двух режимах (два метода проверки):

**Enforce** режим - основной режим.

Старайтесь использовать именно его. В этом режиме резолвер запускает проверку доступа и в случае получения отказа
провоцирует исключение в виде ошибки `AbilityError`.
Почему предпочтительнее - потому что если доступ должен быть запрещен, то в 100% случаев будет вызвана ошибка доступа. В
ручном же режиме (resolve режим) вам нужно не забыть обработать ситуацию, при которой доступ может быть запрещён.

**Resolve** режим - режим для ручной проверки доступа. Проверка доступа всегда возвращает инстанс класса
`AbilityResult`, который содержит информацию о пройденной проверке. Что делать дальше - решать вам.


> [!TIP]
> В большинстве случаев используйте `enforce` - он гарантирует, что вы не пропустите проверку доступа, а ошибка будет
> обработана на верхнем уровне вашего приложения.


---

## Концепция

Вы можете создать один или несколько резолверов, но не стоит создавать его каждый раз перед проверкой прав доступа.
Лучше определить резолвер один раз и экспортировать уже его инстанс.

Резолвер должен получить на вход массив политик (`AbilityPolicy[]`) и ссылку на класс стратегии. В пакете уже есть 9
стратегий, которые можно использовать. Если этого мало, то создавайте свои - [подробнее про стратегии](./strategies.md).

Каждая политика - это класс `AbilityPolicy`, который содержит в себе группы правил и сами правила. Так как описывать
политики классами неудобно (но вполне реализуемо), в пакете реализована поддержка простого [DSL](./dsl.md), который
позволяет описывать правила, группы и политики простым текстом.

Чтобы еще работал и TypeScript, нужно сгенерировать типы из описанных политик. Для этого в пакете есть специальный
модуль `AbilityTypeGenerator` [Подробнее про генерацию типов](./types-generator.md)

Чтобы вся эта петрушка собралась воедино, можно реализовать следующий подход и структуру:

1. Все политики описываются, формируются и генерируются с типами только на сервере. На клиентскую часть приложения
   пробрасываются уже готовые политики в виде JSON, которые парсятся при помощи `AbilityJSONParser` уже на стороне
   клиента. Так же, на клиентскую сторону нужно пробросить и сгенерированные типы (TypeScript). Такой подход
   гарантирует, что у вас не будет рассинхрона данных между клиентом и сервером.
2. Политики описываются в DSL. Проще всего разбить политики на логические части и хранить в отдельных файлах, например
   политики доступа заявок описывать в файле `./ability/orders.dsl`, политики пользователей - в `./ability/users.dsl` и
   так далее. После, все эти части DSL собираются воедино и подаются на парсинг (`AbilityDSLParser`). Как результат -
   имеем массив политик. Параллельно этому генерируются типы и складываются, например, в файл
   `./ability/ability.types.ts`.

Для примера опишем простую политику заявок (файл `./ability/orders.dsl`)

`text
@name "Читать данные по заявке можно только автору и только до 16 часов дня"
permit permission.orders.read if any:
  order.author is equals user.id
  env.hour less than 16
`


---

## Генерация типов

Начнем с наиболее сложного и важного - генерация типов. Для этого создаем скрипт `./scripts/ability.js` в корне вашего
проекта и запихиваем туда код (commonjs):

<details>
  <summary><b>📄 Код ./scripts/ability.js</b> (нажмите, чтобы развернуть)</summary>

```js
const fs = require('node:fs');
const path = require('node:path');
const { AbilityTypeGenerator, AbilityDSLParser } = require('@via-profit/ability');

// для удобства кладем все в функцию
const parse = () => {
  // директория с вашими dsl файлами
  const dslPath = path.resolve('./src/ability');

  // файл куда будут писаться готовые типы
  const typeDefsFile = path.resolve('./src/ability/ability.types.ts');

  if (fs.existsSync(dslPath)) {
    const dslFiles = fs.readdirSync(dslPath);

    // Здесь сортировка файлов. Файл с именем aliases.dsl будет первым.
    // Для чего - просто если алиасы буду описаны после правил, то парсер 
    // сломается, ведь он ничего не знает о том, какие алиасы вы придумали 
    dslFiles.sort((a, b) => {
      if (a === 'aliases.dsl') {
        return -1;
      }
      if (b === 'aliases.dsl') {
        return 1;
      }
      return a.localeCompare(b);
    });

    let dsl = '';

    // ищем все файлы с расширением dsl, 
    // читаем их и объединяем в один большой DSL
    for (const filename of dslFiles) {
      if (filename.endsWith('.dsl')) {
        dsl += '\n' + fs.readFileSync(path.join(dslPath, filename), { encoding: 'utf-8' });
      }
    }

    try {
      // Парсим весь DSL в политики
      const policies = new AbilityDSLParser(dsl).parse();

      // Заталкиваем политики в генератор типов
      const typeDefs = new AbilityTypeGenerator(policies).generateTypeDefs();

      fs.writeFileSync(typeDefsFile, typeDefs, { encoding: 'utf-8' });
    } catch (err) {
      console.error(err);
      process.exit(1);
    } finally {
      console.log(
        `▶ Typedefs was generated successfully in ${path.relative(process.cwd(), typeDefsFile)}`,
      );
    }
  }

};

// запускаем парсинг
parse();

```

</details>


Чтобы запускать этот код из npm, добавляем в `package.json`:

```json
{
  "scripts": {
    "ability": "node ./scripts/ability.js"
  }
}
```

Теперь, чтобы сгенерировать TypeScript типы, достаточно запустить `npm run ability`.

Можно пойти дальше и, если у вас проект собирается на webpack, можно добавить плагин, который будет слушать изменения
dsl файлов и генерировать типы.

> Код webpack плагина содержит еще и http-сервер, работающий на `http://localhost:8005`, который отдает сгенерированные
> типы и политики в JSON по GET запросу на `http://localhost:8005/ability/artifacts`. Это нужно для того, чтобы
> скачивать
> на клиентскую часть сгенерированные типы и JSON всех политик не вручную, а например, по скрипту. Такой сервер работает
> только в Dev режиме. В проде этот плагин выключен.

<details>
  <summary><b>📄 Код webpack плагина</b> (нажмите, чтобы развернуть)</summary>

```js
/* eslint-disable */
// @ts-nocheck


const { exec } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');
const http = require('node:http');

const {
  AbilityDSLParser,
  AbilityTypeGenerator,
  AbilityJSONParser,
} = require('@via-profit/ability');


class AbilityWatchPlugin {
  constructor(options = {}) {
    this.port = options.port || 8005;
    this.server = null;
    this.abilityDir = null;
  }


  apply(compiler) {
    this.abilityDir = path.resolve(compiler.context, 'src/ability');

    compiler.hooks.afterCompile.tap('AbilityWatchPlugin', compilation => {
      compilation.contextDependencies.add(this.abilityDir);
    });

    if (!this.server) {
      this.startAbilityServer();
    }

    if (fs.existsSync(this.abilityDir)) {
      let timeoutId = null;
      fs.watch(this.abilityDir, { recursive: true }, (event, filename) => {
        if (!filename || !filename.endsWith('.dsl')) return;

        if (timeoutId) {
          clearTimeout(timeoutId);
        }

        timeoutId = setTimeout(() => {
          console.log(`▶ DSL changed: ${filename}`);
          console.log('▶ Running scripts/ability.js');
          exec('node scripts/ability.js', (err, stdout, stderr) => {
            if (stdout) {
              console.log(stdout);
            }
            if (stderr) {
              console.error(stderr);
            }
            if (err) {
              console.error(err);
            }
          });
        }, 50);
      });
    }
  }

  startAbilityServer() {
    if (this.server) {
      return;
    }

    this.server = http.createServer((req, res) => {
      res.setHeader('Access-Control-Allow-Origin', '*');
      res.setHeader('Content-Type', 'application/json');

      if (req.url === '/ability/artifacts') {


        try {
          const artifacts = this.compileArtifacts();

          res.writeHead(200);
          return res.end(JSON.stringify({ ability: artifacts }));
        } catch (err) {
          console.error(err);
          res.writeHead(500);
          return res.end();
        }


      } else {
        res.writeHead(404);
        res.end(JSON.stringify({ error: 'Not found' }));
      }
    });

    this.server.listen(this.port, () => {
      console.log(`🌐 Ability watch server started on http://localhost:${this.port}`);
    });

    this.server.on('error', err => {
      if (err.code === 'EADDRINUSE') {
        console.error(`❌ Port ${this.port} is already in use. Unable to start ability server.`);
      } else {
        console.error('❌ Ability server error:', err);
      }
    });
  }


  compileArtifacts() {
    if (!fs.existsSync(this.abilityDir)) {
      console.error(`dir ${this.abilityDir} not found`);
      return;
    }

    const files = fs.readdirSync(this.abilityDir);

    files.sort((a, b) => {
      if (a === 'aliases.dsl') {
        return -1;
      }
      if (b === 'aliases.dsl') {
        return 1;
      }
      return 0;
    });

    let dsl = '';

    for (const filename of files) {
      if (filename.endsWith('.dsl')) {
        dsl += '\n' + fs.readFileSync(path.join(this.abilityDir, filename), { encoding: 'utf-8' });
      }
    }

    const policies = new AbilityDSLParser(dsl).parse();
    const typeDefs = new AbilityTypeGenerator(policies).generateTypeDefs();

    return {
      policies: AbilityJSONParser.toJSON(policies),
      typeDefs,
    };
  }
}

module.exports = AbilityWatchPlugin;


```

</details>


---

## Создание резолвера

Теперь непосредственно создание самого резолвера.

На данном этапе у нас есть сгенерированные типы (`./ability/ability-types.ts`), dsl файлы c политиками (
`./ability/orders.dsl`, `./ability/users.dsl` и тп.).

Реализация файла `./ability/index.ts`, который будет содержать в себе сам резолвер.

> [!IMPORTANT]
> Резолвер создается и инициализируется один раз. Экспортируется уже его инстанс.

```ts
import {
  AbilityDSLParser,
  AbilityError,
  AbilityResolver,
  DenyOverridesStrategy,
} from '@via-profit/ability';

// Ваши DSL файлы
import dslAliases from './aliases.dsl';
import dslOrders from './orders.dsl';
import dslUsers from './users.dsl';

// Сгенерированные типы
import type { Environment, PolicyTags, Resources } from './ability.types';

export * from './ability.types';

// Формируем общий DSL в вид одной большой строки
const fullDSL = [
  // Алиасы, если у вам они есть - первыми
  dslAliases,

  // Остальные ваши DSL файлы
  dslOrders,
  dslUsers,
].join('\n');

// Создаем политики через DSL парсер
// В качестве дженериков передаем сгенерированные типы в парсер
const policies = new AbilityDSLParser<Resources, Environment, PolicyTags>(fullDSL).parse();

// Создаем инстанс резолвера
// Вторым аргументом передаем ссылку на класс стратегии
// Третий аргумент - опции, среди которых есть onDeny.
// onDeny - это коллбэк, который будет вызван всякий раз
// когда резолвер вернет deny.
export const abilityResolver = new AbilityResolver(policies, DenyOverridesStrategy, {
  onDeny: res => {

    // Получаем политику, которая вернула deny
    // для того чтобы узнать её имя и 
    // отобразить в тексте ошибки (опционально)
    const decisive = res.decisive();

    // Если это dev, то вываливаем в консоль explain всех политик,
    // которые отработали
    if (process.env.NODE_ENV === 'development') {
      console.log(res.explain());
    }

    // Если не вернуть ошибку, то резолвер сам её вернет,
    // но в тексте будет просто Permission denied, а нам
    // нужно поместить туда название политики, которая
    // первая вернула deny
    throw new AbilityError(`Permission denied.${decisive ? ' ' + decisive.name : ''}`);
  },
});


```

---

## Использование

Теперь, имея резолвер и типы, можно запустить проверку прав доступа в том месте, где это требуется.

Метод проверки `enforce` принимает первым аргументом ключ разрешения без префикса `permission.`. Вторым аргументом нужно
передать проверяемый субъект (в данном случае - заявка). Так как ключ разрешения `orders.read` описан только в одной
политике (см. пример выше) и в этой политике есть проверки статуса и поля author, то передавать нужно объект содержащий
эти поля. Данные всей заявки передавать не обязательно.

Например:

```ts
import { abilityResolver, Environment, Resource } from './ability';
import Order from './Order';

const Mutation = new GraphQLObjectType<unknown, unknown>({
  name: 'Query',
  fields: {
    order: {
      type: Order,
      args: {
        id: { type: new GraphQLNonNull(GraphQLID) },
      },
      resolve: async (_, args, ctx) => {
        const { id } = args;
        const { user } = ctx;
        const order = await db.getOrder(id);

        // запускаем проверку прав для ключа orders.read (первый аргумент)
        // вторым передаем объект с проверяемым ресурсом (заявка)
        // третьим передаем данные окружения (все что не статично:
        // время, ip адрес, часовой пояс и пр.)
        abilityResolver.enforce('orders.read', { order }, {
          hour: new Date().getHours()
        });

        // Если проверка будет завалена, то выполнение кода
        // до этой строки даже не дойдет. Если нужно иное поведение,
        // то используйте метод resolve, а не enforce
        return order;
      },
    },
  }
});
```

Пример для `resolve` режима:

```ts
const order = await db.getOrder(id);
// Ручная проверка (resolve режим)
const result = abilityResolver.resolve('orders.read', { order }, {
  hour: new Date().getHours()
});

if (result.isDeny()) {
  // Сами решаете, что делать
  console.log('Доступ запрещен:', result.decisive()?.name);
  return null;
}

// Если allow - продолжаем
return order;
```

---

## Параметры конструктора `AbilityResolver`

| Параметр   | Тип                                 | Обязательный | Описание                                                 |
|------------|-------------------------------------|--------------|----------------------------------------------------------|
| `policies` | `readonly P[] \| P`                 | Да           | Список политик или одна политика                         |
| `strategy` | `new (policies: readonly P[]) => S` | Да           | Стратегия разрешения (например, `DenyOverridesStrategy`) |
| `options`  | `AbilityResolverOptions<TTags>`     | -            | Дополнительные опции (см. ниже)                          |

---

## Методы `AbilityResolver`

| Метод         | Параметры                                                                                                                                                                                                                                                                          | Возвращает                                  | Описание                                                                                                                                               |
|---------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`resolve`** | <ul><li>`permission: Permission` - ключ разрешения (например, `"orders.read"`)</li><li>`resource: Resource` - проверяемый ресурс (субъект)</li><li>`environment?: Environment` - данные окружения (опционально)</li></ul>                                                          | `AbilityResult<Resource, Environment>`      | **Ручная проверка доступа**. Возвращает объект с результатом проверки. Используйте, когда нужно самостоятельно обработать ситуацию с запретом доступа. |
| **`enforce`** | <ul><li>`permission: Permission` - ключ разрешения (например, `"orders.read"`)</li><li>`resource: Resource` - проверяемый ресурс (субъект)</li><li>`environment?: Environment` - данные окружения (опционально)</li><li>`options?: EnforceOptions` - опции (опционально)</li></ul> | `void` или `never` (выбрасывает исключение) | **Автоматическая проверка доступа**. При запрете выбрасывает `AbilityError`. Рекомендуемый способ для API-эндпоинтов.                                  |

---

### Описание параметров

| Параметр      | Тип                                                  | Описание                                                                                                  |
|---------------|------------------------------------------------------|-----------------------------------------------------------------------------------------------------------|
| `permission`  | `Permission`                                         | Ключ разрешения без префикса `permission.`. Например: `"orders.read"`, `"users.delete"`                   |
| `resource`    | `Resource`                                           | Проверяемый ресурс (субъект). Должен содержать поля, указанные в политике. Может быть частичным объектом. |
| `environment` | `Environment` (опционально)                          | Данные окружения: время, IP-адрес, часовой пояс и другие динамические данные.                             |
| `options`     | `EnforceOptions` (опционально, только для `enforce`) | Дополнительные опции. См. [EnforceOptions](#enforceoptions)                                               |

---

### EnforceOptions

| Опция     | Тип                               | Описание                                                                                                  |
|-----------|-----------------------------------|-----------------------------------------------------------------------------------------------------------|
| `onDeny`  | `(result: AbilityResult) => void` | Коллбэк, вызываемый перед выбрасыванием исключения. Получает инстанс `AbilityResult` с деталями проверки. |
| `onAllow` | `(result: AbilityResult) => void` | Коллбэк, вызываемый при успешной проверке доступа. Получает инстанс `AbilityResult` с деталями проверки.  |

---

### Примеры использования

#### `resolve` - ручная проверка

```ts
// Ручная проверка доступа
const result = abilityResolver.resolve('orders.read', { order }, {
  hour: new Date().getHours()
});

if (result.isDenied()) {
  // Самостоятельно обрабатываем запрет
  console.log('Доступ запрещен:', result.decisive()?.name);
  return null;
}

// Продолжаем выполнение
return order;
```

#### `enforce` - автоматическая проверка

```ts
// Автоматическая проверка с выбрасыванием исключения
abilityResolver.enforce('orders.read', { order }, {
  hour: new Date().getHours()
});

// Если код дошел сюда - доступ разрешен
return order;
```

#### `enforce` с кастомным обработчиком

```ts
// Автоматическая проверка с кастомной логикой при запрете
abilityResolver.enforce('orders.read', { order }, {
  hour: new Date().getHours()
}, {
  onDeny: (result) => {
    // Логируем причину отказа
    console.error('Access denied:', result.explain());

    // Можно отправить метрики или уведомления
    metrics.increment('permission_denied', {
      permission: 'orders.read',
      policy: result.decisive()?.name
    });
  }
});
```
---

### Типизация методов

```ts
class AbilityResolver<P extends AbilityPolicy = AbilityPolicy> {
  // Ручная проверка
  public resolve<
    Permission extends keyof ExtractResources<P> & string
  >(
    permission: Permission,
    resource: ExtractResourceByPermission<P, Permission>,
    environment?: ExtractEnvironmentByPermission<P, Permission>,
  ): AbilityResult<
    ExtractResourceByPermission<P, Permission>,
    ExtractEnvironment<P>
  >;

  // Автоматическая проверка
  public enforce<
    Permission extends keyof ExtractResources<P> & string
  >(
    permission: Permission,
    resource: ExtractResourceByPermission<P, Permission>,
    environment?: ExtractEnvironmentByPermission<P, Permission>,
    options?: EnforceOptions,
  ): void | never;
}
```

---

## Опции `AbilityResolverOptions`

| Опция     | Тип                               | Описание                                                                                                          |
|-----------|-----------------------------------|-------------------------------------------------------------------------------------------------------------------|
| `tags`    | `readonly TTags[]`                | Фильтр политик по тегам. Учитываются только политики с указанными тегами. Политики без тегов используются всегда. |
| `onDeny`  | `(result: AbilityResult) => void` | Коллбэк, вызываемый при каждом `deny`. Получает инстанс `AbilityResult`.                                          |
| `onAllow` | `(result: AbilityResult) => void` | Коллбэк, вызываемый при каждом `permit`. Получает инстанс `AbilityResult`.                                        |

### Теги

Каждая политика может иметь список тегов. Это механизм для:

- **Разграничения резолверов** - создавайте резолверы для разных контекстов (админка / пользовательская часть)
- **Фильтрации политик** - выполняйте проверки только с определенными тегами
- **Упрощения тестирования** - изолируйте группы политик для юнит-тестов

#### Пример использования тегов

Опишем политики с тегами в DSL:

```text
@name "Управление пользователями"
@tags admin, user-management
permit permission.users.* if any:
  user.role is equals "admin"

@name "Просмотр заявок"
@tags user, orders
permit permission.orders.read if any:
  order.author is equals user.id
```

Теперь можно создать несколько резолверов для разных контекстов:

```ts
// Резолвер для администраторов (учитывает только админские политики)
export const adminResolver = new AbilityResolver(policies, DenyOverridesStrategy, {
  tags: ['admin'],
});

// Резолвер для обычных пользователей
export const userResolver = new AbilityResolver(policies, DenyOverridesStrategy, {
  tags: ['user'],
});

// Универсальный резолвер (использует все политики)
export const fullResolver = new AbilityResolver(policies, DenyOverridesStrategy);
```

Или использовать теги для выборочной проверки в рантайме:

```ts
// Проверка только с тегами ['admin'] без создания отдельного резолвера
const result = abilityResolver.resolve('users.delete', { user }, {
  hour: new Date().getHours()
}, {
  tags: ['admin'] // Переопределяем теги для конкретной проверки
});
```

> [!NOTE]
> Теги указываются в конструкторе резолвера и могут быть переопределены при вызове методов enforce или resolve через
> параметр options.

> [!IMPORTANT]
> Если политика не имеет тегов, она будет использоваться ВСЕГДА, независимо от фильтрации по тегам.


---

## Частые вопросы

### Можно ли использовать несколько резолверов?

Да, но обычно достаточно одного.

### Как отлаживать политики?

Используйте метод `explain()` на результате проверки - он покажет все сработавшие правила.

### Можно ли использовать без генерации типов?

Да, но тогда линтер не сможет подсказать вам о наличии ошибки или опечатки

### Как добавить свои стратегии?

Наследуйтесь от класса `AbilityStrategy` и реализуйте метод `resolve()`. Подробнее
в [документации по стратегиям](./strategies.md).

### Почему в примере используется `DenyOverridesStrategy`?

Это стандартная стратегия, которая работает по принципу "один deny - всё deny". Если хотя бы одна политика запрещает
доступ, то результат будет `deny`.

### Как использовать теги для разных ролей?

Создайте отдельные резолверы с разными тегами или передавайте теги в параметрах методов `enforce`/`resolve`. Например,
для API-эндпоинтов админа используйте резолвер с тегом `admin`, для пользовательских - с тегом `user`.
