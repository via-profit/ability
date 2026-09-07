# @via-profit/ability - Resolver

[![npm version](https://img.shields.io/npm/v/@via-profit/ability)](https://www.npmjs.com/package/@via-profit/ability)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The resolver is the main component that runs policy checks. It is represented by the `AbilityResolver` class.

> 📚 **Related documents:**
> - [Strategies](./strategies.md)
> - [DSL](./dsl.md)
> - [Type generation](./types-generator.md)

---

## Table of contents

- [Concept](#concept)
- [Type generation](#type-generation)
- [Creating a resolver](#creating-a-resolver)
- [Usage](#usage)
- [AbilityResolver constructor parameters](#abilityresolver-constructor-parameters)
- [Options](#options-abilityresolveroptions)
    - [Tags](#tags)
- [Frequently asked questions](#frequently-asked-questions)

`AbilityResolver` has two modes (two checking methods):

**Enforce mode** is the primary mode.

Prefer using it whenever possible. In this mode, the resolver checks access and throws an `AbilityError` when access is denied. This is preferable because a forbidden operation will always raise an access error. In manual (`resolve`) mode, you must remember to handle the case where access is denied.

**Resolve mode** is for manual access checks. It always returns an instance of `AbilityResult`, which contains information about the completed check. What to do next is up to you.

> [!TIP]
> In most cases, use `enforce`: it ensures that you do not accidentally skip access handling and that the error is handled at the top level of your application.

---

## Concept

You can create one or more resolvers, but do not create one before every permission check. It is better to define a resolver once and export its instance.

The resolver receives an array of policies (`AbilityPolicy[]`) and a reference to a strategy class. The package already provides 9 strategies. If they are not enough, create your own — see [more about strategies](./strategies.md).

Each policy is an `AbilityPolicy` class containing rule groups and rules. Describing policies as classes is inconvenient (though possible), so the package supports a simple [DSL](./dsl.md) for describing rules, groups, and policies as text.

To make TypeScript work as well, generate types from the policies. The package provides the `AbilityTypeGenerator` module for this purpose. See [more about type generation](./types-generator.md).

A typical approach and structure can bring all of this together:

1. All policies are described, built, and typed only on the server. The client receives ready-made policies as JSON, parses them with `AbilityJSONParser`, and also receives the generated TypeScript types. This guarantees that client and server data stay synchronized.
2. Policies are described in the DSL. The simplest approach is to split them into logical parts and store them in separate files. For example, order access policies can be stored in `./ability/orders.dsl` and user policies in `./ability/users.dsl`. Then collect all DSL parts and pass them to `AbilityDSLParser`. The result is an array of policies. Types can be generated in parallel and saved, for example, to `./ability/ability.types.ts`.

For example, define a simple order policy (`./ability/orders.dsl`):

```text
@name "Only the author can read order data before 4 PM"
permit permission.orders.read if any:
  order.author is equals user.id
  env.hour less than 16
```

---

## Type generation

Start with the most complex and important part: type generation. Create `./scripts/ability.js` in the root of your project and put this CommonJS code in it:

<details>
  <summary><b>📄 Code for ./scripts/ability.js</b> (click to expand)</summary>

```js
const fs = require('node:fs');
const path = require('node:path');
const { AbilityTypeGenerator, AbilityDSLParser } = require('@via-profit/ability');

// Put everything in a function for convenience
const parse = () => {
  // Directory containing your DSL files
  const dslPath = path.resolve('./src/ability');

  // File where the generated types will be written
  const typeDefsFile = path.resolve('./src/ability/ability.types.ts');

  if (fs.existsSync(dslPath)) {
    const dslFiles = fs.readdirSync(dslPath);

    // Sort files so aliases.dsl comes first.
    // Otherwise, aliases declared after rules would be unknown to the parser.
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

    // Find all DSL files, read them, and combine them into one DSL string
    for (const filename of dslFiles) {
      if (filename.endsWith('.dsl')) {
        dsl += '\n' + fs.readFileSync(path.join(dslPath, filename), { encoding: 'utf-8' });
      }
    }

    try {
      // Parse all DSL into policies
      const policies = new AbilityDSLParser(dsl).parse();

      // Pass the policies to the type generator
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

// Start parsing
parse();
```

</details>

To run this code from npm, add the following to `package.json`:

```json
{
  "scripts": {
    "ability": "node ./scripts/ability.js"
  }
}
```

Now run `npm run ability` to generate the TypeScript types.

If your project is built with webpack, you can go further and add a plugin that watches DSL files and generates types.

> The webpack plugin also contains an HTTP server at `http://localhost:8005`. It serves generated types and all policies as JSON through GET `http://localhost:8005/ability/artifacts`. This lets the client download generated types and policy JSON through a script rather than manually. The server works only in development mode; the plugin is disabled in production.

<details>
  <summary><b>📄 Webpack plugin code</b> (click to expand)</summary>

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

## Creating a resolver

At this point, you have generated types (`./ability/ability-types.ts`) and DSL policy files (`./ability/orders.dsl`, `./ability/users.dsl`, and so on).

Implement `./ability/index.ts` to contain the resolver itself.

> [!IMPORTANT]
> Create and initialize the resolver only once. Export its instance.

```ts
import {
  AbilityDSLParser,
  AbilityError,
  AbilityResolver,
  DenyOverridesStrategy,
} from '@via-profit/ability';

// Your DSL files
import dslAliases from './aliases.dsl';
import dslOrders from './orders.dsl';
import dslUsers from './users.dsl';

// Generated types
import type { Environment, PolicyTags, Resources } from './ability.types';

export * from './ability.types';

// Build one complete DSL string
const fullDSL = [
  // Put aliases first, if you have any
  dslAliases,

  // The rest of your DSL files
  dslOrders,
  dslUsers,
].join('\n');

// Create policies with the DSL parser
// Pass generated types to the parser as generics
const policies = new AbilityDSLParser<Resources, Environment, PolicyTags>(fullDSL).parse();

// Create the resolver instance
// Pass a strategy class as the second argument
// The third argument contains options, including onDeny.
// onDeny is called whenever the resolver returns deny.
export const abilityResolver = new AbilityResolver(policies, DenyOverridesStrategy, {
  onDeny: res => {
    // Get the policy that returned deny so its name
    // can optionally be included in the error message
    const decisive = res.decisive();

    // In development, print an explanation of all policies
    // that were evaluated
    if (process.env.NODE_ENV === 'development') {
      console.log(res.explainToString());
    }

    // Without throwing, the resolver would return its own error
    // with only "Permission denied". Include the name of the
    // first policy that returned deny instead.
    throw new AbilityError(`Permission denied.${decisive ? ' ' + decisive.name : ''}`);
  },
});
```

---

## Usage

With the resolver and types in place, you can check permissions wherever needed.

The `enforce` method takes a permission key without the `permission.` prefix as its first argument. Its second argument is the subject being checked (an order in this example). Because `orders.read` is described in one policy above and that policy checks the status and author fields, the object only needs to contain those fields; you do not need to pass the entire order.

For example:

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

        // Check permissions for orders.read (first argument)
        // Pass the resource (order) as the second argument
        // Pass environment data as the third argument (anything dynamic:
        // time, IP address, time zone, and so on)
        abilityResolver.enforce('orders.read', { order }, {
          hour: new Date().getHours()
        });

        // If the check fails, execution never reaches this line.
        // Use resolve instead of enforce for different behavior.
        return order;
      },
    },
  }
});
```

Example for `resolve` mode:

```ts
const order = await db.getOrder(id);
// Manual check (resolve mode)
const result = abilityResolver.resolve('orders.read', { order }, {
  hour: new Date().getHours()
});

if (result.isDeny()) {
  // Decide what to do yourself
  console.log('Access denied:', result.decisive()?.name);
  return null;
}

// Continue when allowed
return order;
```

---

## `AbilityResolver` constructor parameters

| Parameter | Type | Required | Description |
|---|---|---|---|
| `policies` | `readonly P[] \| P` | Yes | A list of policies or one policy |
| `strategy` | `new (policies: readonly P[]) => S` | Yes | Resolution strategy (for example, `DenyOverridesStrategy`) |
| `options` | `AbilityResolverOptions<TTags>` | — | Additional options (see below) |

---

## `AbilityResolver` methods

| Method | Parameters | Returns | Description |
|---|---|---|---|
| **`resolve`** | `permission: Permission`, `resource: Resource`, `environment?: Environment` | `AbilityResult<Resource, Environment>` | **Manual access check.** Returns the result object. Use it when you need to handle denial yourself. |
| **`enforce`** | `permission: Permission`, `resource: Resource`, `environment?: Environment`, `options?: EnforceOptions` | `void` or `never` (throws) | **Automatic access check.** Throws `AbilityError` when access is denied. Recommended for API endpoints. |

### Parameter descriptions

| Parameter | Type | Description |
|---|---|---|
| `permission` | `Permission` | Permission key without the `permission.` prefix, for example `"orders.read"` or `"users.delete"`. |
| `resource` | `Resource` | Resource (subject) being checked. It must contain fields referenced by the policy and may be a partial object. |
| `environment` | `Environment` (optional) | Environment data: time, IP address, time zone, and other dynamic values. |
| `options` | `EnforceOptions` (optional, `enforce` only) | Additional options. See [EnforceOptions](#enforceoptions). |

### `EnforceOptions`

| Option | Type | Description |
|---|---|---|
| `onDeny` | `(result: AbilityResult) => void` | Callback invoked before an exception is thrown. Receives an `AbilityResult` with check details. |
| `onAllow` | `(result: AbilityResult) => void` | Callback invoked after a successful check. Receives an `AbilityResult` with check details. |

### `AbilityResult` methods

The `onDeny` and `onAllow` callbacks receive an `AbilityResult`. The following methods provide an explanation of the check:

| Method | Returns | Description |
|---|---|---|
| `explain()` | `string` | Alias for `explainToString()`, retained for backward compatibility. |
| `explainToString()` | `string` | Returns a text explanation with the result and trees of all participating policies. |
| `explainToJSON()` | `AbilityResultExplainJSON` | Returns a typed object with `permission`, the final `effect`, and JSON policy representations. |
| `decisive()` | `AbilityPolicy \| null` | Returns the policy that determined the strategy's final result, when identifiable. |
| `explainDecisive()` | `string \| null` | Returns a text explanation of the decisive policy. |

Example of using explanations in `onDeny`:

```ts
abilityResolver.enforce(
  'orders.read',
  { order },
  { hour: new Date().getHours() },
  {
    onDeny: result => {
      console.error(result.explainToString());
      console.log(JSON.stringify(result.explainToJSON(), null, 2));
    },
  },
);
```

The `explainToJSON()` result has this shape:

```ts
type AbilityResultExplainJSON = {
  permission: string;
  effect: 'permit' | 'deny';
  policies: AbilityExplainJSON[];
};
```

### Usage examples

#### `resolve` — manual check

```ts
// Manual access check
const result = abilityResolver.resolve('orders.read', { order }, {
  hour: new Date().getHours()
});

if (result.isDenied()) {
  // Handle denial yourself
  console.log('Access denied:', result.decisive()?.name);
  return null;
}

// Continue execution
return order;
```

#### `enforce` — automatic check

```ts
// Automatic check that throws an exception
abilityResolver.enforce('orders.read', { order }, {
  hour: new Date().getHours()
});

// If execution reaches this line, access was granted
return order;
```

#### `enforce` with a custom handler

```ts
// Automatic check with custom denial logic
abilityResolver.enforce('orders.read', { order }, {
  hour: new Date().getHours()
}, {
  onDeny: (result) => {
    // Log the textual explanation
    console.error('Access denied:', result.explainToString());

    // Use JSON for APIs, logging, or UI
    console.error(JSON.stringify(result.explainToJSON(), null, 2));

    // Send metrics or notifications
    metrics.increment('permission_denied', {
      permission: 'orders.read',
      policy: result.decisive()?.name
    });
  }
});
```

### Method typing

```ts
class AbilityResolver<P extends AbilityPolicy = AbilityPolicy> {
  // Manual check
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

  // Automatic check
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

## Options: `AbilityResolverOptions`

| Option | Type | Description |
|---|---|---|
| `tags` | `readonly TTags[]` | Filter policies by tags. Only policies with the specified tags are considered. Untagged policies are always used. |
| `onDeny` | `(result: AbilityResult) => void` | Callback invoked for every `deny`. Receives an `AbilityResult`. |
| `onAllow` | `(result: AbilityResult) => void` | Callback invoked for every `permit`. Receives an `AbilityResult`. |

### Tags

Each policy can have a list of tags. Tags can be used to:

- **Separate resolvers** — create resolvers for different contexts (admin / user areas)
- **Filter policies** — run checks only with selected tags
- **Simplify testing** — isolate policy groups for unit tests

#### Tag usage example

Define tagged policies in the DSL:

```text
@name "User management"
@tags admin, user-management
permit permission.users.* if any:
  user.role is equals "admin"

@name "Order viewing"
@tags user, orders
permit permission.orders.read if any:
  order.author is equals user.id
```

Now create several resolvers for different contexts:

```ts
// Resolver for administrators (only admin policies)
export const adminResolver = new AbilityResolver(policies, DenyOverridesStrategy, {
  tags: ['admin'],
});

// Resolver for regular users
export const userResolver = new AbilityResolver(policies, DenyOverridesStrategy, {
  tags: ['user'],
});

// Universal resolver (all policies)
export const fullResolver = new AbilityResolver(policies, DenyOverridesStrategy);
```

You can also use tags for a one-off runtime check:

```ts
// Check only with ['admin'] without creating a separate resolver
const result = abilityResolver.resolve('users.delete', { user }, {
  hour: new Date().getHours()
}, {
  tags: ['admin'] // Override tags for this check
});
```

> [!NOTE]
> Tags are specified in the resolver constructor and can be overridden when calling `enforce` or `resolve` through the options parameter.

> [!IMPORTANT]
> A policy without tags is always used, regardless of tag filtering.

---

## Frequently asked questions

### Can I use several resolvers?

Yes, but usually one resolver is sufficient. Create several when you need different strategies, tag filters, or policy sets.

### How do I debug policies?

Use `explainToString()` or `explainToJSON()` on an `AbilityResult` returned by `resolve()` or received in an `onDeny`/`onAllow` callback. `explain()` is an alias for `explainToString()`.

### Can I use the resolver without generated types?

Yes, but the linter will not be able to warn you about errors or typos.

### How do I add custom strategies?

Extend `AbilityStrategy` and implement its evaluation method. See the [strategy documentation](./strategies.md).

### Why does the example use `DenyOverridesStrategy`?

It is the standard strategy and follows the “one deny means deny” principle. If any policy denies access, the result is `deny`.

### How do I use tags for different roles?

Create separate resolvers with different tags or pass tags to `enforce`/`resolve`. For example, use a resolver with the `admin` tag for administrator API endpoints and one with the `user` tag for user endpoints.
