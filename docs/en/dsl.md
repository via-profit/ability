# Ability DSL

Ability DSL is a declarative language for describing access policies.  
It lets you express rules in a human-readable form and then use them at runtime to make decisions.

- [Basic structure](#basic-structure)
- [Policy](#policy)
    - [Permission key](#permission-key)
    - [Policy annotations](#policy-annotations)
- [Rule group](#rule-group)
    - [Group annotations](#group-annotations)
    - [Implicit group](#implicit-group)
    - [Excluding group (`except`)](#excluding-group-except)
- [Rule](#rule)
    - [Operator](#operator)
    - [Value](#value)
    - [Rule annotations](#rule-annotations)
- [Aliases](#aliases)
- [Environments](#environments)
- [Parse-time checks](#parse-time-checks)
- [Frequently asked questions](#frequently-asked-questions)

## Basic structure

```
# <comment-line>
@<annotation>
<effect> <permission> if <all|any>:
    <all|any> of: <subject> <operator> <value|resource|env>
    <all|any> of: <subject> <operator> <value|resource|env>
    ...
    except <all|any> of:
      <subject> <operator> <value|resource|env>
      <subject> <operator> <value|resource|env>
      ...
```

where:

- `comment-line` — a comment
- `annotation` — an annotation (`@id`, `@name`, `@description`, `@disabled`, `@tags`, `@priority`)
- `effect` — `permit` or `deny`
- `permission` — a permission key with the `permission.` prefix (for example, `permission.order.update`)
- `all` / `any` — a logical operator for a rule group
- `except` — the beginning of an exception block

## Policy

A policy consists of this construction:

```
@<annotation>
<effect> <permission> if <all|any>:
  <group>
  <group>
```

where:

- **annotation** — an annotation (`@id`, `@name`, `@disabled`, `@description`, `@tags`, `@priority`);
- **effect** — `permit` | `allow` or `deny` | `forbidden`;
- **permission** — a permission key (permission key), a string in the form `permission.foo.bar`  
  (the `permission.` prefix is required in the DSL but is automatically removed by the parser);
- **if all:** — all groups must be true;
- **if any:** — at least one group must be true;
- a policy may contain one or more rule groups.

**Example**

```dsl
permit permission.order.update if any:
  all of:
    user.roles contains 'admin'
    user.token is not null

  any of:
    user.roles contains 'developer'
    user.login is equals 'dev'
```

### Permission key

Permission keys use `dot notation` and support wildcard patterns with `*`. This lets you group permissions and override behavior for entire families of operations.

**Wildcard pattern example**

| Policy (permission) | Key | Matches |
|---|---|---|
| `order.*` | `order.create` | yes |
| `order.*` | `order.update` | yes |
| `order.*` | `user.create` | no |
| `*.create` | `order.create` | yes |
| `*.create` | `user.create` | yes |
| `order.*` | `order.item.update` | yes |
| `*.create` | `order.update` | no |
| `*.create` | `order.item.create` | no |
| `user.profile.*` | `user.profile.update` | yes |
| `user.profile.*` | `user.settings.update` | no |
| `*` | any key | yes |

- `*` at the end of a key matches all remaining segments (`order.*` → `order.item.update`).
- `*` in the middle of a key matches exactly one segment (`*.create` → `order.create`, but not `order.item.create`).

### Policy annotations

Annotations are specified **directly before the policy definition** (one per line). Format:
`@annotation_name value`.

#### Supported annotations

| Annotation | Value type | Required | Default value | Description |
|---|---|---|---|---|
| `@id` | string | no | generated automatically | Unique policy identifier. |
| `@name` | string | no | generated from the rule | Human-readable policy name. |
| `@description` | string | no | — | Detailed description of what the policy allows or denies. |
| `@tags` | comma-separated list of strings | no | `[]` | Tags for filtering policies in `AbilityResolver`. |
| `@disabled` | `true` or `false` | no | `false` | When `true`, the policy is ignored during checks. |
| `@priority` | integer | no | `-1` | The higher the number, the earlier the policy is applied (all else being equal). |

#### Usage examples

```dsl
# A simple policy with an ID and name
@id order_delete_001
@name "Only an administrator can delete an order"
permit permission.order.delete if all:
  user.roles contains 'admin'

# A disabled policy with tags
@disabled true
@tags security, audit, v2
deny permission.user.block if any:
  user.status in ['blocked', 'deleted']

# A policy with priority and description
@priority 10
@description "Allows authors and moderators to edit drafts"
permit permission.post.update if all:
  user.id equals post.author_id
  any of:
    user.roles contains 'moderator'
```

#### Notes

- If `@name` is not specified, the ID is used as the name. If `@id` is not specified, it is generated from the policy content (effect, key, groups and rules), so identical policies get the same ID and different policies get different IDs.
- String annotation values (`@name`, `@description`, `@id`) **do not need** quotes when they contain no spaces. Use double quotes when they contain spaces.
- Tags in `@tags` are comma-separated; spaces after commas are ignored. A tag cannot contain spaces or square brackets: write `@tags admin, user`, not `@tags ["admin", "user"]`.
- `@priority` is an integer, including `0` and negative values.
- `@disabled` without a value is the same as `@disabled true`.
- Priority affects policy evaluation order in a strategy (for example, `FirstMatchStrategy` or `PriorityStrategy`). By default, all policies have priority `-1` and are evaluated in declaration order.
- Unknown annotations, repeated annotations and annotations without a value cause a parse error. See [Parse-time checks](#parse-time-checks).

## Rule group

A group defines how rules within it are combined:

```
all of:
  <rule>
  <rule>

any of:
  <rule>
  <rule>
```

- `all of:` — logical AND
- `any of:` — logical OR

`all of` means that a group is fulfilled when all rules in it match.

`any of` means that a group is fulfilled when at least one rule in it matches.

Each group in a policy is evaluated independently of the other groups. The final result is determined by comparing the evaluation results of all groups in the policy.

### Group annotations

Annotations are specified **directly before the group definition** (one per line). Format:
`@annotation_name value`.

#### Supported annotations

| Annotation | Value type | Required | Default value | Description |
|---|---|---|---|---|
| `@id` | string | no | generated automatically | Unique group identifier. |
| `@name` | string | no | generated from the rule | Human-readable group name. |
| `@description` | string | no | — | Detailed description of what the group checks. |
| `@disabled` | `true` or `false` | no | `false` | When `true`, the group is ignored during checks. |

### Implicit group

When rules are written without `all of:` or `any of:`, they are combined using the policy operator:

```dsl
permit permission.order.update if all:
  user.roles contains 'admin'
  user.token is not null
```

Equivalent to:

```dsl
permit permission.order.update if all:
  all of:
    user.roles contains 'admin'
    user.token is not null
```

_Note: An implicit group always uses the policy operator (`if all` or `if any`)._

> **Note:** It is recommended to avoid using implicit and explicit groups together in one policy.
>
> Implicit groups are intended for short, simple policies whose structure is obvious without additional keywords. Explicit groups (`all of:`, `any of:`) are intended for more complex logic and always have priority in the parse structure.
>
> Mixing these approaches can make a policy's structure ambiguous. In particular, an explicit group can absorb all subsequent implicit groups, interpreting their rules as part of its own scope.

### Excluding group (`except`)

The `except` block defines conditions that cancel a policy's action even when the main rule group has matched.

- If the main group is true and the `except` block is fulfilled → the policy is not applied (as if it did not match). The effect is **not** inverted: a `deny` policy does not turn into `permit`, it simply does not take part in the decision.
- If the main group is false → `except` is not checked.
- A policy cannot consist of an `except` block only — it must contain at least one rule outside `except`.

> [!NOTE]
> An excluding group cancels a policy after its main group has already returned “true”. In other words, the policy looks ready to match, but `except` says: “not under these conditions.” It is most often used with denying policies (`deny`).

```dsl
@name "Only an authorized user can edit an order"
permit permission.order.update if all:
  user.token is not null

@name "Updating a completed order is forbidden, except for administrators"
deny permission.order.update if all:
  all of:
    order.status equals 'completed'
  except any of:
    user.role is equals 'administrator'
```

**How it works**

**1. Authorization check**
- If `user.token is not null` → **permit** order updates.
- If there is no token → the main group is false → the policy is not applied → no permission is granted → the final result is **deny** if there are no other permitting policies.

**2. Order status check**
- If `order.status equals 'completed'` → the main group is true → the policy is ready to return **deny**.

**2.1. Exception check (`except`)**
- If `user.role equals 'administrator'` → `except` is true → **deny is canceled** → the final result is **permit** (granted by the first policy).
- If the role is not administrator → `except` is false → **deny is applied** → updating a completed order is forbidden.

## Rule

A rule is an atomic condition inside a policy. It determines which data makes a policy match. Rules define the conditions used to determine the policy effect (`permit` or `deny`).

*Rule structure*:

```
<subject> <operator> <value|resource|env>
```

where:

- **subject** — a dot-notation path to the checked field of the resource or environment (`user.age`, `env.ip`). The left side of a rule is **always** a path, even without a dot: `role equals 'admin'` compares the `role` field of the resource.
- **operator** — a comparison operator.
- **value** — a value or a path:
    - **literal** — a quoted string (`'admin'`, `"1.2.3"`), a number, `true`/`false`, `null` or an array;
    - **path** — an **unquoted** identifier with a dot (`user.id`, `env.request.ip`).

A quoted string is always a literal, even if it contains dots: `domain.version is equals '1.2.3'` compares the version with the string `1.2.3`. An unquoted identifier without a dot on the right side is a parse error (most likely the quotes are missing).

_A value is not required for every operator (for example, `is null` needs no value)._

**Rule examples**

```
# A simple rule
user.role equals "admin"

# Compare a number
user.age >= 18

# Check membership in an array
user.status in ["active", "verified"]

# Compare with another resource field (unquoted path)
document.author equals user.id

# A string with dots is a literal because it is quoted
domain.version equals '1.2.3'
```

### Operator

**Basic comparison operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **is equals** | `=`, `==`, `is` `equals` | `user.age is equals 18` | Strict equality | number, string, boolean |
| **is not equals** | `!=`, `<>`, `not equals` | `user.role is not equals 'admin'` | Strict inequality | number, string, boolean |
| **greater than** | `>`, `gt`, `greater` | `user.age greater than 18` | Greater than | number |
| **greater than or equal** | `>=`, `gte` | `user.age greater than or equal 18` | Greater than or equal | number |
| **less than** | `<`, `lt`, `less` | `user.age less than 18` | Less than | number |
| **less than or equal** | `<=`, `lte` | `user.age less than or equal 18` | Less than or equal | number |

Numbers can be decimal and negative: `account.balance >= -100`, `order.discount < 0.25`.

Comparison is strict (`===`) with no type coercion: `user.age equals '18'` does not match the number `18`. Comparison operators work with numbers only. If either value is not a number, the rule does not match.

**Null operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **is null** | `== null`, `= null` | `user.middleName is null` | Value is strictly `null` | any |
| **is not null** | `!= null` | `user.middleName is not null` | Value is strictly not `null` | any |

> [!IMPORTANT]
> The `null` check is strict: `undefined` is **not** `null`. If the field is missing from the resource, `is null` does not match and `is not null` **does** match. To check that a value is really set, use `is defined` and `is not null` together:
>
> ```dsl
> permit permission.order.update if all:
>   user.token is defined
>   user.token is not null
> ```

**Defined operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **is defined** | — | `user.middleName is defined` | Value is defined (not `undefined`, may be `null`) | any |
| **is not defined** | — | `user.middleName is not defined` | Value is not defined (`undefined`) | any |

**List (array) operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **in [...]** | `is in` | `user.role in ['admin', 'manager']` | Value is in the list | number, string |
| **not in [...]** | — | `user.role not in ['banned']` | Value is not in the list | number, string |
| **contains** | `includes`, `has` | `user.tags contains 'vip'` | Array contains the item | array |
| **not contains** | `not includes`, `not has` | `user.tags not contains 'vip'` | Array does not contain the item | array |
| **contains all** | — | `user.roles contains all ['editor', 'writer']` | Array contains **all** items of the list | array |
| **contains any** | — | `user.roles contains any ['admin', 'owner']` | Array contains **at least one** item | array |

- When `contains` gets an array, it works like `contains any`. For readability, prefer writing `contains any` explicitly.
- `contains all` and `contains any` with an empty array `[]` is a parse error. If the right side is a path and the array there is empty, the rule does not match.
- If the left side is not an array, list operators do not match.

**Boolean operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **is true** | `= true` | `user.isActive is true` | Value is true | boolean |
| **is false** | `= false` | `user.isActive is false` | Value is false | boolean |

**Note:** `is true` and `is false` are syntactic sugar for `equals true` / `equals false`.

**Length operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **length equals** | `length =`, `len =` | `user.tags length equals 3` | Length is equal | array, string |
| **length greater than** | `length >`, `len >` | `user.tags length greater than 2` | Length is greater | array, string |
| **length less than** | `length <`, `len <` | `user.tags length less than 5` | Length is less | array, string |

**Emptiness operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **is empty** | — | `user.tags is empty` | Empty string `''` or empty array `[]` | array, string |
| **is not empty** | — | `user.tags is not empty` | Non-empty string or non-empty array | array, string |

For `null`, `undefined` and other types, neither operator matches.

**String operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **starts with** | — | `file.path starts with '/public/'` | String starts with | string |
| **ends with** | — | `user.email ends with '@example.com'` | String ends with | string |

The comparison is case-sensitive. If either value is not a string, the rule does not match. The right side can be a path: `file.path starts with user.homeDir`.

**Special operators**

| DSL operator | Aliases | Example | Description | Types |
|---|---|---|---|---|
| **always** | — | `always` | A condition that is always true. Used for global permission or simpler logic. | special operator |
| **never** | — | `never` | A condition that is always false. Used for global denial or disabling a rule. | special operator |

### `always`

An operator that always returns `true`.  
It is used for:

- global permission (`permit permission.* if all: always`)
- testing
- disabling complex conditions
- creating fallback rules

### `never`

An operator that always returns `false`.  
It is used for:

- global denial (`deny permission.* if all: never`)
- temporarily disabling a rule
- explicit denial without conditions

### Value

The following values are supported:

- strings in single or double quotes: `'text'`, `"text"`
- numbers, including decimal and negative ones: `42`, `-1`, `0.5`
- booleans `true` / `false`
- `null`
- arrays of literals `[1, 2, 3]` / `['foo', false, null, 1, -2, 3.5]`
- unquoted paths: `user.id`, `env.request.ip`

Only literals are allowed inside an array: paths and nested arrays are parse errors.

Value examples:

```dsl
# User age is greater than 18
user.age greater than 18

# The roles array contains 'admin'
user.roles contains 'admin'

# The order tag is either 'vip' or 'priority'
order.tag in ['vip', 'priority']

# The user token is not null
user.token is not null

# Check that the user has a middle name (even when it is null)
user.middleName is defined
```

### Rule annotations

Annotations are specified **directly before the rule definition** (one per line). Format:
`@annotation_name value`.

#### Supported annotations

| Annotation | Value type | Required | Default value | Description |
|---|---|---|---|---|
| `@id` | string | no | generated automatically | Unique rule identifier. |
| `@name` | string | no | generated from the rule | Human-readable rule name. |
| `@description` | string | no | — | Detailed description of what the rule checks. |
| `@disabled` | `true` or `false` | no | `false` | When `true`, the rule is ignored during checks. |

## Aliases

Aliases are predefined rules assigned a unique key. An alias can be used by name in a policy without repeating the rule itself.

An alias always represents exactly one rule.

**Example:**

```
@name "User is administrator"
alias isAdmin:
  user.roles contains 'admin'

permit permission.order.update if any:
  user.roles contains 'writer'
  isAdmin
```

Alias rules:

- an alias must be defined **before** it is used in a policy, otherwise the parser throws `Unknown alias`;
- the alias name is unique and contains no dots;
- the alias body contains exactly one rule;
- rule annotations (`@name`, `@id`, `@description`, `@disabled`) can be specified before an alias usage in a policy — they apply to that usage only.

### Alias annotations

Annotations are specified **directly before the alias definition** (one per line). Format:
`@annotation_name value`.

#### Supported annotations

| Annotation | Value type | Required | Default value | Description |
|---|---|---|---|---|
| `@name` | string | no | generated from the alias key | Human-readable alias/rule name. |
| `@description` | string | no | — | Detailed description of what the alias/rule checks. |
| `@disabled` | `true` or `false` | no | `false` | When `true`, the alias is ignored during checks. |

## Environments

On the right side of a rule, you can reference not only literals or resource fields, but also **environment variables** — data passed when a policy is checked (for example, the request IP address, current time, or session parameters).

### Syntax

Use the `env.` prefix before a dot-notation path:

```
<subject> <operator> env.<path>
```

### Examples

```
# Allow access only from localhost
permit permission.admin if all:
  env.request.ip equals '127.0.0.1'

# Deny operations after business hours (the hour is passed as a number)
deny permission.order.create if all:
  env.hour greater than or equal 18

# Check a role from the authentication context
permit permission.report.view if all:
  env.auth.roles contains 'analyst'
```

### Where are environment variables used?

- In `resource` (the right side of a rule)
- In `subject` when `env` is specified
- On the left side (for example, `env.user.id equals resource.owner_id`)

### Notes

- Environment is passed separately from the main resource as the third argument of `resolve()` or `enforce()`.
- Paths with the `env.` prefix are looked up **only** in the environment. An `env` field inside the resource is not used, so the environment cannot be spoofed through resource data.
- If a path under `env.` does not exist or no environment is passed, its value is considered `undefined`.
- Do not confuse `env.` with fields on the resource itself; they belong to different contexts.

## Parse-time checks

The parser throws `AbilityDSLSyntaxError` (exported from the package) with the line, column and a DSL fragment when:

- **Annotations**
    - an annotation is unknown (for example, the typo `@diasbled` — the parser suggests `@disabled`);
    - the same annotation is specified twice for one element;
    - `@id`, `@name`, `@description`, `@priority`, `@tags` have no value;
    - `@priority` is not an integer, `@disabled` is not `true`/`false`;
    - `@tags` contains an empty tag or a tag with spaces/brackets;
    - an annotation is not allowed for the element (for example, `@priority` on a group);
    - an annotation is not attached to anything (for example, at the end of the file).
- **Identifiers**: an `@id` is repeated for two policies, two groups or two rules.
- **Structure**
    - a policy without rules;
    - a policy that has only an `except` block;
    - an empty `all of:` / `any of:` group or an empty `except` block.
- **Aliases**: an unknown alias (or usage before definition), a repeated definition, an alias without a rule or with several rules, a dot in the alias name.
- **Values**
    - an unquoted identifier without a dot on the right side (`user.role equals admin`);
    - a path or a nested array inside an array;
    - a value that does not fit the operator: comparison (`>`, `<`, length) — not a number; `in` / `not in` — not an array; `starts with` / `ends with` — not a string; `equals` — an array; `contains all` / `contains any` — an empty array.
- **Lexical**: an unknown character, an unterminated string, an invalid number (`10abc`, `10.`).

---

## Frequently asked questions

### Why is `except` not checked when the main group is false?

Because `except` is an exception to an already matched policy, not an alternative branch. If the policy is not going to be applied (the main group is false), there is nothing to cancel.

### Why are there no date operators?

Comparing dates requires parsing formats, time zones and calendar logic — this contradicts the idea of a lightweight engine without dependencies. Pass dates and time as numbers (timestamp, hour, day of week) through the resource or environment and compare them with the regular numeric operators: `env.hour less than 18`, `order.createdAt greater than env.minTimestamp`.

### What happens to disabled rules and groups?

Disabled (`@disabled`) rules and groups do not take part in the check. If a group has no active rules left, the group is skipped. If a policy has no active groups left, the policy **does not match** — it cannot grant `permit` or `deny` without conditions.
