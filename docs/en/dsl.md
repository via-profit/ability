```markdown
# Ability DSL

Ability DSL is a declarative language for describing access policies.  
It allows you to describe rules in a human-readable form and then use them at runtime to make decisions.

- [Basic Structure](#basic-structure)
- [Policy](#policy)
    - [Permission Key](#permission-key)
    - [Policy Annotations](#policy-annotations)
- [Rule Set](#rule-set)
    - [Rule Set Annotations](#rule-set-annotations)
    - [Implicit Group](#implicit-group)
    - [Except Group](#except-group)
- [Rule](#rule)
    - [Operators](#operators)
    - [Values](#values)
    - [Rule Annotations](#rule-annotations)
- [Aliases](#aliases)
- [Environments](#environments)

## Basic Structure

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

Where:

- `comment-line` - comment
- `annotation` - annotation (`@id`, `@name`, `@description`, `@disabled`, `@tags`, `@priority`)
- `effect` – `permit` or `deny`
- `permission` – permission key with `permission.` prefix (e.g., `permission.order.update`)
- `all` / `any` – logical operator for the rule group
- `except` - start of the exception block

## Policy

A policy consists of the following structure:

```
@<annotation>
<effect> <permission> if <all|any>:
<group>
<group>
```

Where:

- **annotation** - annotation (`@id`, `@name`, `@disabled`, `@description`, `@tags`, `@priority`);
- **effect** — `permit` | `allow` or `deny` | `forbidden`
- **permission** — permission key - a string like `permission.foo.bar`  
  (the `permission.` prefix is required in DSL but automatically removed by the parser)
- **if all:** — all groups must be true
- **if any:** — at least one group must be true
- a policy can contain one or more rule groups

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

### Permission Key

Permission keys are written in `dot notation` and support wildcard patterns using the `*` character.  
This allows you to group permissions and override behavior for entire families of operations.

**Example of wildcard pattern usage**

| Policy (permission) | Key                    | Matches |
|---------------------|------------------------|---------|
| `order.*`           | `order.create`         | yes     |
| `order.*`           | `order.update`         | yes     |
| `order.*`           | `user.create`          | no      |
| `*.create`          | `order.create`         | yes     |
| `*.create`          | `user.create`          | yes     |
| `*.create`          | `order.update`         | no      |
| `user.profile.*`    | `user.profile.update`  | yes     |
| `user.profile.*`    | `user.settings.update` | no      |


### Policy Annotations

Annotations are specified **immediately before the policy definition** (each on a separate line). Format:
`@annotation_name value`.

#### Supported Annotations

| Annotation    | Value Type                     | Required | Default Value              | Description                                                              |
|---------------|--------------------------------|----------|----------------------------|--------------------------------------------------------------------------|
| `@id`         | string                         | no       | auto-generated             | Unique policy identifier.                                                |
| `@name`       | string                         | no       | generated from the rule    | Human-readable policy name.                                              |
| `@description`| string                         | no       | –                          | Detailed description of what the policy permits/denies.                  |
| `@tags`       | comma-separated list of strings| no       | `[]`                       | Tags for filtering policies in `AbilityResolver`.                        |
| `@disabled`   | `true` or `false`              | no       | `false`                    | If `true`, the policy is ignored during evaluation.                      |
| `@priority`   | integer                        | no       | `-1`                       | Higher number means the policy is evaluated earlier (all else being equal). |

#### Usage Examples

```dsl
# Simple policy with ID and name
@id order_delete_001
@name "Only an administrator can delete an order"
permit permission.order.delete if all:
  user.roles contains 'admin'

# Disabled policy with tags
@disabled true
@tags security, audit, v2
deny permission.user.block if any:
  user.status in ['blocked', 'deleted']

# Policy with priority and description
@priority 10
@description "Allows editing drafts by the author and moderators"
permit permission.post.update if all:
  user.id equals post.author_id
  any of:
    user.roles contains 'moderator'
```

#### Notes

- If `@name` is not specified, the name is automatically generated based on the rule.
- String annotation values (`@name`, `@description`, `@id`) **do not need** to be quoted unless they contain spaces. If spaces are present, use double quotes.
- In `@tags`, tags are separated by commas; spaces after commas are ignored.
- Priority affects the evaluation order of policies in strategies (e.g., `DenyOverridesStrategy` or `PermitOverridesStrategy`). By default, all policies have priority `-1` and are evaluated in declaration order.

## Rule Set

A rule set defines how rules are combined within it:

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

`all of` means the group is considered satisfied if all rules within the group are true.

`any of` means the group is considered satisfied if at least one rule within the group is true.

Each rule set within a policy is evaluated independently of other rule sets. The final evaluation result is determined by comparing the results of all rule sets in the policy according to the policy's logical operator.

### Rule Set Annotations

Annotations are specified **immediately before the rule set definition** (each on a separate line). Format:
`@annotation_name value`.

#### Supported Annotations

| Annotation    | Value Type          | Required | Default Value              | Description                                      |
|---------------|---------------------|----------|----------------------------|--------------------------------------------------|
| `@id`         | string              | no       | auto-generated             | Unique rule set identifier.                      |
| `@name`       | string              | no       | generated from the rule    | Human-readable rule set name.                    |
| `@description`| string              | no       | –                          | Detailed description of what the rule set checks.|
| `@disabled`   | `true` or `false`   | no       | `false`                    | If `true`, the rule set is ignored during evaluation. |

### Implicit Group

If rules are written without `all of:` or `any of:`, they are combined using the policy's operator:

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

_Note: An implicit group always uses the policy's operator (`if all` or `if any`)._

> **Note:** It is recommended to avoid mixing implicit and explicit groups in the same policy.
>
> Implicit groups are intended for short, simple policies where the structure is obvious without additional keywords. Explicit groups (`all of:`, `any of:`) are used for more complex logic and always take precedence in the parsing structure.
>
> Mixing these two approaches can lead to ambiguous perception of the policy structure. In particular, a situation may arise where an explicit group consumes all subsequent implicit groups, interpreting their rules as part of its own scope.

### Except Group

The `except` block defines conditions that override the policy's effect, even if the main rule set is satisfied.

- If the main rule set is true and at least one condition in `except` is true → the policy does NOT apply (effect is inverted).
- If the main rule set is false → `except` is not evaluated.

```dsl
# Allow a manager to edit an order,
# but deny if the order is already paid or cancelled
permit permission.order.update if all:
  all of:
    user.roles contains 'manager'
  except any of:
    order.status equals 'paid'
    order.status equals 'cancelled'
```

**How it works:**

1. Check `user.roles contains 'manager'` — if manager, proceed to `except`.
2. If the order status is `paid` or `cancelled` — permission is denied.
3. Otherwise — permission is granted.

## Rule

A rule is an atomic condition within a policy. It defines what data makes the policy match. Rules specify the conditions that determine the policy's effect (`permit` or `deny`).

*Rule structure*:

```
<subject> <operator> <value|resource|env>
```

Where:
**subject** - dot-notation path to the subject field being checked
**operator** - comparison operator
**value** - value (or resource)

_A value is not required for all operators (e.g., the `is null` operator does not need a value)._

**Rule Examples**

```
# Simple rule
user.role equals "admin"

# Comparison with a number
user.age >= 18

# Array membership check
user.status in ["active", "verified"]
```

### Operators

**Basic Comparison Operators**

| DSL Operator               | Synonyms                 | Example                         | Description           | Supported Types        |
|----------------------------|--------------------------|--------------------------------|-----------------------|------------------------|
| **is equals**              | `=`, `==`, `is`, `equals`| `age is equals 18`             | Strict equality       | number, string, boolean|
| **is not equals**          | `!=`, `<>`, `not equals` | `role is not equals 'admin'`   | Strict inequality     | number, string, boolean|
| **greater than**           | `>`, `gt`                | `age greater than 18`          | Greater than          | number, date           |
| **greater than or equal**  | `>=`, `gte`              | `age greater than or equal 18` | Greater than or equal | number, date           |
| **less than**              | `<`, `lt`                | `age less than 18`             | Less than             | number, date           |
| **less than or equal**     | `<=`, `lte`              | `age less than or equal 18`    | Less than or equal    | number, date           |

**Null Operators**

| DSL Operator    | Synonyms              | Example                       | Description                 | Types |
|-----------------|-----------------------|-------------------------------|-----------------------------|-------|
| **is null**     | `== null`, `= null`   | `middleName is null`          | Value is absent             | any   |
| **is not null** | `!= null`             | `middleName is not null`      | Value is present            | any   |

**Defined Operators**

| DSL Operator       | Synonyms | Example                               | Description                            | Types |
|--------------------|----------|---------------------------------------|----------------------------------------|-------|
| **is defined**     | -        | `user.middleName is defined`          | Value is defined (not `undefined`)     | any   |
| **is not defined** | -        | `user.middleName is not defined`      | Value is not defined (`undefined`)     | any   |

**Array Operators**

| DSL Operator     | Synonyms                     | Example                            | Description                       | Supported Types |
|------------------|------------------------------|------------------------------------|-----------------------------------|-----------------|
| **in [...]**     | -                            | `role in ['admin', 'manager']`     | Value is in the list              | number, string  |
| **not in [...]** | -                            | `role not in ['banned']`           | Value is not in the list          | number, string  |
| **contains**     | `includes`, `has`            | `tags contains 'vip'`              | Array contains element            | array           |
| **not contains** | `not includes`, `not has`    | `tags not contains 'vip'`          | Array does not contain element    | array           |

**Boolean Operators**

| DSL Operator | Synonyms  | Example               | Description           | Types   |
|--------------|-----------|-----------------------|-----------------------|---------|
| **is true**  | `= true`  | `isActive is true`    | Value is true         | boolean |
| **is false** | `= false` | `isActive is false`   | Value is false        | boolean |

**Note:** The `is true` and `is false` operators are syntactic sugar for `equals true` / `equals false`.

**Length Operators**

| DSL Operator            | Synonyms   | Example                           | Description       | Supported Types |
|-------------------------|------------|-----------------------------------|-------------------|-----------------|
| **length equals**       | `length =` | `tags length equals 3`            | Length is equal   | array, string   |
| **length greater than** | `length >` | `tags length greater than 2`      | Length is greater | array, string   |
| **length less than**    | `length <` | `tags length less than 5`         | Length is less    | array, string   |

**Special Operators**

| DSL Operator | Synonyms | Example  | Description                                                                                    | Types               |
|--------------|----------|----------|------------------------------------------------------------------------------------------------|---------------------|
| **always**   | —        | `always` | Always true. Used for global permission or simplifying logic.                                  | special operator    |
| **never**    | —        | `never`  | Always false. Used for global denial or disabling a rule.                                      | special operator    |

**always**

An operator that always returns `true`.  
Used for:

- global permission (`permit permission.* if all: always`)
- testing
- disabling complex conditions
- creating fallback rules

**never**

An operator that always returns `false`.  
Used for:

- global denial (`deny permission.* if all: never`)
- temporarily disabling a rule
- explicit negation without conditions

### Values

Supported values:

- strings `'text'`
- numbers `42`
- booleans `true` / `false`
- `null`
- arrays `[1, 2, 3]` / `['foo', false, null, 1, 2, '999']`

Value examples:

```dsl
# user age is greater than 18
user.age greater than 18

# role array contains 'admin'
user.roles contains 'admin'

# order tag is either 'vip' or 'priority'
order.tag in ['vip', 'priority']

# user token is not null
user.token is not null

# check that the user has a middle name (even if it's null)
user.middleName is defined
```

### Rule Annotations

Annotations are specified **immediately before the rule definition** (each on a separate line). Format:
`@annotation_name value`.

#### Supported Annotations

| Annotation    | Value Type          | Required | Default Value              | Description                                       |
|---------------|---------------------|----------|----------------------------|---------------------------------------------------|
| `@id`         | string              | no       | auto-generated             | Unique rule identifier.                           |
| `@name`       | string              | no       | generated from the rule    | Human-readable rule name.                         |
| `@description`| string              | no       | –                          | Detailed description of what the rule checks.     |
| `@disabled`   | `true` or `false`   | no       | `false`                    | If `true`, the rule is ignored during evaluation. |

## Aliases

Aliases are pre-defined rules that are assigned a unique key. An alias can be used in a policy by name, without repeating the rule itself.

An alias always represents a single rule.

**Example:**
```
@name is user administrator
alias isAdmin:
  user.roles.contains 'admin'

permit permission.order.update if any:
  user.rules.contains 'writer'
  isAdmin
```

An alias must be defined **before** it is used in a policy.

### Alias Annotations

Annotations are specified **immediately before the alias definition** (each on a separate line). Format:
`@annotation_name value`.

#### Supported Annotations

| Annotation    | Value Type          | Required | Default Value              | Description                                           |
|---------------|---------------------|----------|----------------------------|-------------------------------------------------------|
| `@name`       | string              | no       | generated from alias key   | Human-readable alias/rule name.                       |
| `@description`| string              | no       | –                          | Detailed description of what the alias/rule checks.   |
| `@disabled`   | `true` or `false`   | no       | `false`                    | If `true`, the alias is ignored during evaluation.    |

## Environments

On the right side of a rule, you can reference not only literals or resource fields, but also **environment variables** — data passed at the time of policy evaluation (e.g., request IP, current time, session parameters).

### Syntax

Use the `env.` prefix before a dot‑notation path:

```
<subject> <operator> env.<path>
```

### Examples

```
# Allow access only from localhost
permit permission.admin if all:
  env.request.ip equals '127.0.0.1'

# Deny operations after business hours
deny permission.order.create if all:
  env.current_time greater_than '18:00'

# Check role from authentication context
permit permission.report.view if all:
  env.auth.roles contains 'analyst'
```

### Where are environment variables used?

- In `resource` (right side of a rule)
- In `subject`, if specified with `env.`
- On the left side (e.g., `env.user.id equals resource.owner_id`)

### Notes

- Environment is passed separately from the main resource when calling `resolve()` or `enforce()`.
- If a path in `env.` does not exist, the value is considered `undefined` (the rule does not match unless the operator checks `is null`).
- Do not confuse `env.` with resource fields — they belong to different contexts.
