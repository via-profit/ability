# @via-profit/ability - Resolution strategies

[![npm version](https://img.shields.io/npm/v/@via-profit/ability)](https://www.npmjs.com/package/@via-profit/ability)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A resolution strategy determines **how** the final decision is made when several policies produce conflicting results (some allow and others deny access).

> 📚 **Related documents:**
> - [Resolver](./resolver.md)
> - [DSL](./dsl.md)
> - [Type generation](./types-generator.md)

---

## Table of contents

- [What is a strategy](#what-is-a-strategy)
- [Available strategies](#available-strategies)
- [Usage](#usage)
- [Creating a custom strategy](#creating-a-custom-strategy)
- [Example scenarios](#example-scenarios)
- [Frequently asked questions](#frequently-asked-questions)

---

## What is a strategy

A strategy is a class that implements the logic for making the final decision based on **matched** policies. When the resolver runs a check, it finds all policies whose conditions are **matched** (their conditions are satisfied), passes them to the strategy, and the strategy returns the final result.

Every strategy is a class that must extend `AbilityStrategy`.

The base `AbilityStrategy` class provides methods for working with the list of matched policies:

| Method | Description |
|---|---|
| `matchedPolicies()` | Returns all matched policies |
| `hasMatched()` | Checks whether any policies matched |
| `firstMatched()` | Returns the first matched policy |
| `lastMatched()` | Returns the last matched policy |
| `firstDenied()` | Returns the first policy with the `deny` effect |
| `firstPermitted()` | Returns the first policy with the `permit` effect |
| `getPermitPolicies()` | Returns all policies with the `permit` effect |
| `getDenyPolicies()` | Returns all policies with the `deny` effect |
| `hasPermit()` | Whether there is at least one `permit` policy |
| `hasDeny()` | Whether there is at least one `deny` policy |
| `isAllowed()` | Returns `true` when the final effect is `permit` |
| `isDenied()` | Returns `true` when the final effect is `deny` |

---

## Available strategies

| Strategy | Behavior | When to use |
|---|---|---|
| **`DenyOverridesStrategy`** | If at least one `deny` exists → `deny`, otherwise `permit` | **Default.** A secure approach where denial overrides permission |
| **`PermitOverridesStrategy`** | If at least one `permit` exists → `permit`, otherwise `deny` | When you need to grant the maximum possible access |
| **`FirstMatchStrategy`** | Result of the first matched policy | Priority follows policy declaration order |
| **`SequentialLastMatchStrategy`** | Result of the last matched policy | Later declarations have priority |
| **`PriorityStrategy`** | Selects the policy with the highest `priority` | Explicit priority control through `@priority` |
| **`AllMustPermitStrategy`** | `permit` only if **all** matched policies are `permit` | Maximum strictness (consensus) |
| **`OnlyOneApplicableStrategy`** | `deny` when more than one policy matched | Prohibiting multiple policies for one resource |
| **`AnyPermitStrategy`** | `permit` when at least one `permit` exists | Broad access |

---

## Usage

### Basic usage

```typescript
import {
  AbilityResolver,
  DenyOverridesStrategy,
  PermitOverridesStrategy,
  PriorityStrategy
} from '@via-profit/ability';

// The default strategy
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// A strategy where permission takes precedence
const permissiveResolver = new AbilityResolver(policies, PermitOverridesStrategy);

// A priority-based strategy
const priorityResolver = new AbilityResolver(policies, PriorityStrategy);
```

### Priority-based strategy

```typescript
// DSL with priorities
const dsl = `
  @name "Regular access"
  @priority 10
  permit permission.document.read if all:
    user.role equals "user"

  @name "Administrator access"
  @priority 100
  permit permission.document.read if all:
    user.role equals "admin"
`;

const policies = new AbilityDSLParser(dsl).parse();
const resolver = new AbilityResolver(policies, PriorityStrategy);

// The policy with @priority 100 will be used
resolver.enforce('document.read', {
  document: { id: '123' }
}, {
  user: { role: 'admin' }
});
```

---

## Creating a custom strategy

You can create your own strategy by extending the base `AbilityStrategy` class:

```typescript
import {
  AbilityStrategy,
  AbilityPolicy,
  AbilityPolicyEffect,
  AbilityPolicyEffectType,
  ResourceObject,
  EnvironmentObject
} from '@via-profit/ability';

/**
 * A strategy that requires the number of permits
 * to exceed the number of denies.
 */
export class MajorityStrategy<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<Resource, Environment> {
  private decisive: AbilityPolicy<Resource, Environment> | null = null;

  evaluate(): AbilityPolicyEffectType {
    const permits = this.getPermitPolicies();
    const denies = this.getDenyPolicies();

    // Deny when no policies matched
    if (!this.hasMatched()) {
      this.decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // When there are more permits than denies
    if (permits.length > denies.length) {
      this.decisive = permits[0] || null;
      return AbilityPolicyEffect.permit;
    }

    // When denies are equal to or greater than permits
    this.decisive = denies[0] || null;
    return AbilityPolicyEffect.deny;
  }

  decisivePolicy(): AbilityPolicy<Resource, Environment> | null {
    return this.decisive;
  }
}

// Usage
const resolver = new AbilityResolver(policies, MajorityStrategy);
```

### Example: business-hours strategy

```typescript
/**
 * A strategy that allows access only during business hours.
 */
export class BusinessHoursStrategy<
  Resource extends ResourceObject = Record<string, unknown>,
  Environment extends EnvironmentObject = Record<string, unknown>,
> extends AbilityStrategy<Resource, Environment> {
  private decisive: AbilityPolicy<Resource, Environment> | null = null;

  evaluate(): AbilityPolicyEffectType {
    const now = new Date();
    const hour = now.getHours();
    const isBusinessHours = hour >= 9 && hour <= 18;

    // Always deny outside business hours
    if (!isBusinessHours) {
      this.decisive = null;
      return AbilityPolicyEffect.deny;
    }

    // During business hours, use standard deny-overrides logic
    if (this.hasDeny()) {
      this.decisive = this.firstDenied();
      return AbilityPolicyEffect.deny;
    }

    this.decisive = this.firstPermitted();
    return AbilityPolicyEffect.permit;
  }

  decisivePolicy(): AbilityPolicy<Resource, Environment> | null {
    return this.decisive;
  }
}
```

---

## Example scenarios

### Scenario 1: corporate security

```typescript
// Use DenyOverridesStrategy as the safest option
const securityResolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Any denial overrides all permissions
securityResolver.enforce('system.delete', {
  system: { id: 'prod-1' }
}, {
  user: { role: 'developer' }
});
```

### Scenario 2: public API

```typescript
// Use PermitOverridesStrategy for maximum availability
const publicResolver = new AbilityResolver(policies, PermitOverridesStrategy);

// Access is allowed when at least one policy allows it
publicResolver.enforce('api.read', {
  api: { endpoint: '/public' }
}, {
  apiKey: 'valid-key'
});
```

### Scenario 3: priority hierarchy

```typescript
// Use PriorityStrategy for explicit control
const hierarchicalResolver = new AbilityResolver(policies, PriorityStrategy);

// Specify priorities in the DSL:
// @priority 100 - super administrator
// @priority 50  - moderator
// @priority 10  - regular user
```

### Scenario 4: strict consensus

```typescript
// All policies must agree
const consensusResolver = new AbilityResolver(policies, AllMustPermitStrategy);

// Access is allowed only when all matched policies permit it
consensusResolver.enforce('document.approve', {
  document: { id: '123', status: 'review' }
}, {
  reviewers: ['alice', 'bob']
});
```

---

## Frequently asked questions

**Which strategy is used by default?**  
`DenyOverridesStrategy` is used by default because it is the safest option.

**Can several strategies be used at the same time?**  
No. A resolver uses one strategy, but you can create several resolvers with different strategies.

**Why did `decisivePolicy()` return nothing even though access was denied?**  
This method returns the policy that directly affected the final decision. If no policy matched, the strategy made the denial decision, so `decisivePolicy` is empty.

**How do I choose the right strategy?**

- For security: `DenyOverridesStrategy` or `AllMustPermitStrategy`
- For availability: `PermitOverridesStrategy` or `AnyPermitStrategy`
- For control: `PriorityStrategy` or `FirstMatchStrategy`
- For strictness: `OnlyOneApplicableStrategy`
