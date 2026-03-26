# Решение проблем

## Почему политика `deny` не превращается в `permit`, если её условия не выполнены

Рассмотрим политику, которая **запрещает** доступ пользователю с возрастом 16 лет:

```ts
const dsl = `
deny action.test if all:
  user.age is equals 16
`;

const policies = new AbilityDSLParser(dsl).parse();
const resolver = new AbilityResolver(policies);

const result = await resolver.resolve('action.test', {
  user: { age: 16 },
});

console.log(result.isDenied());  // true  ✔
console.log(result.isAllowed()); // false ✔
```

В этом случае всё очевидно:  
условие выполнено → политика совпала → эффект `deny` → доступ запрещён.

**Что происходит, если условия `не выполнены`?**

```ts
const result = await resolver.resolve('action.test', {
  user: { age: 12 },
});

console.log(result.isDenied());  // true  ✔
console.log(result.isAllowed()); // false ✔
```

На первый взгляд может показаться, что если условие не выполнено, то политика должна «разрешить» доступ.  
Но это **не так**.

**Модель принятия решений: `Default Deny`**

`AbilityResolver` использует классическую модель безопасности:

> **Если нет ни одной совпавшей permit‑политики → доступ запрещён.**

**Что происходит в данном примере:**

1. Политика `deny` существует, но её условие **не выполнено**  
   → политика получает статус `mismatch`.

2. Политика `deny` **не применяется**, потому что условия не совпали.

3. Политики `permit` **нет**.

4. Раз нет ни одной разрешающей политики → итоговое решение:  
   **deny (по умолчанию)**.


**Итог**

- `deny` с совпавшими условиями → **deny**
- `deny` с несовпавшими условиями → **deny (default deny)**
- `permit` с совпавшими условиями → **allow**
- `permit` с несовпавшими условиями → **deny (default deny)**

**Заключение**

**Доступ разрешается только при наличии явного permit.**
