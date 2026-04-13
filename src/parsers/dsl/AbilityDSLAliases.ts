import AbilityRule from '~/core/AbilityRule';


export class AbilityDSLAliases {
  private store = new Map<string, AbilityRule>();

  get(alias: string): AbilityRule | null {
    return this.store.get(alias) || null;
  }

  set(alias: string, rule: AbilityRule): this {
    this.store.set(alias, rule);

    return this;
  }

  has(alias: string): boolean {
    return this.store.has(alias);
  }
}
