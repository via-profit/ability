import AbilityPolicy from './AbilityPolicy';

class AbilityPolicyResult {
  public policies: readonly AbilityPolicy[];

  public constructor(policies: readonly AbilityPolicy[]) {
    this.policies = policies;
  }

  // public get isDeny
}

export default AbilityPolicyResult;