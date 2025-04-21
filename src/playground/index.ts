import AbilityPolicy from '~/AbilityPolicy';
import AbilityRule from '~/AbilityRule';

type Department = 'admins' | 'managers' | 'users'
type User = {
  readonly id: string;
  readonly name: string;
  readonly department: Department;
}

class Person {
  readonly id: string
  readonly name: string;
  readonly department: Department;
  public constructor(id: string, name: string, department: Department) {
    this.name = name;
    this.department = department;
    this.id = id;
  }
}

const policy = new AbilityPolicy('First test policy')
  .addRule(new AbilityRule(
    ['subject.department', '=', 'object.department'],
    'permit',
    'Same name'
  ));

const Ivan = new Person('1', 'Ivan', 'users');
const Oleg = new Person('2', 'Oleg', 'managers');
const Ann = new Person('3', 'Ann', 'users');

policy.enforce(Ivan, Ann);
