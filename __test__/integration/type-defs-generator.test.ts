import { ability, AbilityResolver, AbilityTypeGenerator, DenyOverridesStrategy } from '../../src';
import fs from 'node:fs';
import path from 'node:path';


describe('Type defs generation', () => {

  type Res = import('../../__test__/integration/types.gen').Resources;
  type Env = import('../../__test__/integration/types.gen').Environment;
  type Tags = import('../../__test__/integration/types.gen').PolicyTags;

  const policies = ability<Res, Env, Tags>`
    permit permission.document.create if all:
      document.ownerId equals user.id
      document.cteated equals env.createdAt
      document.status in ["published", "archived"]
      
permit permission.document.read if all:
  document.ownerId equals user.id
  document.status in ["published", "archived"]

# Пользователь должен быть аутентифицирован (токен обязателен)
@name AuthenticationRequired
@tags mutation
allow permission.mut.* if all:
  token.id not equals 'NOT_ASSIGNED'
  token.type is equals 'access'


@name OrderDeleteAccess
@tags mutation, order
permit permission.mut.order.delete if all:
  account.roles contains 'administrator'
  account.roles contains 'developer'
  env.hour is equals 12
  
@name ClientDeleteAccess
@tags mutation, client
permit permission.mut.client.delete if all:
  account.roles contains 'viewer'
  account.roles contains 'administrator'
  account.roles contains 'developer'

@name Cant remove unnamed clients
@tags mutation, client
deny permission.mut.client.delete if all:
  client.name equals 'Неизвестный'

  deny permission.mut.order.delete if all:
    order.createdHourLef gte 12
    env.time.hour > 16


      permit permission.equals if all:
        user.name is equals 'Oleg'
        user.age is equals 21
        user.age = 21
        user.age == 21
        user.surname equals 'Ivanov'
        user.stateOn equals true

        permit permission.notEquals if all:
      user.age is not equals 21
      user.age != 16
      user.age <> 16
      user.age not equals 16

      permit permission.gte if all:
      user.age greater than 18
      user.age > 18
      user.age gt 18

      permit permission.greaterThanOrEqual if all:
      user.age greater than or equal 18

       permit permission.less if all:
        user.age less than or equal 30
      user.vol <= 15
      user.vol lte 15

        permit permission.isnull if all:
      user.middleName is null
      user.surname = null

       permit permission.isnotnull if all:
      user.name is not null
      user.surname is not null
      user.surname != null
      user.surname <> null

        permit permission.in if all:
      user.role in ['admin', 'manager']
      user.age in [1, 4]

       permit permission.notin if all:
      user.role not in ['banned', 'blocked']


          permit permission.contains if all:
      user.tags contains 'vip'
      user.tags includes 'vip'
      user.tags has 'vip'

         permit permission.notcontains if all:
      user.tags not contains 'banned'
      user.tags not includes 'banned'
      user.tags not has 'banned'

       permit permission.istrue if all:
      user.statusOn is true
      user.statusOn = true
      user.statusOn == true

      permit permission.length if all:
      user.name length equals 4
      user.name length = 4

      permit permission.lengthGte if all:
      user.name length greater than 3
      user.name length > 3
      user.roles length greater than 2
      user.roles length > 2

      permit permission.lengthLessThan if all:
      user.name length less than 5
      user.name length < 5
      user.roles length less than 4
      user.roles length < 4


     permit permission.lessThan if all:
      user.name less than 5

      allow permission.always if all:
        always

      permit permission.never if all:
        never
        
        
  
    
    
    
`;



  test('Type defs generation', () => {
    const types = new AbilityTypeGenerator(policies).generateTypeDefs();

    fs.writeFileSync(path.resolve('./__test__/integration/types.gen.ts'), types, {
      encoding: 'utf-8',
    });

    const resolver = new AbilityResolver(policies, DenyOverridesStrategy);
    // resolver.resolve('document.read', {
    //   document: {
    //     ownerId: '',
    //     status: '',
    //   },
    //   user: {
    //     id: ''
    //   }
    // });

    // console.log(types);
  });

});
