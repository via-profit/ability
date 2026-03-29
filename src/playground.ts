import {createServer} from 'node:http';
import AbilityPolicy, { AbilityPolicyConfig } from './core/AbilityPolicy';
import { AbilityDSLParser } from './parsers/dsl/AbilityDSLParser';
import AbilityResolver from './core/AbilityResolver';
import { AbilityDSLSyntaxError } from './parsers/dsl/AbilityDSLSyntaxError';
import AbilityCompare from './core/AbilityCompare';
import AbilityPolicyEffect from './core/AbilityPolicyEffect';
import AbilityRuleSet from './core/AbilityRuleSet';
import AbilityRule from './core/AbilityRule';
import { AbilityJSONParser } from './parsers/json/AbilityJSONParser';


async function exampleB() {
// Определяем типы ресурсов для TypeScript
// Типы можно генерировать автоматически (об этом позже), либо описывать вручную
// В данном примере, для простоты, типы описываются вручную
type Resources = {
  ['order.action.create']: {
    user: {
      age: number;
    }
  }
  ['order.data.price']: {
    user: {
      roles: string[];
    }
  }
}

const policies = AbilityJSONParser.parse<Resources>([
  {
    id: '1',
    name: 'Создание заказа доступно только лицам старше 18 лет',
    effect: 'permit',
    permission: 'order.action.create',
    compareMethod: 'and',
    ruleSet: [
      {
        compareMethod: 'and',
        rules: [
          {
            subject: 'user.age',
            resource: 18,
            condition: '>',
          }
        ]
      }
    ],
  },
  {
    id: '2',
    name: 'Редактирование стоимости доступно только администратору',
    effect: 'permit',
    permission: 'order.data.price',
    compareMethod: 'and',
    ruleSet: [
      {
        compareMethod: 'and',
        rules: [
          {
            subject: 'user.roles',
            resource: 'administrator',
            condition: 'contains',
          }
        ]
      }
    ]
  }
]);

}

async function exampleA() {

// Определяем типы ресурсов для TypeScript
// Типы можно генерировать автоматически (об этом позже), либо описывать вручную
// В данном примере, для простоты, типы описываются вручную
type Resources = {
  ['order.action.create']: {
    user: {
      age: number;
    }
  }
  ['order.data.price']: {
    user: {
      roles: string[];
    }
  }
}

const policies = [
  // первая политика
  new AbilityPolicy<Resources>({
    id: '1',
    name: 'Создание заказа доступно только лицам старше 18 лет',
    compareMethod: AbilityCompare.and,
    effect: AbilityPolicyEffect.permit,
    permission: 'order.action.create',
  }).addRuleSet(
    AbilityRuleSet.and([
      // правило
      AbilityRule.moreOrEqual('user.age', 18),
    ]),
  ),

  // вторая политика
  new AbilityPolicy<Resources>({
    id: '2',
    name: 'Редактирование стоимости доступно только администратору',
    compareMethod: AbilityCompare.and,
    effect: AbilityPolicyEffect.permit,
    permission: 'order.data.price',
  }).addRuleSet(
    AbilityRuleSet.and([
      // правило
      AbilityRule.contains('user.roles', 'administrator'),
    ])
  ),
];

console.log(policies); // [AbilityPolicy, AbilityPolicy, ...]
}


const server = createServer();

server.on('request', async  (_req, res) => {
  type MyResources = {
    ['order.status']: {
      readonly order: {
        readonly amount: number;
      };
      readonly user: {
        readonly roles: string[];
      };
    };
  };

  const config: AbilityPolicyConfig[] = [
    {
      id: 'policy-1',
      name: 'Deny if is admin and order amount <= 1000',
      permission: 'order.status',
      effect: 'deny',
      compareMethod: 'and',
      ruleSet: [
        {
          id: 'rule-set-1',
          name: 'user roles contain amin and order amount <= 1000',
          compareMethod: 'and',
          rules: [
            {
              subject: 'user.roles',
              resource: ['admin'],
              condition: 'in',
            },
            {
              subject: 'order.amount',
              resource: 1000,
              condition: '<=',
            },
          ],
        },
      ],
    },
  ];

  // const policies: AbilityPolicy<MyResources>[] = AbilityPolicy.fromJSONAll(config);
  //
  // const result = await new AbilityResolver(policies).resolve('order.status', {
  //   order: {
  //     amount: 1000,
  //   },
  //   user: {
  //     roles: ['admin'],
  //   },
  // });

  res.statusCode = 200;
  res.setHeader('content-type', 'text/plain');
  const dsl = `
#    @name can order update
         permit order.update if any:
  # @name authorized admin
  all of:
    # @name contains role admin
      user.roles contain 'admin'
    user.token is not null

  # @name if is developer
  any of:
    user.roles contains 'developer'
    user.logit is equals 'dev'
`;

 const policies = new AbilityDSLParser<MyResources>(dsl).parse();

  const resolver = new AbilityResolver(policies);

  const viewer = {
    id: '123',
  };

  // Пользователь, чьи данные пытаются прочитать
  // Он же - владелец поля `passwordHash`, поэтому
  // этот объект будет именован как `owner`
  const user = {
    id: '987',
    // passwordHash: '...'
  };

  // const result = await resolver.resolve('order.status', {
  //   order: {
  //     amount: 200
  //   }
  // });

  // console.log(result.isDenied()); // true
  res.end();
});

server.listen(8081, 'localhost', () => {
  console.debug('server started at http://localhost:8081');
});
