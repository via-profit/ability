import http from 'node:http';
import { AbilityPolicyConfig } from './core/AbilityPolicy';
import { AbilityDSLParser } from './parsers/dsl/AbilityDSLParser';
import AbilityResolver from './core/AbilityResolver';
import { AbilityDSLSyntaxError } from './parsers/dsl/AbilityDSLSyntaxError';

const server = http.createServer();

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

  const result = await resolver.resolve('order.status', {
    order: {
      amount: 200
    }
  });

  console.log(result.isDenied()); // true
  res.end();
});

server.listen(8081, 'localhost', () => {
  console.debug('server started at http://localhost:8081');
});
