import http from 'node:http';
import { AbilityPolicy, AbilityPolicyConfig } from './core/AbilityPolicy';
import AbilityParser from './core/AbilityParser';
import AbilityResolver from './core/AbilityResolver';

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
      action: 'order.status',
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

  const policies: AbilityPolicy<MyResources>[] = AbilityPolicy.parseAll(config);

  const result = await new AbilityResolver(policies).resolve('order.status', {
    order: {
      amount: 1000,
    },
    user: {
      roles: ['admin'],
    },
  });

  res.statusCode = 200;
  res.setHeader('content-type', 'text/plain');

  const typeDefs = AbilityParser.generateTypeDefs(policies);

  res.write(
    `${result.explain().toString()}
    
    Current ation - ${result.isAllowed() ? '✔ is allowed' : result.isDenied() ? '🗙 is denied' : 'unknown'}
    `
  );
  // res.write(typeDefs);
  res.end();
});

server.listen(8081, 'localhost', () => {
  console.debug('server started at http://localhost:8081');
});
