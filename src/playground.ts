import http from 'node:http';
import { AbilityPolicy, AbilityPolicyConfig } from './AbilityPolicy';
import AbilityParser from './AbilityParser';

const server = http.createServer();

server.on('request', (_req, res) => {
  type Resources = {
    ['order.status']: {
      // <-- название экшена
      readonly account: {
        // <-- данные ресурса
        readonly roles: readonly string[];
      };
    };
    ['order.create']: {
      readonly user: {
        readonly department: string;
      };
    };
  };

  const config: AbilityPolicyConfig[] = [
    {
      id: 'policy-1',
      name: 'Example policy',
      action: 'order.status',
      effect: 'deny',
      compareMethod: 'and',
      ruleSet: [
        {
          id: 'rule-set-1',
          name: 'Example rule set',
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

  const policies: AbilityPolicy<Resources>[] = config.map(cfg => AbilityPolicy.parse(cfg));

  res.statusCode = 200;
  res.setHeader('content-type', 'text/plain');

  const typeDefs = AbilityParser.generateTypeDefs(policies);

  res.write(typeDefs);
  res.end();
});

server.listen(8081, 'localhost', () => {
  console.debug('server started at http://localhost:8081');
});
