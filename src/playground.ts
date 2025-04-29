import http from 'node:http';
import AbilityPolicy, { AbilityPolicyConfig } from './AbilityPolicy';
import AbilityResolver from '~/AbilityResolver';

import policies from './policies.json';

const server = http.createServer();

server.on('request', (_req, res) => {
  new AbilityResolver<Resources>(
    policies.map(p => {
      return AbilityPolicy.parse(p as AbilityPolicyConfig);
    }),
  ).enforce('account.read', {
    account: {
      id: '1',
      roles: ['managers'],
    },
    resource: {
      id: '1',
    },
  });

  res.statusCode = 200;
  res.setHeader('content-type', 'application/json');

  res.write(
    JSON.stringify({
      status: 'Done',
    }),
  );
  res.end();
});

server.listen(8080, 'localhost', () => {
  console.debug('server started at http://localhost:8080');
});

export type Resources = {
  readonly ['account.read']: {
    readonly account: {
      readonly id: string;
      readonly roles: readonly string[];
    };
    readonly resource: {
      readonly id: string;
    };
  };
  readonly ['access.auth']: {
    readonly token: {
      readonly type: string;
      readonly id: string;
    };
  };
  readonly ['order.update']: {
    readonly account: {
      readonly roles: readonly string[];
      readonly department: string;
    };
    readonly order: {
      readonly status: string;
    };
  };
};
