export type Resources = {
    readonly ['account.read']: {
        readonly account: {
            readonly id: string;
            readonly roles: readonly string[];
        } | null;
        readonly resource: {
            readonly id: string;
        };
    };
    readonly ['access.auth']: {
        readonly token: {
            readonly type: string;
            readonly id: string;
        } | null;
    };
    readonly ['order.update']: {
        readonly account: {
            readonly roles: readonly string[];
            readonly department: string;
        } | null;
        readonly order: {
            readonly status: string;
        } | null;
    };
};
