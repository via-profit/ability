import type { Locale } from '~/providers/UiProvider';
import type { PlaygroundState } from './share';

export interface PlaygroundExample extends PlaygroundState {
  readonly id: string;
  readonly title: Record<Locale, string>;
}

const json = (value: unknown) => JSON.stringify(value, null, 2);

export const examples: readonly PlaygroundExample[] = [
  {
    id: 'basic',
    title: { ru: 'Автор и статус документа', en: 'Document author and status' },
    strategy: 'DenyOverridesStrategy',
    permission: 'order.update',
    dsl: `# Try to change the order status to "completed"
# or add "admin" to the user roles

@name "Author can update a draft order"
permit permission.order.update if all:
  @name "User is the author"
  order.authorId equals user.id

  @name "Order is not finished yet"
  order.status in ['draft', 'review']

@name "Locked orders can not be changed"
deny permission.order.update if all:
  order.locked is true
`,
    context: json({
      resource: {
        order: { authorId: 1, status: 'review', locked: true },
        user: { id: 1, roles: ['manager'] },
      },
      environment: {},
    }),
  },
  {
    id: 'except',
    title: { ru: 'Исключения (except)', en: 'Exceptions (except)' },
    strategy: 'DenyOverridesStrategy',
    permission: 'order.update',
    dsl: `@name "Only an authorized user can edit an order"
permit permission.order.update if all:
  user.token is defined
  user.token is not null

@name "Completed orders are read-only, except for administrators"
deny permission.order.update if all:
  all of:
    order.status equals 'completed'
  except any of:
    user.roles contains 'admin'
`,
    context: json({
      resource: {
        order: { status: 'completed' },
        user: { token: 'secret', roles: ['admin'] },
      },
      environment: {},
    }),
  },
  {
    id: 'priority',
    title: { ru: 'Приоритеты', en: 'Priorities' },
    strategy: 'PriorityStrategy',
    permission: 'document.delete',
    dsl: `@name "Nobody can delete documents"
@priority 1
deny permission.document.* if all:
  always

@name "Owners can delete their documents"
@priority 10
permit permission.document.delete if all:
  document.ownerId equals user.id

@name "Archived documents are protected"
@priority 100
deny permission.document.delete if all:
  document.archived is true
`,
    context: json({
      resource: {
        document: { ownerId: 7, archived: false },
        user: { id: 7 },
      },
      environment: {},
    }),
  },
  {
    id: 'aliases',
    title: { ru: 'Алиасы и окружение', en: 'Aliases and environment' },
    strategy: 'DenyOverridesStrategy',
    permission: 'report.export',
    dsl: `@name "User is an analyst"
alias isAnalyst:
  user.roles contains any ['analyst', 'admin']

@name "Reports are exported by analysts during business hours"
permit permission.report.export if all:
  isAnalyst
  env.hour greater than or equal 9
  env.hour less than 18

@name "Export only from the office network"
deny permission.report.export if all:
  env.network not equals 'office'
`,
    context: json({
      resource: {
        user: { roles: ['analyst'] },
      },
      environment: { hour: 14, network: 'office' },
    }),
  },
  {
    id: 'operators',
    title: { ru: 'Операторы строк и списков', en: 'String and list operators' },
    strategy: 'DenyOverridesStrategy',
    permission: 'project.publish',
    dsl: `@name "Project can be published by its team"
permit permission.project.publish if all:
  user.email ends with '@example.com'
  user.permissions contains all ['write', 'publish']
  project.tags is not empty
  project.slug starts with 'public-'
  project.version equals '1.2.3'
  project.rating >= 4.5
`,
    context: json({
      resource: {
        user: { email: 'jane@example.com', permissions: ['read', 'write', 'publish'] },
        project: { tags: ['docs'], slug: 'public-ability', version: '1.2.3', rating: 4.8 },
      },
      environment: {},
    }),
  },
  {
    id: 'wildcards',
    title: { ru: 'Шаблоны ключей (*)', en: 'Key wildcards (*)' },
    strategy: 'DenyOverridesStrategy',
    permission: 'invoice.create',
    dsl: `@name "Administrators can do everything"
permit permission.* if all:
  user.roles contains 'admin'

@name "Managers can create anything"
permit permission.*.create if all:
  user.roles contains 'manager'

@name "Invoices are not created on weekends"
deny permission.invoice.* if any:
  env.dayOfWeek in [0, 6]
`,
    context: json({
      resource: {
        user: { roles: ['manager'] },
      },
      environment: { dayOfWeek: 3 },
    }),
  },
];

export const defaultExample = examples[0];
