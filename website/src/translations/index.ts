import { useIntl } from 'react-intl';
import React from 'react';

import type { Locale } from '~/providers/UiProvider';

/**
 * Texts of the site interface. Every text has all the translations,
 * so a missing translation is a type error
 */
const messages = {
  'nav.home': { ru: 'Главная', en: 'Home' },
  'nav.docs': { ru: 'Документация', en: 'Docs' },
  'nav.playground': { ru: 'Песочница', en: 'Playground' },
  'nav.changelog': { ru: 'Изменения', en: 'Changelog' },

  'header.menu': { ru: 'Открыть меню', en: 'Open menu' },
  'header.themeLight': { ru: 'Включить светлую тему', en: 'Switch to light theme' },
  'header.themeDark': { ru: 'Включить тёмную тему', en: 'Switch to dark theme' },
  'header.language': { ru: 'Switch to English', en: 'Переключить на русский' },

  'search.placeholder': { ru: 'Поиск по документации', en: 'Search docs' },
  'search.empty': { ru: 'Ничего не найдено', en: 'Nothing found' },

  'footer.license': { ru: 'Лицензия MIT', en: 'MIT License' },

  'sidebar.filter': { ru: 'Найти раздел…', en: 'Find a section…' },
  'sidebar.group.start': { ru: 'Начало', en: 'Getting started' },
  'sidebar.group.guides': { ru: 'Руководство', en: 'Guides' },
  'sidebar.group.more': { ru: 'Ещё', en: 'More' },
  'sidebar.empty': { ru: 'Ничего не найдено', en: 'Nothing found' },
  'sidebar.group.site': { ru: 'Сайт', en: 'Site' },

  'doc.getting-started': { ru: 'Введение', en: 'Introduction' },
  'doc.dsl': { ru: 'Язык политик (DSL)', en: 'Policy language (DSL)' },
  'doc.resolver': { ru: 'Резолвер', en: 'Resolver' },
  'doc.strategies': { ru: 'Стратегии', en: 'Strategies' },
  'doc.types-generator': { ru: 'Генерация типов', en: 'Types generation' },
  'doc.server-and-client': { ru: 'Сервер и клиент', en: 'Server and client' },
  'doc.changelog': { ru: 'Список изменений', en: 'Changelog' },
  'doc.edit': { ru: 'Редактировать на GitHub', en: 'Edit on GitHub' },
  'doc.prev': { ru: 'Назад', en: 'Previous' },
  'doc.next': { ru: 'Далее', en: 'Next' },
  'doc.changelogLanguage': {
    ru: 'Список изменений ведётся на русском языке.',
    en: 'The changelog is maintained in Russian.',
  },
  'doc.loadError': {
    ru: 'Не удалось загрузить страницу. Обновите страницу и попробуйте снова.',
    en: 'Failed to load the page. Reload the page and try again.',
  },

  'breadcrumbs.docs': { ru: 'Документация', en: 'Docs' },
  'toc.title': { ru: 'На этой странице', en: 'On this page' },

  'code.copy': { ru: 'Копировать', en: 'Copy' },
  'code.copied': { ru: 'Скопировано', en: 'Copied' },
  'code.openInPlayground': { ru: 'Открыть в песочнице', en: 'Open in playground' },

  'callout.note': { ru: 'Примечание', en: 'Note' },
  'callout.tip': { ru: 'Совет', en: 'Tip' },
  'callout.important': { ru: 'Важно', en: 'Important' },
  'callout.warning': { ru: 'Внимание', en: 'Warning' },
  'callout.caution': { ru: 'Осторожно', en: 'Caution' },

  'home.pill': { ru: 'ABAC · DSL · TypeScript · 0 зависимостей', en: 'ABAC · DSL · TypeScript · 0 dependencies' },
  'home.title': { ru: 'Правила доступа,', en: 'Access rules' },
  'home.titleAccent': { ru: 'которые можно прочитать', en: 'you can actually read' },
  'home.lead': {
    ru: 'Лёгкий ABAC-движок: опишите политики на простом DSL, получите TypeScript-типы и проверяйте доступ одинаково на сервере и в браузере.',
    en: 'A lightweight ABAC engine: describe policies in a simple DSL, get TypeScript types and check access the same way on the server and in the browser.',
  },
  'home.docs': { ru: 'Документация', en: 'Read the docs' },
  'home.playground': { ru: 'Открыть песочницу', en: 'Try the playground' },
  'home.example': { ru: 'Как это выглядит', en: 'How it looks' },
  'home.exampleDsl': { ru: 'Политики', en: 'Policies' },
  'home.exampleCode': { ru: 'Проверка', en: 'Check' },
  'home.features': { ru: 'Возможности', en: 'Features' },
  'home.feature.dsl.title': { ru: 'Простой DSL', en: 'Simple DSL' },
  'home.feature.dsl.text': {
    ru: 'Политики читаются как текст: группы all/any, исключения except, алиасы и аннотации.',
    en: 'Policies read like text: all/any groups, except blocks, aliases and annotations.',
  },
  'home.feature.types.title': { ru: 'Типы из политик', en: 'Types from policies' },
  'home.feature.types.text': {
    ru: 'Генератор строит TypeScript-типы ресурсов и окружения — опечатки видны ещё в IDE.',
    en: 'The generator builds TypeScript types of resources and environment — typos show up in the IDE.',
  },
  'home.feature.strategies.title': { ru: '8 стратегий', en: '8 strategies' },
  'home.feature.strategies.text': {
    ru: 'Deny overrides, permit overrides, приоритеты, первое совпадение и другие — или своя.',
    en: 'Deny overrides, permit overrides, priorities, first match and more — or your own.',
  },
  'home.feature.explain.title': { ru: 'Explain', en: 'Explain' },
  'home.feature.explain.text': {
    ru: 'Дерево проверки показывает, какое правило сработало и почему доступ запрещён.',
    en: 'The check tree shows which rule matched and why access was denied.',
  },
  'home.feature.isomorphic.title': { ru: 'Сервер и браузер', en: 'Server and browser' },
  'home.feature.isomorphic.text': {
    ru: 'Политики описываются на сервере и передаются клиенту в JSON — один источник правды.',
    en: 'Policies are defined on the server and sent to the client as JSON — a single source of truth.',
  },
  'home.feature.fast.title': { ru: 'Быстро и легко', en: 'Fast and light' },
  'home.feature.fast.text': {
    ru: 'Ноль зависимостей и около 1.5 µs на проверку — можно проверять права на каждом рендере.',
    en: 'Zero dependencies and about 1.5 µs per check — fine to check on every render.',
  },
  'home.install': { ru: 'Установка', en: 'Installation' },

  'playground.title': { ru: 'Песочница', en: 'Playground' },
  'playground.policies': { ru: 'Политики', en: 'Policies' },
  'playground.context': { ru: 'Контекст', en: 'Context' },
  'playground.output': { ru: 'Результат', en: 'Output' },
  'playground.properties': { ru: 'Параметры', en: 'Properties' },
  'playground.example': { ru: 'Пример', en: 'Example' },
  'playground.customExample': { ru: 'Свой код', en: 'Custom code' },
  'playground.strategy': { ru: 'Стратегия', en: 'Strategy' },
  'playground.permission': { ru: 'Ключ разрешения', en: 'Permission key' },
  'playground.share': { ru: 'Поделиться', en: 'Share' },
  'playground.shared': { ru: 'Ссылка скопирована', en: 'Link copied' },
  'playground.reset': { ru: 'Сбросить', en: 'Reset' },
  'playground.tab.explain': { ru: 'Explain', en: 'Explain' },
  'playground.tab.types': { ru: 'Типы', en: 'Types' },
  'playground.tab.json': { ru: 'JSON', en: 'JSON' },
  'playground.permit': { ru: 'Доступ разрешён', en: 'Access permitted' },
  'playground.deny': { ru: 'Доступ запрещён', en: 'Access denied' },
  'playground.decisive': { ru: 'Решающая политика', en: 'Decisive policy' },
  'playground.noDecisive': {
    ru: 'нет — ни одна политика не совпала, стратегия вернула deny по умолчанию',
    en: 'none — no policy matched, the strategy returned deny by default',
  },
  'playground.duration': { ru: 'Проверка: {time}', en: 'Check: {time}' },
  'playground.policiesCount': {
    ru: '{count, plural, one {# политика} few {# политики} other {# политик}}',
    en: '{count, plural, one {# policy} other {# policies}}',
  },
  'playground.dslError': { ru: 'Ошибка в DSL', en: 'DSL error' },
  'playground.jsonError': { ru: 'Ошибка в JSON контекста', en: 'Context JSON error' },
  'playground.contextHint': {
    ru: '«resource» — проверяемые данные, «environment» — окружение (env.*)',
    en: '"resource" is the checked data, "environment" is the environment (env.*)',
  },
  'playground.noPolicies': {
    ru: 'Для этого ключа нет ни одной политики',
    en: 'There are no policies for this key',
  },
  'playground.loading': { ru: 'Загрузка редактора…', en: 'Loading the editor…' },

  'notFound.title': { ru: 'Страница не найдена', en: 'Page not found' },
  'notFound.text': {
    ru: 'Такой страницы нет. Возможно, она переехала.',
    en: 'This page does not exist. Maybe it has moved.',
  },
  'notFound.back': { ru: 'На главную', en: 'Go home' },
} as const satisfies Record<string, Record<Locale, string>>;

export type MessageId = keyof typeof messages;

export const intlLocales: Record<Locale, string> = {
  ru: 'ru-RU',
  en: 'en-US',
};

export const getMessages = (locale: Locale): Record<MessageId, string> =>
  Object.fromEntries(
    Object.entries(messages).map(([id, translations]) => [id, translations[locale]]),
  ) as Record<MessageId, string>;

/**
 * Returns the function, which translates the interface text by its ID
 */
export const useT = () => {
  const intl = useIntl();

  return React.useCallback(
    (id: MessageId, values?: Record<string, string | number>) =>
      intl.formatMessage({ id }, values),
    [intl],
  );
};
