# @via-profit/ability — website

Documentation website and playground of `@via-profit/ability`.

The website is a part of the library repository, but it is not an npm workspace and is not published:
it has its own `package.json` and dependencies.

## Development

```bash
cd website
npm install
npm start          # http://localhost:8080, the port can be changed with PORT=3000
```

From the repository root: `npm run website`, `npm run website:build` and `npm run website:prod`.

## Production

```bash
npm run start:prod   # build + static server, http://localhost:8080
npm run serve        # only the server of the already built dist/
```

The server (`scripts/serve.js`) has no dependencies: it serves `dist/`, returns `index.html` for the routes
of the site, compresses text files with gzip and caches the hashed assets forever.
Settings: `PORT`, `HOST` and `PUBLIC_PATH` (the same value as for the build).

## Build

```bash
npm run build                        # dist/
PUBLIC_PATH=/ability/ npm run build  # the site is served from a sub path (for example, GitHub Pages)
npm run lint                         # type check
```

`dist/` is a static single-page application. `dist/404.html` is a copy of `index.html`, so static hosting
(GitHub Pages and similar) opens any route of the site.

## How it works

- **Library** — the site imports `@via-profit/ability` directly from `../src` (webpack alias), so changes of the
  library are visible on the site without a build.
- **Documentation** — the pages are rendered from `../docs/ru/*.md` and `../docs/en/*.md`, the changelog from
  `../CHANGELOG.md`. The same markdown is shown on GitHub, so the texts are edited in one place.
  The list of pages is in `src/utils/docs.ts`.
- **Design** — components of [`@via-profit/ui-kit`](https://github.com/via-profit/ui-kit) wrapped into the own
  components of the site (`src/components`), the theme is in `src/themes`.
- **Interface texts** — `src/translations/index.ts`, every text has all the translations.
- **Playground** — `src/playground`: CodeMirror editors, examples, evaluation of the policies.
