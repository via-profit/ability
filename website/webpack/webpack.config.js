const fs = require('node:fs');
const path = require('node:path');
const webpack = require('webpack');

const isDev = process.env.NODE_ENV === 'development';
const repositoryRoot = path.resolve(__dirname, '../..');
const src = path.resolve(__dirname, '../src');
const outputDir = path.resolve(__dirname, isDev ? '../build' : '../dist');
const { version: abilityVersion } = require('../../package.json');

const shared = {
  mode: isDev ? 'development' : 'production',
  module: {
    rules: [
      {
        test: /\.[jt]sx?$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: { configFile: path.resolve(__dirname, '../babel.config.js') },
        },
      },
      { test: /\.md$/, type: 'asset/source' },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.jsx', '.js'],
    alias: {
      '~': src,
      '@via-profit/ability$': path.join(repositoryRoot, 'src/index.ts'),
      '@repository': repositoryRoot,
    },
  },
  devtool: isDev ? 'inline-source-map' : false,
  performance: { hints: false },
};

const versionPlugin = () =>
  new webpack.DefinePlugin({
    'process.env.ABILITY_VERSION': JSON.stringify(abilityVersion),
  });

const clientAssets = {
  apply(compiler) {
    compiler.hooks.thisCompilation.tap('ClientAssets', compilation => {
      compilation.hooks.processAssets.tap(
        { name: 'ClientAssets', stage: webpack.Compilation.PROCESS_ASSETS_STAGE_REPORT },
        () => {
          const files = compilation.entrypoints
            .get('app')
            .getFiles()
            .filter(file => file.endsWith('.js'));
          const favicon = path.join(src, 'assets/favicon.svg');
          compilation.fileDependencies.add(favicon);
          compilation.emitAsset(
            'favicon.svg',
            new webpack.sources.RawSource(fs.readFileSync(favicon)),
          );
          compilation.emitAsset(
            'entrypoints.json',
            new webpack.sources.RawSource(JSON.stringify(files.map(file => `/public/${file}`))),
          );
        },
      );
    });
  },
};

module.exports = [
  {
    ...shared,
    name: 'client',
    target: 'web',
    entry: { app: path.join(src, 'index.tsx') },
    output: {
      path: path.join(outputDir, 'public'),
      publicPath: '/public/',
      filename: 'js/[name]-[contenthash].js',
      chunkFilename: 'js/[name]-[contenthash].js',
      clean: true,
    },
    plugins: [versionPlugin(), clientAssets],
  },
  {
    ...shared,
    name: 'server',
    target: 'node24',
    entry: {
      server: path.join(src, 'server/index.ts'),
    },
    output: {
      path: outputDir,
      filename: 'server.cjs',
      chunkFilename: 'server/[name].cjs',
      clean: {
        keep: /^public(?:\/|$)/,
      },
    },
    // Browser-only async modules should not ship in the server build.
    optimization: {
      minimize: false,
    },
    plugins: [
      versionPlugin(),
      new webpack.IgnorePlugin({ resourceRegExp: /^(mermaid|~\/playground\/CodeEditor)$/ }),
    ],
  },
];
