const fs = require('node:fs');
const path = require('node:path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const ReactRefreshWebpackPlugin = require('@pmmmwh/react-refresh-webpack-plugin');

const isDev = process.env.NODE_ENV === 'development';

/**
 * Root of the ability repository. The website renders the documentation (`docs/`)
 * and runs the library from its sources (`src/`), so no build of the library is needed
 */
const repositoryRoot = path.resolve(__dirname, '../..');

/**
 * Only the version is injected: importing the whole package.json
 * would put all its fields into the bundle
 */
const { version: abilityVersion } = JSON.parse(
  fs.readFileSync(path.join(repositoryRoot, 'package.json'), { encoding: 'utf8' }),
);

/**
 * Base path of the site. For example, `/ability/` for GitHub Pages of the project
 */
const publicPath = process.env.PUBLIC_PATH || '/';

const htmlOptions = {
  template: path.resolve(__dirname, '../src/assets/index.html'),
  favicon: path.resolve(__dirname, '../src/assets/favicon.svg'),
  minify: !isDev,
};

module.exports = {
  target: 'web',
  mode: isDev ? 'development' : 'production',
  entry: {
    index: path.resolve(__dirname, '../src/index.tsx'),
  },
  output: {
    publicPath,
    path: path.resolve(__dirname, isDev ? '../build' : '../dist'),
    filename: 'public/js/[name]-[contenthash].js',
    chunkFilename: 'public/js/[name]-[chunkhash].js',
    assetModuleFilename: 'public/assets/[name]-[contenthash][ext]',
    clean: true,
  },
  module: {
    rules: [
      {
        test: /\.[jt]sx?$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            configFile: path.resolve(__dirname, '../babel.config.js'),
          },
        },
      },
      {
        // Documentation and changelog are imported as strings
        test: /\.md$/,
        type: 'asset/source',
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.jsx', '.js'],
    alias: {
      '~': path.resolve(__dirname, '../src'),
      '@via-profit/ability$': path.join(repositoryRoot, 'src/index.ts'),
      '@repository': repositoryRoot,
    },
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.ABILITY_VERSION': JSON.stringify(abilityVersion),
      'process.env.PUBLIC_PATH': JSON.stringify(publicPath),
    }),
    new HtmlWebpackPlugin(htmlOptions),

    /**
     * Static hosting (for example, GitHub Pages) returns 404.html for unknown paths.
     * The same page lets the client router handle any URL of the SPA
     */
    new HtmlWebpackPlugin({ ...htmlOptions, filename: '404.html' }),
    isDev && new ReactRefreshWebpackPlugin({ overlay: false }),
  ].filter(Boolean),
  optimization: {
    splitChunks: {
      chunks: 'all',
    },
  },
  devtool: isDev ? 'eval-source-map' : false,
  devServer: {
    historyApiFallback: true,
    hot: true,
    port: Number(process.env.PORT || 8080),
  },
  performance: {
    hints: false,
  },
};
