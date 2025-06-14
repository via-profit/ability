// eslint-disable-next-line @typescript-eslint/ban-ts-comment
/* @ts-ignore */
import path from 'node:path';
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
/* @ts-ignore */
import { Configuration, DefinePlugin } from 'webpack';

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
import packageInfo from '../package.json';

const isDev = process.env.NODE_ENV === 'development';
const webpackBaseConfig: Configuration = {
  target: 'node',
  mode: isDev ? 'development' : 'production',
  devtool: isDev ? 'source-map' : false,
  optimization: {
    minimize: false,
  },
  entry: {
    index: path.resolve(__dirname, '../src/index.ts'),
  },
  output: {
    libraryTarget: 'commonjs2',
    path: isDev ? path.join(__dirname, '../build/') : path.join(__dirname, '../dist/'),
    filename: '[name].js',
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'ts-loader',
      },
    ],
  },
  resolve: {
    extensions: ['.ts', '.js', '.json'],
    alias: {
      '~': path.resolve(__dirname, '..', 'src'),
    },
  },
  externals: [],
  node: {
    __dirname: true,
  },
  plugins: [
    /**
     * Development and production plugins
     */
    new DefinePlugin({
      'process.env.WEBPACK_INJECT_APP_VERSION': JSON.stringify(packageInfo.version),
    }),
  ],
};

export default webpackBaseConfig;
