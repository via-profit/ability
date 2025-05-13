/*
 eslint-disable @typescript-eslint/ban-ts-comment
*/


/* @ts-ignore */
import path from 'node:path';
import { Configuration } from 'webpack';

/* @ts-ignore */
import NodemonPlugin from 'nodemon-webpack-plugin';

const playgroundConfig: Configuration = {
  target: 'node',
  mode: 'development',
  devtool: 'source-map',
  optimization: {
    minimize: false,
  },
  entry: {
    playground: path.resolve(__dirname, '../src/playground.ts'),
  },
  output: {
    libraryTarget: 'commonjs2',
    path: path.join(__dirname, '../build/'),
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
    new NodemonPlugin({
      watch: ['./build'],
      exec: 'node --inspect=9226 ./build/playground.js',
    }),
  ]
}

export default playgroundConfig;