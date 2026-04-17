import path from 'node:path';
import fs from 'node:fs';
import alias from '@rollup/plugin-alias';
import dts from 'rollup-plugin-dts';
import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import copy from 'rollup-plugin-copy';
import { fileURLToPath } from 'node:url';

const isDev = process.env.NODE_ENV === 'development';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const pkg = JSON.parse(fs.readFileSync(path.resolve('./package.json'), 'utf8'));

const aliasPlugin = alias({
  entries: [{ find: '~', replacement: path.resolve(__dirname, 'src') }],
});

const external = [
  ...Object.keys(pkg.dependencies || {}),
  ...Object.keys(pkg.peerDependencies || {}),
];

// ESM bundle
const esmConfig = {
  input: 'src/index.ts',
  output: {
    file: isDev ? 'build/index.js' : 'dist/index.js',
    format: 'esm',
    sourcemap: isDev,
  },
  external,
  plugins: [
    aliasPlugin,
    resolve({ extensions: ['.ts', '.js'] }),
    commonjs(),
    typescript({
      tsconfig: './tsconfig.json',
      sourceMap: isDev,
      declaration: false,
    }),
    copy({
      targets: [{ src: 'docs/en/README.md', dest: '.', rename: 'README.md' }],
    }),
  ],
};

// CJS bundle
const cjsConfig = {
  input: 'src/index.ts',
  output: {
    file: isDev ? 'build/index.cjs' : 'dist/index.cjs',
    format: 'cjs',
    sourcemap: isDev,
  },
  external,
  plugins: [
    aliasPlugin,
    resolve({ extensions: ['.ts', '.js'] }),
    commonjs(),
    typescript({
      tsconfig: './tsconfig.json',
      sourceMap: isDev,
      declaration: false,
    }),
  ],
};

// DTS bundle
const dtsConfig = {
  input: 'src/index.ts',
  output: {
    file: isDev ? 'build/index.d.ts' : 'dist/index.d.ts',
    format: 'es',
  },
  plugins: [aliasPlugin, dts()],
  external,
};

export default [esmConfig, cjsConfig, dtsConfig];
