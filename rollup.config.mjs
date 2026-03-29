import path from 'node:path';
import fs from 'node:fs';
import alias from '@rollup/plugin-alias';
import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import copy from 'rollup-plugin-copy';
import { fileURLToPath } from 'node:url';

const isDev = process.env.NODE_ENV === 'development';


const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);


const pkg = JSON.parse(
  fs.readFileSync(path.resolve('./package.json'), 'utf8')
);


export default {
  input: 'src/index.ts',
  output: {
    file: isDev ? 'build/index.js' : 'dist/index.js',
    format: 'cjs',
    sourcemap: isDev,
  },
  external: [
    ...Object.keys(pkg.dependencies || {}),
    ...Object.keys(pkg.peerDependencies || {}),
  ],
  plugins: [
    alias({
      entries: [
        { find: '~', replacement: path.resolve(__dirname, 'src') },
      ],
    }),

    resolve({
      extensions: ['.ts', '.js'],
    }),

    commonjs(),

    typescript({
      tsconfig: './tsconfig.json',
      sourceMap: isDev,
    }),

    copy({
      targets: [
        { src: 'docs/en/README.md', dest: '.', rename: 'README.md' },
      ],
    }),
  ],
};
