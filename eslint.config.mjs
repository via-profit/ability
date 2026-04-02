// eslint.config.js
import globals from 'globals';
import js from '@eslint/js';
import jestPlugin from 'eslint-plugin-jest';

export default [
  // Global ignores
  {
    ignores: ['build/**', 'dist/**', 'node_modules/**', 'coverage/**', '.cache/**', '**/*.min.js'],
  },

  // Base recommended rules for all JavaScript files
  js.configs.recommended,

  // Main source code (both Node.js and browser)
  {
    files: ['**/*.js', '**/*.cjs', '**/*.mjs'],
    languageOptions: {
      globals: {
        ...globals.browser, // browser APIs (window, document, etc.)
        ...globals.node, // Node.js globals (process, __dirname, etc.)
        ...globals.es2021, // modern ES features
      },
      parserOptions: {
        ecmaVersion: 'latest',
        sourceType: 'module', // use 'script' if you need CommonJS, but 'module' works for both
      },
    },
    rules: {
      // Optional: adjust rules to your taste
      'no-console': 'off', // allow console in Node/browser
      'no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
    },
  },

  // Test files (Jest) – override globals and add Jest rules
  {
    files: ['**/__tests__/**/*.js', '**/*.test.js', '**/*.spec.js'],
    ...jestPlugin.configs['flat/recommended'],
    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.node,
        ...globals.jest, // jest globals (describe, it, expect, etc.)
      },
    },
    rules: {
      // Allow test‑specific patterns
      'jest/no-disabled-tests': 'warn',
      'jest/no-focused-tests': 'error',
      'jest/no-identical-title': 'error',
      'jest/prefer-to-have-length': 'warn',
      'jest/valid-expect': 'error',
    },
  },

  // If you have CommonJS files (e.g., scripts, configs) – override module type
  {
    files: ['**/*.cjs', '**/scripts/**/*.js'],
    languageOptions: {
      sourceType: 'commonjs',
    },
  },
];
