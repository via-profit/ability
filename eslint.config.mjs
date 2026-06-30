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
    files: ['**/*.js', '**/*.mjs'], // Убрали .cjs отсюда
    languageOptions: {
      sourceType: 'module', // Изменили с 'script' на 'module'
      globals: {
        ...globals.browser,
        ...globals.node,
        ...globals.es2021,
      },
      parserOptions: {
        ecmaVersion: 'latest',
      },
    },
    rules: {
      'no-console': 'off',
      'no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
    },
  },

  // Test files (Jest)
  {
    files: ['**/__tests__/**/*.js', '**/*.test.js', '**/*.spec.js'],
    ...jestPlugin.configs['flat/recommended'],
    languageOptions: {
      sourceType: 'module', // Добавлено явно
      globals: {
        ...globals.browser,
        ...globals.node,
        ...globals.jest,
      },
    },
    rules: {
      'jest/no-disabled-tests': 'warn',
      'jest/no-focused-tests': 'error',
      'jest/no-identical-title': 'error',
      'jest/prefer-to-have-length': 'warn',
      'jest/valid-expect': 'error',
    },
  },

  // CommonJS files (только .cjs)
  {
    files: ['**/*.cjs'],
    languageOptions: {
      sourceType: 'commonjs',
      globals: {
        ...globals.node,
        ...globals.es2021,
      },
    },
    rules: {
      'no-console': 'off',
      'no-unused-vars': ['warn', { argsIgnorePattern: '^_' }],
    },
  },

  // Rollup config файл (если он с расширением .js и использует import/export)
  {
    files: ['rollup.config.js', '*.config.js', '*.config.mjs'],
    languageOptions: {
      sourceType: 'module',
      globals: {
        ...globals.node,
        ...globals.es2021,
      },
    },
  },
];
