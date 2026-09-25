const isDev = process.env.NODE_ENV === 'development';

module.exports = {
  presets: [
    ['@babel/preset-env', { targets: 'defaults' }],
    ['@babel/preset-react', { runtime: 'automatic', importSource: '@emotion/react' }],
    '@babel/preset-typescript',
  ],
  plugins: ['@emotion/babel-plugin', isDev && 'react-refresh/babel'].filter(Boolean),
};
