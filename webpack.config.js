const path = require('path');

module.exports = {
  entry: './src/webpack-module.js',
  output: {
    filename: 'locoMotion.js',
    path: path.resolve(__dirname, '.'),
    library: {
      name: 'locoMotion',
      type: 'umd'
    }
  },
};
