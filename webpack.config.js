const path = require('path');

module.exports = {
  mode: 'production',
  entry: './src/webpack-module.js',
  output: {
    filename: 'locoMotion.js',
    path: path.resolve(__dirname, '.'),
    library: {
      name: 'LocoMotion',
      type: 'umd'
    }
  },
};
