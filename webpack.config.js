/* jshint node: true */
/* jshint -W097 */

'use strict';

const path = require('path');
// const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;
// const CompressionPlugin = require('compression-webpack-plugin');
const webpack = require('webpack');

module.exports = {
  mode: 'development',

  devtool: 'original-source',
  entry: './src/app.js',

  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js',
    publicPath: '/dist/'
  },
  plugins: [
    new webpack.ProvidePlugin({
      'Observable': 'zen-observable'
    }),
  ]
};
