const path = require('path')
const merge = require('webpack-merge')
const common = require('./webpack.config.common.js')

module.exports = merge(common, {
  mode: 'development',
  devtool: 'cheap-module-eval-source-map',
  devServer: {
    contentBase: path.join(__dirname, '../dist'),
    port: 9999,
    inline: true,
    writeToDisk: true,
    clientLogLevel: 'error'
  }
})
