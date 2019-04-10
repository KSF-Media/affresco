 const merge = require('webpack-merge');
 const common = require('./webpack.common.js');

 module.exports = merge(common, {
   devtool: 'inline-source-map',
   devServer: {
     contentBase: './dist',
     historyApiFallback: true,
    proxy: {
       '/duellen/api' : {
         target: 'http://127.0.0.1:8000'
       },
       '/images/media' : {
         target: 'http://127.0.0.1:8000'
       }
     }
   }
 });
