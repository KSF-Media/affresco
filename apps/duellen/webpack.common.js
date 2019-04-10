 const path = require('path');
 const CleanWebpackPlugin = require('clean-webpack-plugin');
 const HtmlWebpackPlugin = require('html-webpack-plugin');
 const HtmlWebpackPluginConfig = new HtmlWebpackPlugin({
   template: './client/index.html',
   filename: 'index.html',
   inject: 'body',
   title: 'production'
 });

 module.exports = {
   entry: {
     app: './client/index.js'
   },
   plugins: [
     new CleanWebpackPlugin(['dist']),
     HtmlWebpackPluginConfig
   ],
   output: {
     filename: '[name].bundle.js',
     path: path.resolve(__dirname, 'dist')
   },
   module: {
     loaders: [
       { test: /\.js$/, loader: 'babel-loader', exclude: /node_modules/ },
       { test: /\.jsx$/, loader: 'babel-loader', exclude: /node_modules/ }
     ]
   },
  
 };
