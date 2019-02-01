var React = require('react');
var ReactRouter = require('react-router-dom');
var Route = ReactRouter.Route;
var Link = ReactRouter.Link;
var Switch = ReactRouter.Switch;

exports.link_ = function(args) {
  return Link;
};

exports.switch_ = function() {
  return Switch;
};

exports.route_ = function() {
  return Route;
};
