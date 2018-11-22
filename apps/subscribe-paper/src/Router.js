var React = require('react');
var ReactRouter = require('react-router-dom');
var Route = ReactRouter.Route;
var Link = ReactRouter.Link;
var Switch = ReactRouter.Switch;

exports.link_ = function(args) {
  return React.createElement(Link, args, null);
};

exports.route_ = function(routes) {
  var routeComponents = routes.map(function(route) {
    return React.createElement(Route, {path: route.path, component: route.component}, null);
  });
  return React.createElement(Switch, {children: routeComponents}, null);
};
