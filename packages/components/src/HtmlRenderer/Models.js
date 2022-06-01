"use strict";

// html-to-react uses htmlparser to parse input and each
// node in the output tree follow htmlparser's specific format:
// https://www.npmjs.com/package/htmlparser

// Getters
exports.getRawImpl = (node) => {
    if (node && node.raw)
      return node.raw;

    return null;
}

exports.getDataImpl = (node) => {
    if (node && node.data)
      return node.data;

    return null;
}

exports.getTypeImpl = (node) => {
    if (node && node.type)
      return node.type;

    return null;
}

exports.getNameImpl = (node) => {
    if (node && node.name)
      return node.name;

    return null;
}

exports.getStringAttribImpl = (name, node) => {
    if (node && node.attribs)
      return node.attribs[name];

    return null;
}

exports.getChildrenImpl = (node) => {
    if (node && node.children)
      return node.children;

    return null;
}

exports.setStringAttribImpl = (name, value, node) => {
    if (node && node.attribs)
	node.attribs[name] = value;
    return node;
}

exports.removeAttribImpl = (name, node) => {
    if (node && node.attribs)
	delete node.attribs[name];
    return node;
}
