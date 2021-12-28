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

exports.getAttribsImpl = (node) => {
    if (node && node.attribs)
      return node.attribs;

    return null;
}

exports.getChildrenImpl = (node) => {
    if (node && node.children)
      return node.children;

    return null;
}

// Setters
exports.setRawImpl = (node, raw) => {
    if (node)
      node.raw = raw;
}

exports.setDataImpl = (node, data) => {
    if (node)
      node.data = data;
}

exports.setTypeImpl = (node, type) => {
    if (node)
      node.type = type;
}

exports.setNameImpl = (node, name) => {
    if (node)
      node.name = name;
}

exports.setAttribsImpl = (node, attribs) => {
    if (node)
      node.attribs = attribs;
}
