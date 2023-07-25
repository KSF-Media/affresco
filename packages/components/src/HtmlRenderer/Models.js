"use strict";

// html-to-react uses htmlparser to parse input and each
// node in the output tree follow htmlparser's specific format:
// https://www.npmjs.com/package/htmlparser

// Getters
export function getRawImpl(node) {
    if (node && node.raw)
      return node.raw;

    return null;
}

export function getDataImpl(node) {
    if (node && node.data)
      return node.data;

    return null;
}

export function getTypeImpl(node) {
    if (node && node.type)
      return node.type;

    return null;
}

export function getNameImpl(node) {
    if (node && node.name)
      return node.name;

    return null;
}

export function getStringAttribImpl(name, node) {
    if (node && node.attribs)
      return node.attribs[name];

    return null;
}

export function getChildrenImpl(node) {
    if (node && node.children)
      return node.children;

    return null;
}

export function setStringAttribImpl(name, value, node) {
    if (node && node.attribs)
	node.attribs[name] = value;
    return node;
}

export function removeAttribImpl(name, node) {
    if (node && node.attribs)
	delete node.attribs[name];
    return node;
}

export function removeChildImpl(name, parent) {
    if (parent && parent.children) {
        delete parent.children[name];
    }
    return parent;
}
