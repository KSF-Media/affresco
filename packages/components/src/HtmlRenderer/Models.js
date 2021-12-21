"use strict";

exports.getRaw = (node) =>  { 
    if (node && node.raw)
      return node.raw;

    return null;
}

exports.getData = (node) =>  { 
    if (node && node.data)
      return node.data;

    return null;
}

exports.getType = (node) =>  { 
    if (node && node.type)
      return node.type;

    return null;
}

exports.getName = (node) =>  { 
    if (node && node.name)
      return node.name;

    return null;
}

exports.getAttribs = (node) =>  { 
    if (node && node.attribs)
      return node.attribs;

    return null;
}

exports.setRaw = (node, raw) =>  { 
    if (node)
      node.raw = raw;
}

exports.setData = (node, data) =>  { 
    if (node)
      node.data = data;
}

exports.setType = (node, type) =>  { 
    if (node)
      node.type = type;
}

exports.setName = (node, name) =>  { 
    if (node)
      node.name = name;
}

exports.setAttribs = (node, attribs) =>  { 
    if (node)
      node.attribs = attribs;
}
