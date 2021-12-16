const React = require('react');
const HtmlToReact = require('html-to-react');
const HtmlToReactParser = require('html-to-react').Parser;

const htmlToReactParser = new HtmlToReactParser();

exports.renderHtmlInput = function (htmlInput) {
    return htmlToReactParser.parse(htmlInput);
}

exports.renderHtmlInputWithHooks = function (htmlInput, hooks) {
    const processNodeDefinitions = new HtmlToReact.ProcessNodeDefinitions(React);

    // Purescript functions are curried, so to call them from javascript we need
    // to uncurry them
    
}