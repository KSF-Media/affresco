// See https://www.npmjs.com/package/html-to-react

const React = require('react');
const HtmlToReact = require('html-to-react');
const HtmlToReactParser = require('html-to-react').Parser;

const htmlToReactParser = new HtmlToReactParser();

exports.renderHtmlInputImpl = function (htmlInput) {
    return htmlToReactParser.parse(htmlInput);
}

exports.renderHtmlInputWithHooksImpl = function (htmlInput, hooks) {

    // Not used but needed
    const isValidNode = function () {
        return true;
      };

    const processNodeDefinitions = new HtmlToReact.ProcessNodeDefinitions(React);

    const uncurriedHooks = hooks.map(h => {

        const replaceChildren = h.replaceChildren;

        const processNode = replaceChildren ?
            h.processNodeWithReplacement :
            h.processNode;

        return {
          replaceChildren,
          shouldProcessNode: h.shouldProcessNode,
          processNode,
        }
    });

    // A catch-all hook
    uncurriedHooks.push({
        shouldProcessNode: function (node) {
            return true;
        },
        processNode: processNodeDefinitions.processDefaultNode,
    });

    return htmlToReactParser.parseWithInstructions(
        htmlInput,
        isValidNode,
        uncurriedHooks
    );
}