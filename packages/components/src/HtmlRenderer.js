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

        // Purescript functions are curried, so to call them from javascript we need
        // to uncurry them
        const processNode = replaceChildren ?
            (node, children, index) => {
                return h.processNodeWithReplacement(node)(children)(index);
            } :
            (node, children) => {
                return h.processNode(node)(children);
            };

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