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

    const processHooks = hooks.filter(h => h.shouldProcessNode);
    const preprocessHooks = hooks.filter(h => h.shouldPreprocessNode);

    // A catch-all hook
    processHooks.push({
        shouldProcessNode: function (node) {
            return true;
        },
        processNode: processNodeDefinitions.processDefaultNode,
    });

    return htmlToReactParser.parseWithInstructions(
        htmlInput,
        isValidNode,
	processHooks,
	preprocessHooks
    );
}
