// See https://www.npmjs.com/package/html-to-react

import React from "react";
import HtmlToReact from "html-to-react";
const HtmlToReactParser = HtmlToReact.Parser;

const htmlToReactParser = new HtmlToReactParser();

export function renderHtmlInputImpl(htmlInput) {
  return htmlToReactParser.parse(htmlInput);
}

export function renderHtmlInputWithHooksImpl(htmlInput, hooks) {
  // Not used but needed
  const isValidNode = function () {
    return true;
  };

  const processNodeDefinitions = new HtmlToReact.ProcessNodeDefinitions(React);

  const processHooks = hooks.filter((h) => h.shouldProcessNode);
  const preprocessHooks = hooks.filter((h) => h.shouldPreprocessNode);

  // A catch-all hook
  processHooks.push({
    shouldProcessNode: function () {
      return true;
    },
    processNode: processNodeDefinitions.processDefaultNode,
  });

  return htmlToReactParser.parseWithInstructions(htmlInput, isValidNode, processHooks, preprocessHooks);
}
