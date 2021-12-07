const HtmlToReactParser = require('html-to-react').Parser;

const htmlToReactParser = new HtmlToReactParser();

exports.renderHtmlInput = function (htmlInput) {
    return htmlToReactParser.parse(htmlInput);
}