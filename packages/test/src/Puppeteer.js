exports.headless = process.env.CI ? true : false;

exports._xpathEval = () => (page, expression) => (() => page.waitForXPath(expression).then(() => page.$x(expression)));
exports._clickElement = element => () => element.click();
