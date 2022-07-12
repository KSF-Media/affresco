export const headless = process.env.CI ? true : false;

export const _xpathEval = (page, expression) => (() => page.waitForXPath(expression).then(() => page.$x(expression)));
export const _clickElement = element => () => element.click();

export function _goto_options(url, page, options) {
  return function() {
    return page.goto(url, options);
  };
}
