import cheerio from "cheerio";

export function parseTemplate(HTML_TEMPLATE) {
  return cheerio.load(HTML_TEMPLATE);
}

export function renderTemplateHtml($template) {
  return $template.html();
}

export function cloneTemplate($template) {
  return $template.root().clone();
}

// Writes mosaico html inside #app
export function appendMosaicoImpl(a, $template) {
  $template.find("#app").append(a);
  return $template;
};

// Writes given element under the `head` element
export function appendHeadImpl(element, $template) {
  $template.find("head").append(element);
  return $template;
};

// Appends content to body
export function appendVarsImpl(script, $template) {
  $template.find("#app-vars").append(script);
  return $template;
}

// Server Port
export const serverPort = process.env.PORT;

// Tests are prone to failing with ads
export const globalDisableAds = !!process.env.DISABLE_ADS;
