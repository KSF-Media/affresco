import fs from "fs";
import path from "path";
import cheerio from "cheerio";

// The path is relative from server bundle to client bundle, not the source
const templatePath = path.join(__dirname, "..", "client", "index.html");
const HTML_TEMPLATE = fs.readFileSync(templatePath).toString();

export default function generateHtml(markup) {
  // Get the server-rendering values for the <head />

  const $template = cheerio.load(HTML_TEMPLATE);

  $template("#root").html(markup);
  console.log($template.html());
  console.log(markup);
  return $template.html();
}
