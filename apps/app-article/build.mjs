import { buildOrServe, buildSettings } from "../../esbuild.defaults.mjs";
import htmlPlugin from "@chialab/esbuild-plugin-html";
import { lessLoader } from "esbuild-plugin-less";

await buildOrServe({
  ...buildSettings,
  entryPoints: ["src/index.html"],
  plugins: [htmlPlugin(), lessLoader()],
});
