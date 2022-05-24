import esbuild from "esbuild";
import { buildSettings } from "../../esbuild.defaults.mjs";
import htmlPlugin from "@chialab/esbuild-plugin-html";
import { lessLoader } from "esbuild-plugin-less";

await esbuild.build({
  ...buildSettings,
  entryPoints: ["src/browser/index.html"],
  outdir: "dist/client",
  plugins: [htmlPlugin(), lessLoader()],
});

await esbuild.build({
  ...buildSettings,
  entryPoints: ["src/server/index.js"],
  outdir: "dist/server",
  target: 'node12',
  plugins: [lessLoader()],
  platform: 'node',
});
