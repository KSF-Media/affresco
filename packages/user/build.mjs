import { buildOrServe, buildSettings } from "../../esbuild.defaults.mjs";
import htmlPlugin from "@chialab/esbuild-plugin-html";
import { lessLoader } from "esbuild-plugin-less";

await buildOrServe({
  ...buildSettings,
  outdir: "build",
  entryPoints: [
    "../../less/Registration.less",
    "../../less/Login.less",
    "../../less/Button.less",
    "../../less/ksf-utils.less",
    "../../less/InputField.less",
  ],
  plugins: [htmlPlugin(), lessLoader()],
  sourcemap: undefined,
});
