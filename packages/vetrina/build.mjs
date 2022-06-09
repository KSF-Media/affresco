import { buildOrServe, buildSettings } from "../../esbuild.defaults.mjs";
import htmlPlugin from "@chialab/esbuild-plugin-html";
import { lessLoader } from "esbuild-plugin-less";

await buildOrServe({
  ...buildSettings,
  outdir: "build",
  entryPoints: ["../../less/Vetrina.less"],
  plugins: [htmlPlugin(), lessLoader()],
  sourcemap: undefined,
});
