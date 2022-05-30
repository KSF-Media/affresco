import { buildOrServe, buildSettings } from "../../esbuild.defaults.mjs";

await buildOrServe({
  ...buildSettings,
  entryPoints: ["src/index.html"],
});
