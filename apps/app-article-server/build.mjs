import esbuild from "esbuild";
import { buildSettings } from "../../esbuild.defaults.mjs";

await esbuild.build({
  ...buildSettings,
  entryPoints: ["src/browser/index.html"],
  outdir: "dist/client",
});

await esbuild.build({
  ...buildSettings,
  entryPoints: ["src/server/index.js"],
  outdir: "dist/server",
  target: 'node12',
  platform: 'node',
});
