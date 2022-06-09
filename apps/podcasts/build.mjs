import { buildOrServe, buildSettings } from "../../esbuild.defaults.mjs";

await buildOrServe({
  ...buildSettings,
  entryPoints: ["src/index.html"],
  define: {
    ...buildSettings.define,
    "process.env.PODCAST_IDS": '"' + process.env.PODCAST_IDS + '"',
  }
});
