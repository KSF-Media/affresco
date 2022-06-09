import { buildOrServe, buildSettings } from "../../esbuild.defaults.mjs";
import alias from 'esbuild-plugin-alias';

await import.meta.resolve('querystring-es3').then(qp => buildOrServe({
  ...buildSettings,
  entryPoints: ["index.html"],

  plugins: [
    ...buildSettings.plugins,
    alias({
      'querystring': qp.substr('file://'.length),
    }),
  ],
}));
