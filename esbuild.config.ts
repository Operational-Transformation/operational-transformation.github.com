import type { BuildOptions } from 'esbuild';
import { polyfillNode } from 'esbuild-plugin-polyfill-node';
import esbuild from 'esbuild';
import dotenv from 'dotenv';

dotenv.config();

const isProduction = process.env.NODE_ENV === 'production';

const config: BuildOptions = {
  entryPoints: ['src/index.tsx'],
  bundle: true,
  outdir: './local',
  sourcemap: !isProduction,
  minify: isProduction,
  define: {
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV || 'development'),
    'global': 'window',
  },
  target: ['es2015'],
  loader: {
    '.png': 'file',
    '.jpg': 'file',
    '.svg': 'file',
    '.css': 'css',
  },
  platform: 'browser',
  // external: ['ot'],
  plugins: [
    polyfillNode({
      globals: {
        buffer: true,
        process: true,
      },
    }),
  ],
  metafile: true,
};

async function main() {

  const ctx = await esbuild.context(config);
  await esbuild.build(config)
  .then((result: esbuild.BuildResult<BuildOptions>) => {
    console.log(result)
  })
  .catch((error) => {
    console.error('build error:', error);
    process.exit(1);
  });
  await ctx.watch();
  const port = 3999;
  await ctx.serve({
    servedir: './local',
    port,
  });
  console.log(`http://localhost:${port}`);


}
main();