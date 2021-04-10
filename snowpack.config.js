/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    public: { url: "/", static: true },
    src: { url: "/dist" },
  },
  plugins: [
    "@snowpack/plugin-typescript",
    "@snowpack/plugin-react-refresh",
    [
      "@snowpack/plugin-webpack",
      {
        sourceMap: true,
      },
    ],
  ],
  routes: [],
  optimize: {
    bundle: true,
    minify: true,
    target: "es2018",
  },
  packageOptions: {},
  devOptions: {},
  buildOptions: {},
};
