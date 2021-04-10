/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    public: { url: "/", static: true },
    src: { url: "/dist" },
  },
  plugins: ["@snowpack/plugin-typescript", "@snowpack/plugin-react-refresh"],
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
