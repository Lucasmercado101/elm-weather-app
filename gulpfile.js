const { src, dest, series, parallel } = require("gulp");
const elm = require("gulp-elm");
const uglify = require("gulp-uglify");
const htmlmin = require("gulp-htmlmin");
const cleanCss = require("gulp-clean-css");
const jsonminify = require("gulp-jsonminify");

const elmToJs = () =>
  src("src/Main.elm")
    .pipe(
      elm.bundle("elm.js", {
        optimize: true
      })
    )
    .pipe(uglify())
    .pipe(dest("dist"));

const html = () =>
  src("public/index.html")
    .pipe(
      htmlmin({
        collapseWhitespace: true,
        minifyCSS: true,
        removeComments: true
      })
    )
    .pipe(dest("dist"));

const css = () => src("public/styles.css").pipe(cleanCss()).pipe(dest("dist"));

const copyAssets = () => src("public/assets/*").pipe(dest("dist/assets"));

const uglifyJs = () => src("public/**/*.js").pipe(uglify()).pipe(dest("dist"));

const copyWebManifest = () =>
  src("public/manifest.webmanifest").pipe(jsonminify()).pipe(dest("dist"));

exports.default = parallel(
  elmToJs,
  html,
  css,
  copyAssets,
  uglifyJs,
  copyWebManifest
);
