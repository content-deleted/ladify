const gulp = require('gulp');
const elm = require('gulp-elm');
const plumber = require('gulp-plumber');
const open = require('gulp-open');
const connect = require('gulp-connect'); 
const del = require('del');
const sass = require('gulp-sass');
const concat = require('gulp-concat');
const replace = require('gulp-replace');
const fs = require('fs');
const rename = require('gulp-rename');

const paths = {
  dest: 'dist',
  elm: 'src/**/*.elm',
  main: 'src/Main.elm',
  staticAssets: 'static/**/*.{html,css}',
  static: 'static',
  scss: 'src/**/*.scss',
  htmlOut: 'dist/Main.html',
  endpoints: ['dist/edit/', 'dist/playlist-stats/']
};

// scss compile task
gulp.task('scss', function () {
  return gulp.src(paths.scss)
    .pipe(sass({outputStyle: 'compressed'}).on('error', sass.logError))
    // concat everything to a single css file
    .pipe(concat('compressed.css'))
    .pipe(gulp.dest(paths.static));
});

gulp.task('clean', function(cb) {
  del([paths.dest], cb);
});

gulp.task('elm', function() {
  return gulp.src(paths.main)
    .pipe(plumber())
    .pipe(elm({ optimize: true, filetype: "html"}))
    // replace the style with the minified css we generated
    .pipe(replace('body { padding: 0; margin: 0; }', function(match) {
      return fs.readFileSync("static/compressed.css", "utf8");
     }))
    .pipe(gulp.dest(paths.dest));
});
  
gulp.task('staticAssets', function() {
  return gulp.src(paths.staticAssets)
    .pipe(plumber())
    .pipe(gulp.dest(paths.dest));
});

gulp.task('copyEndpoints', function() {
  var p = gulp.src(paths.htmlOut)
    .pipe(rename('index.html'));
  
  paths.endpoints.forEach(d => p.pipe(gulp.dest(d)));

  return p;
});

gulp.task('watch', function() {
  gulp.watch(paths.elm, ['elm','copyEndpoints']);
  gulp.watch(paths.scss, ['scss', 'staticAssets', 'elm', 'copyEndpoints']);
});

gulp.task('build', ['scss', 'elm', 'staticAssets', 'copyEndpoints']);
gulp.task('dev', ['build', 'watch']);
gulp.task('default', ['build']);


// -- Run Server --

// Configuration
var configuration = {
    paths: {
        dist: 'dist/*'
    },
    localServer: {
        port: 8001,
        url: 'http://localhost:8001/'
    }
};

// Gulp task to create a web server
gulp.task('connect', function () {
    connect.server({
        root: 'dist',
        port: configuration.localServer.port,
        livereload: true
    });
});

// Gulp task to open the default web browser
gulp.task('open', function(){
    gulp.src('dist/Main.html')
        .pipe(open({uri: configuration.localServer.url}));
});

// Watch the file system and reload the website automatically
gulp.task('watchHTML', function () {
    gulp.watch(configuration.paths.dist, ['open']);
});

// Pipe for everything we need for server
gulp.task('devHTML', ['connect']);