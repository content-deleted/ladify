var gulp = require('gulp');
var elm = require('gulp-elm');
var plumber = require('gulp-plumber');
var del = require('del');

// builds elm files and static resources (i.e. html and css) from src to dist folder
var paths = {
  dest: 'dist',
  elm: 'src/*.elm',
  main: 'src/Main.elm',
  staticAssets: 'src/*.{html,css}'
};

gulp.task('clean', function(cb) {
  del([paths.dest], cb);
});

gulp.task('elm', function() {
  return gulp.src(paths.main)
    .pipe(plumber())
    .pipe(elm({ optimize: true, filetype: "html"}))
    .pipe(gulp.dest(paths.dest));
});
  
gulp.task('staticAssets', function() {
  return gulp.src(paths.staticAssets)
    .pipe(plumber())
    .pipe(gulp.dest(paths.dest));
});

gulp.task('watch', function() {
  gulp.watch(paths.elm, ['elm']);
  gulp.watch(paths.staticAssets, ['static']);
});

gulp.task('build', ['elm', 'staticAssets']);
gulp.task('dev', ['build', 'watch']);
gulp.task('default', ['build']);


// Server SHit

// Add our dependencies
var gulp = require('gulp'), // Main Gulp module
    concat = require('gulp-concat'), // Gulp File concatenation plugin
    open = require('gulp-open'), // Gulp browser opening plugin
    connect = require('gulp-connect'); // Gulp Web server runner plugin

// Configuration
var configuration = {
    paths: {
        dist: './dist'
    },
    localServer: {
        port: 8001,
        url: 'http://localhost:8001/'
    }
};
/*
// Gulp task to copy HTML files to output directory
gulp.task('html', function() {
    gulp.src(configuration.paths.src.html)
        .pipe(gulp.dest(configuration.paths.dist))
        .pipe(connect.reload());
});

// Gulp task to concatenate our css files
gulp.task('css', function () {
   gulp.src(configuration.paths.src.css)
       .pipe(concat('site.css'))
       .pipe(gulp.dest(configuration.paths.dist + '/css'))
       .pipe(connect.reload());
});
*/
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
    gulp.watch(configuration.paths.dist, ['html']);
    //gulp.watch(configuration.paths.src.css, ['css']);
});

// what we need 
gulp.task('devHTML', ['connect', 'open', 'watchHTML']);
// 'html', 'css',