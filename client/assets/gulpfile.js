var gulp = require('gulp');
var concat = require('gulp-concat');
var sourcemaps = require('gulp-sourcemaps');
var uglify = require('gulp-uglify');
var jshint = require('gulp-jshint');

var javascript_libs = [
    'bower_components/jquery/dist/jquery.js',
    'bower_components/angular/angular.js',
    'bower_components/angular-bootstrap/ui-bootstrap.js',
    'bower_components/angular-bootstrap/ui-bootstrap-tpls.js',
    'bower_components/angular-drag-and-drop-lists/angular-drag-and-drop-lists.js',
    'bower_components/angular-utils-pagination/dirPagination.js',
    'bower_components/trumbowyg/dist/trumbowyg.js',
    'bower_components/trumbowyg/dist/langs/nl.min.js',
    'bower_components/trumbowyg/dist/plugins/upload/trumbowyg.upload.js',
    'bower_components/angular-local-storage/dist/angular-local-storage.js',
    'js/main.js',
    'js/controllers/*.js',
    'js/services/*.js',
    'js/directives/*.js',
    'js/filters/*.js'
];

gulp.task('default', function() {
    gulp.start('uglify');
});

gulp.task('uglify', function() {
    return gulp.src(javascript_libs)
        .pipe(sourcemaps.init())
            .pipe(concat('all.min.js'))
            // .pipe(uglify())
        .pipe(sourcemaps.write('../maps'))
        .pipe(gulp.dest('dist'));
});

gulp.task('watch', function() {
    gulp.watch('js/**/*.js', ['lint', 'uglify']);
});

gulp.task('lint', function() {
    return gulp.src([
        './js/main.js',
        './js/controllers/*.js',
        './js/services/*.js',
        './js/filters/*.js',
    ])
        .pipe(jshint())
        .pipe(jshint.reporter('default'));
});
