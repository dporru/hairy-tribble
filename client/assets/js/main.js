angular.module('ph', ['ui.bootstrap', 'angularUtils.directives.dirPagination', 'dndLists', 'trumbowyg-ng']);

angular.module('ph').config(['paginationTemplateProvider', function(paginationTemplateProvider) {
    paginationTemplateProvider.setPath('assets/bower_components/angular-utils-pagination/dirPagination.tpl.html');
}]);

// Set trumbowyg upload destination.
$.trumbowyg.upload = {serverPath: '/api/v0.0.0/image'};