angular.module('ph', ['ui.bootstrap', 'angularUtils.directives.dirPagination', 'dndLists']);

angular.module('ph').config(['paginationTemplateProvider', function(paginationTemplateProvider) {
    paginationTemplateProvider.setPath('assets/bower_components/angular-utils-pagination/dirPagination.tpl.html');
}]);