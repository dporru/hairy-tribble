angular.module('ph', ['ui.bootstrap', 'angularUtils.directives.dirPagination', 'dndLists']);

angular.module('ph').value('API_PATH', '/api/v0.0.0/');

angular.module('ph').config(['paginationTemplateProvider', function(paginationTemplateProvider) {
    paginationTemplateProvider.setPath('assets/bower_components/angular-utils-pagination/dirPagination.tpl.html');

    // Trumbowyg settings.
    $.trumbowyg.upload = {serverPath: '/api/v0.0.0/image'};
}]);
