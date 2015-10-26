angular.module('ph', ['ui.bootstrap', 'angularUtils.directives.dirPagination', 'dndLists', 'LocalStorageModule']);

angular.module('ph').value('API_PATH', '/api/v0.0.0/');

angular.module('ph').config(['paginationTemplateProvider', 'localStorageServiceProvider', function(paginationTemplateProvider, localStorageServiceProvider) {
    paginationTemplateProvider.setPath('assets/bower_components/angular-utils-pagination/dirPagination.tpl.html');

    // Trumbowyg settings.
    $.trumbowyg.upload = {serverPath: '/uploaded'};

    // Localstorage settings.
    localStorageServiceProvider.setPrefix('ph');
}]);
