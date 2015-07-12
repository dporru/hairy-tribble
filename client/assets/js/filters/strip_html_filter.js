angular.module('ph').filter('stripHTML', [function() {
    return function(text) {
      return String(text).replace(/<[^>]+>/gm, '');
    };
}]);