angular.module('ph').factory('Question', ['$http', function($http){
    var questions = [];

    var methods = {
        getList: function() {
            return questions;
        },
        load: function() {
            $http.get('/api/v0.0.0/question').then(function(result){
                questions = result.data.items;
            });
        },
        create: function(newQuestion) {
            return $http.post('/api/v0.0.0/question', newQuestion).then(function() {
                methods.load();
            });
        }
    };

    return methods;
}]);