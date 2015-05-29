angular.module('ph').factory('Question', ['$http', function($http){
    var questions = [];
    var questionsById = {};
    var changedCallbacks = [];

    function changedExecute() {
        for (var i in changedCallbacks) {
            changedCallbacks[i]();
        }
    }

    var methods = {
        getList: function() {
            return questions;
        },
        getQuestionById: function(questionId) {
            if (questionsById) {
                return questionsById[questionId];
            }
        },
        load: function() {
            $http.get('/api/v0.0.0/question').then(function(result){
                questions = result.data.items;
                for (var i in questions) {
                    questionsById[questions[i].id] = questions[i];
                }
                changedExecute();
            });
        },
        create: function(newQuestion) {
            return $http.post('/api/v0.0.0/question', newQuestion).then(function() {
                methods.load();
            });
        },
        changed: function(callback) {
            changedCallbacks.push(callback);
        }
    };

    return methods;
}]);