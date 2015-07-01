angular.module('ph').factory('Question', ['$http', 'Alert', 'API_PATH' , function($http, Alert, API_PATH) {
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
            $http.get(API_PATH + 'question')
                .then(function(result){
                    questions = result.data.items;
                    for (var i in questions) {
                        questionsById[questions[i].id] = questions[i];
                    }
                    changedExecute();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het ophalen van de vragen.', 'danger');
                });
        },
        create: function(newQuestion) {
            return $http.post(API_PATH + 'question', newQuestion)
                .then(function() {
                    methods.load();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het aanmaken van de vraag.', 'danger');
                });
        },
        changed: function(callback) {
            changedCallbacks.push(callback);
        },
        getQuestionTitle: function(question) {
            var title = question.object.question;

            if (title.indexOf('\n') !== -1) {
                title = title.substring(0, title.indexOf('\n'));
            }

            return title;
        }
    };

    return methods;
}]);