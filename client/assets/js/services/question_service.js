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
        parseQuestion: function(question) {
            question.modificationDate = new Date(question.modificationDate);
            question.creationDate = new Date(question.creationDate);
            return question;
        },
        load: function() {
            $http.get(API_PATH + 'question')
                .then(function(result){
                    questions = result.data.items;
                    for (var i in questions) {
                        questions[i] = methods.parseQuestion(questions[i]);
                        questionsById[questions[i].id] = questions[i];
                    }
                    changedExecute();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het ophalen van de vragen.', 'danger');
                    throw e;
                });
        },
        create: function(question) {
            var newQuestion = {
                'object_': question.object,
                'labels_': question.labels
            };
            return $http.post(API_PATH + 'question', newQuestion)
                .then(function(result) {
                    methods.load();
                    return result.data;
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het aanmaken van de vraag.', 'danger');
                    throw e;
                });
        },
        save: function(question) {
            var updateQuestion = {
                'object_': question.object,
                'labels_': question.labels
            };
            return $http.put(API_PATH + 'question/id/' + question.id, updateQuestion)
                .then(function(result) {
                    methods.load();
                })
                .catch(function(e){
                    Alert.add('Er trad een fout op bij het opslaan van de vraag.', 'danger');
                    throw e;
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