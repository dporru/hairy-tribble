angular.module('ph').factory('Question', ['$http', 'Alert', 'API_PATH', 'Auth', function($http, Alert, API_PATH, Auth) {
    var questions = [];
    var questionsById = {};
    var changedCallbacks = [];
    var testMethods;

    function changedExecute() {
        for (var i in changedCallbacks) {
            changedCallbacks[i]();
        }
    }

    function handleRequestError(msg) {
        return function(request) {
            if (request.status === 401) {
                Auth.setLoginNeeded(request.data);
                throw 'Authentication needed';
            } else {
                Alert.add(msg, 'danger');
            }
        };
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
                .catch(handleRequestError('Er trad een fout op bij het ophalen van de vragen.'));
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
                .catch(handleRequestError('Er trad een fout op bij het aanmaken van de vraag.'));
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
                .catch(handleRequestError('Er trad een fout op bij het opslaan van de vraag.'));
        },
        remove: function(question_id) {
            return $http.delete(API_PATH + 'question/id/' + question_id)
                .then(function(result) {
                    methods.load();
                    if (testMethods) {
                        if (testMethods.isQuestionInCurrentTest(question_id)) {
                            testMethods.removeQuestionFromCurrentTest(question_id);
                        }
                    }
                })
                .catch(handleRequestError('Er trad een fout op bij het verwijderen van de vraag.'));
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
        },
        setTestMethods: function(methods) {
            testMethods = methods;
        }
    };

    return methods;
}]);
