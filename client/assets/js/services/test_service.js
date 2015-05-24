angular.module('ph').factory('Test', ['$http', 'Question', function($http, Question){
    var currentTestId = null;
    var tests = [];
    var testsById = {};

    function getTestQuestionIds(testId) {
        var questionIds = [];
        for (var i in testsById[testId].object.questions) {
            questionIds.push(testsById[testId].object.questions[i]);
        }
        return questionIds;
    }

    var methods = {
        getList: function() {
            return tests;
        },
        createTest: function(newTest) {
            return $http.post('/api/v0.0.0/test', newTest).then(function() {
                return methods.load();
            });
        },
        getTestById: function(id) {
            return testsById[id];
        },
        getCurrentTest: function() {
            if (currentTestId) {
                return methods.getTestById(currentTestId);
            }
        },
        setCurrentTest: function(testId) {
            currentTestId = testId;
        },
        getCurrentTestQuestions: function() {
            var questions = [];
            if (testsById && currentTestId) {            
                for (var i in testsById[currentTestId].object.questions) {
                    var questionId =  testsById[currentTestId].object.questions[i];
                    var question = Question.getQuestionById(questionId);
                    if (question) {
                        questions.push(question);
                    }
                }
            }
            return questions;
        },
        load: function() {
            return $http.get('/api/v0.0.0/test').then(function(result){
                tests = result.data.items;
                for (var i in tests) {
                    testsById[tests[i].id] = tests[i];
                }
                return tests;
            });
        },
        addQuestion: function(testId, questionId) {
            var questionIds = getTestQuestionIds(testId);
            questionIds.push(questionId);

            updatedTest = {
                name: testsById[testId].object.name,
                questions: questionIds
            }
            return $http.put('/api/v0.0.0/test/id/' + testId, updatedTest).then(function(){
                methods.load();
            });
        },
        addQuestionToCurrentTest: function(questionId) {
            if (currentTestId) {
                methods.addQuestion(currentTestId, questionId);
            }
        },
        isQuestionInCurrentTest: function(questionId) {
            if (currentTestId) {
                for (var i in testsById[currentTestId].object.questions) {
                    if (testsById[currentTestId].object.questions[i] === questionId) {
                        return true;
                    }
                }
            }

            return false;
        },
        removeQuestionFromCurrentTest: function(questionId) {
            var questions = [];
            for (var i in testsById[currentTestId].object.questions) {
                if (testsById[currentTestId].object.questions[i] !== questionId) {
                    questions.push(testsById[currentTestId].object.questions[i]);
                }
            }
            testsById[currentTestId].object.questions = questions;
        }
    };

    return methods;
}]);