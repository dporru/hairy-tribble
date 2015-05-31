angular.module('ph').factory('Test', ['$http', 'Question', function($http, Question){
    var currentTestId = null;
    var tests = [];
    var testsById = {};
    var changedCallbacks = [];

    function getTestQuestionIds(testId) {
        var questionIds = [];
        for (var i in testsById[testId].object.questions) {
            questionIds.push(testsById[testId].object.questions[i]);
        }
        return questionIds;
    }

    function changedExecute() {
        for (var i in changedCallbacks) {
            changedCallbacks[i]();
        }
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
            changedExecute();
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
                changedExecute();
                return tests;
            });
        },
        addQuestion: function(testId, questionId) {
            var questionIdList = getTestQuestionIds(testId);
            questionIdList.push(questionId);

            return methods.saveQuestionIdList(testId, questionIdList);
        },
        saveQuestionIdList: function(testId, questionIdList) {
            var updatedTest = {
                name: testsById[testId].object.name,
                questions: questionIdList
            };

            return $http.put('/api/v0.0.0/test/id/' + testId, updatedTest).then(function(){
                methods.load();
            });
        },
        saveQuestionList: function(testId, questionList) {
            var questionIdList = [];
            for (var i in questionList) {
                questionIdList.push(questionList[i].id);
            }
            return methods.saveQuestionIdList(testId, questionIdList);
        },
        saveCurrentQuestionList: function(questionList) {
            return methods.saveQuestionList(currentTestId, questionList);
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
            methods.saveQuestionIdList(currentTestId, questions);

        },
        changed: function(callback) {
            changedCallbacks.push(callback);
        }
    };

    return methods;
}]);