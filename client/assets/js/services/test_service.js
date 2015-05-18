angular.module('ph').factory('Test', ['$http', function($http){
    var tests = [];
    var testsById = {};

    function getTestQuestionIds(testId) {
        var questionIds = [];
        for (var i in testsById[testId].object.questions) {
            questionIds.push(testsById[testId].object.questions[i].id);
        }
        return questionIds;
    }

    var methods = {
        getList: function() {
            return tests;
        },
        getTestById: function(id) {
            return testsById[id];
        },
        load: function() {
            return $http.get('/api/v0.0.0/test').then(function(result){
                tests = result.data.items;
                for (var i in tests) {
                    testsById[tests[i].id] = tests[i];
                }
            });
        },
        addQuestion: function(testId, questionId) {
            var questionIds = getTestQuestionIds(testId).push(questionId);
            updatedTest = {
                name: testsById[testId].object.name,
                questions: questionIds
            }
            return $http.put('/api/v0.0.0/test/id/' + testId, updatedTest).then(function(){
                methods.load();
            });
        }
    };

    return methods;
}]);