angular.module('ph').factory('Test', ['$http', 'Question', 'Alert', 'API_PATH', function($http, Question, Alert, API_PATH){
    var currentTestId = null;
    var tests = [];
    var testsById = {};
    var changedCallbacks = [];

    function changedExecute() {
        for (var i in changedCallbacks) {
            changedCallbacks[i]();
        }
    }

    var methods = {
        getList: function() {
            return tests;
        },
        createTest: function(newTest, labels) {
            if (typeof labels == 'undefined') {
                labels = [];
            }
            var object = {
                object_: newTest,
                labels_: labels
            };
            return $http.post(API_PATH + 'test', object)
                .then(function() {
                    return methods.load();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het aanmaken van de toets.', 'danger');
                    throw e;
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
        getCurrentTestElements: function() {
            var elements = [];
            if (testsById && currentTestId) {            
                for (var i in testsById[currentTestId].object.elements) {
                    var element = testsById[currentTestId].object.elements[i];
                    if (typeof element.testQuestion != 'undefined') {
                        var questionId =  element.testQuestion;
                        var question = Question.getQuestionById(questionId);
                        if (question) {
                            elements.push({'testQuestion': question});
                        }
                    } else {
                        elements.push(element);
                    }
                }
            }
            return elements;
        },
        load: function() {
            return $http.get(API_PATH + 'test')
                .then(function(result){
                    tests = result.data.items;
                    for (var i in tests) {
                        testsById[tests[i].id] = tests[i];
                    }
                    changedExecute();
                    return tests;
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het ophalen van de toetsen.', 'danger');
                    throw e;
                });
        },
        addQuestion: function(testId, questionId) {
            var test = testsById[testId];
            var questionIdList = test.object.elements;
            questionIdList.push({'testQuestion': questionId});

            return methods.saveElementIdList(testId, questionIdList);
        },
        saveElementIdList: function(testId, elementIdList) {
            var test = testsById[testId];
            var updatedTest = {
                object_: test.object,
                labels_: test.labels
            };
            updatedTest.object_.elements = elementIdList;

            return $http.put(API_PATH + 'test/id/' + testId, updatedTest)
                .then(function(){
                    methods.load();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het opslaan van de vragenlijst.', 'danger');
                    throw e;
                });
        },
        saveElementList: function(testId, elementList) {
            var elementIdList = [];
            for (var i in elementList) {
                var element = elementList[i];
                if (typeof element.testQuestion != 'undefined') {
                    elementIdList.push({'testQuestion': element.testQuestion.id});
                } else {
                    elementIdList.push(element);
                }
            }
            return methods.saveElementIdList(testId, elementIdList);
        },
        saveCurrentElementList: function(elementList) {
            return methods.saveElementList(currentTestId, elementList);
        },
        addQuestionToCurrentTest: function(questionId) {
            if (currentTestId) {
                return methods.addQuestion(currentTestId, questionId);
            }
        },
        isQuestionInCurrentTest: function(questionId) {
            if (currentTestId) {
                for (var i in testsById[currentTestId].object.elements) {
                    var element = testsById[currentTestId].object.elements[i];
                    if (element.testQuestion === questionId) {
                        return true;
                    }
                }
            }

            return false;
        },
        removeQuestionFromCurrentTest: function(questionId) {
            var elements = [];
            for (var i in testsById[currentTestId].object.elements) {
                var element = testsById[currentTestId].object.elements[i];
                if (element.testQuestion !== questionId) {
                    elements.push(element);
                }
            }
            methods.saveElementIdList(currentTestId, elements);

        },
        removeTest: function(testId) {
            return $http.delete(API_PATH + 'test/id/' + testId)
                .then(function(){
                    methods.load();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het verwijderen van de toets.', 'danger');
                    throw e;
                });
        },
        changed: function(callback) {
            changedCallbacks.push(callback);
        },
        getCurrentTestExportUrl: function(type, showAnswers) {
            var parameters = '';
            if (showAnswers) {
                parameters = '?withAnswers';
            }
            return API_PATH + 'test/id/' + currentTestId + '/export/' + type + parameters;
        },
        updateTestName: function(testId, newName) {
            var updatedTest = {
                name: newName,
                questions: testsById[testId].object.questions
            };

            return $http.put(API_PATH + 'test/' + testId, updatedTest)
                .then(function(){
                    methods.load();
                })
                .catch(function(){
                    Alert.add('Er trad een fout op bij het opslaan van de nieuwe toetsnaam.', 'danger');
                    throw e;
                });
        }
    };

    return methods;
}]);
