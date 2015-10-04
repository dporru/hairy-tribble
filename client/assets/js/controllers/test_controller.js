angular.module('ph').controller('TestController', ['$modal', '$http', 'Question', 'Test', 'Alert', function($modal, $http, Question, Test, Alert){
    var testCtrl = this;
    testCtrl.elements = [];
    testCtrl.newQuestion = {};
    testCtrl.editingNewQuestion = false;
    testCtrl.editingQuestionId = null;

    testCtrl.getCurrentTest = function() {
        return Test.getCurrentTest();
    };

    Test.changed(function() {
        testCtrl.elements = Test.getCurrentTestElements();
    });

    Question.changed(function() {
        testCtrl.elements = Test.getCurrentTestElements();
    });

    testCtrl.removeQuestion = function(questionId) {
        Test.removeQuestionFromCurrentTest(questionId);
    };

    testCtrl.submitNewQuestion = function(){
        Question.create(testCtrl.newQuestion).then(function(newQuestionId) {
            testCtrl.newQuestion = {};
            testCtrl.closeEditing();
            return Test.addQuestionToCurrentTest(newQuestionId).then(function(){
                Alert.add('Nieuwe vraag toegevoegd.', 'success');
            });
        });
    };

    testCtrl.saveQuestion = function(question) {
        Question.save(question).then(function(){
            testCtrl.closeEditing();
        });
    };

    testCtrl.openTestListModal = function() {
        var modalInstance = $modal.open({
            templateUrl: 'test_list_modal.html',
            controller: 'TestListController as test_list',
            keyboard: false
        });
    };

    testCtrl.elementMoved = function($index) {
        testCtrl.elements.splice($index, 1);
        Test.saveCurrentElementList(testCtrl.elements);
    };

    testCtrl.getQuestionTitle = function(question) {
        return Question.getQuestionTitle(question);
    };

    testCtrl.getCurrentTestExportUrl = function(type, showAnswers) {
        return Test.getCurrentTestExportUrl(type, showAnswers);
    };

    testCtrl.closeEditing = function() {
        // Reload to dismiss changes.
        Question.load();
        testCtrl.editingQuestionId = null;
        testCtrl.editingNewQuestion = false;
    };

    Test.load().then(function(tests) {
        if (!testCtrl.getCurrentTest() && typeof tests != 'undefined' && tests.length) {
            Test.setCurrentTest(tests[0].id);
        }else if(!testCtrl.getCurrentTest()) {
            testCtrl.openTestListModal();
        }
    });
}]);
