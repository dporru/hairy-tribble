angular.module('ph').controller('TestController', ['$modal', '$http', 'Question', 'Test', 'Alert', 'Auth', function($modal, $http, Question, Test, Alert, Auth){
    var testCtrl = this;
    testCtrl.elements = [];
    testCtrl.newQuestion = {};
    testCtrl.editingNewQuestion = false;
    testCtrl.editingQuestionId = null;

    testCtrl.loginNeeded = function() {
        return Auth.getLoginNeeded();
    };

    testCtrl.getCurrentTest = function() {
        return Test.getCurrentTest();
    };

    Test.changed(function() {
        testCtrl.elements = Test.getCurrentTestElements();
    });

    Question.changed(function() {
        testCtrl.elements = Test.getCurrentTestElements();
    });

    testCtrl.removeQuestionFromTest = function(questionId) {
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

    testCtrl.removeQuestion = function(question) {
        var modalInstance = $modal.open({
            templateUrl: 'remove_confirmation.html',
            controller: 'RemoveConfirmationController as removeCtrl',
            resolve: {
                title: function(){return 'Verwijder ' + question.object.question + '?';},
                content: function(){return 'Weet je zeker dat je ' + question.object.question + ' wilt verwijderen?';}
            },
            keyboard: true
        });

        modalInstance.result.then(function (remove) {
            if (remove) {
                Question.remove(question.id).then(function(){
                    testCtrl.closeEditing();
                });
            }
        });
        Question.remove(question_id).then(function(){
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
        if(!testCtrl.getCurrentTest()) {
            testCtrl.openTestListModal();
        }
    });
}]);
