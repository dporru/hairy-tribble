angular.module('ph').controller('TestController', ['$modal', '$http', 'Question', 'Test', 'Alert', function($modal, $http, Question, Test, Alert){
    var testCtrl = this;
    testCtrl.questionFormFocus = false;
    testCtrl.questions = [];
    testCtrl.newQuestion = {};

    // testCtrl.newQuestion = {object:{question: "de vraag", answer: {multipleChoice: {correct: 'correct', incorrect: ['incorrect1', 'incorrect2']}}}};
    // testCtrl.newQuestion = {object:{question: "de vraag", answer: {open: 'Open antwoord'}}};

    testCtrl.getCurrentTest = function() {
        return Test.getCurrentTest();
    };

    Test.changed(function() {
        testCtrl.questions = Test.getCurrentTestQuestions();
    });

    Question.changed(function() {
        testCtrl.questions = Test.getCurrentTestQuestions();
    });

    testCtrl.removeQuestion = function(questionId) {
        Test.removeQuestionFromCurrentTest(questionId);
    };

    testCtrl.submitNewQuestion = function(){
        // if (testCtrl.newQuestionType == 'open'){
        //     answer = {open: testCtrl.newOpenAnswer};
        // }else{   
        //     answer = {multipleChoice: {correct: 'correct', incorrect: ['incorrect1', 'incorrect2']}};
        // }

        // Question.create(testCtrl.newQuestion).then(function() {
        // });
            console.log(testCtrl.newQuestion);
            testCtrl.newQuestion = {};
            Alert.add('Nieuwe vraag toegevoegd.', 'success');
    };

    testCtrl.openTestListModal = function(){
        var modalInstance = $modal.open({
            templateUrl: 'test_list.html',
            controller: 'TestListController as test_list',
            resolve: {context: function(){return 'Mijn context';}},
            keyboard: false
        });
    };

    testCtrl.questionMoved = function($index) {
        testCtrl.questions.splice($index, 1);
        Test.saveCurrentQuestionList(testCtrl.questions);
    };

    testCtrl.getQuestionTitle = function(question) {
        return Question.getQuestionTitle(question);
    };

    testCtrl.getCurrentTestExportUrl = function(type) {
        return Test.getCurrentTestExportUrl(type);
    };

    // Test.load().then(function(tests) {
    //     if (!testCtrl.getCurrentTest() && typeof tests != 'undefined' && tests.length) {
    //         Test.setCurrentTest(tests[0].id);
    //     }else if(!testCtrl.getCurrentTest()) {
    //         testCtrl.openTestListModal();
    //     }
    // });
}]);
