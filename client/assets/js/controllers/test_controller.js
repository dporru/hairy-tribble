angular.module('ph').controller('TestController', function($modal, $http, Question, Test, Alert){
    var testCtrl = this;
    testCtrl.newQuestionType = 'open';
    testCtrl.questionFormFocus = false;
    testCtrl.questions = [];

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

    testCtrl.getNewQuestionRows = function(){
        return !testCtrl.questionFormFocus ? 2 : 5;
    };

    testCtrl.submitNewQuestion = function(){
        if (testCtrl.newQuestionType == 'open'){
            var answer = {open: testCtrl.newOpenAnswer};
        }else{
            var answer = {multipleChoice: {correct: 'correct', incorrect: ['incorrect1', 'incorrect2']}};
        }

        var newQuestion = {
            question: testCtrl.newQuestion,
            answer: answer
        };

        Question.create(newQuestion).then(function() {
            testCtrl.resetNewQuestion();
            Alert.add('Nieuwe vraag toegevoegd.', 'success');
        });
    };

    testCtrl.resetNewQuestion = function(){
        testCtrl.newQuestion = '';
        testCtrl.newOpenAnswer = '';
        testCtrl.newQuestionType = 'open';
    };

    testCtrl.openTestListModal = function(){
        var modalInstance = $modal.open({
            templateUrl: 'test_list.html',
            controller: 'TestListController as test_list',
            resolve: {context: function(){return 'Mijn context'}},
            keyboard: false
        });
    };

    testCtrl.questionMoved = function($index) {
        testCtrl.questions.splice($index, 1);
        Test.saveCurrentQuestionList(testCtrl.questions);
    }

    testCtrl.getQuestionTitle = function(question) {
        return Question.getQuestionTitle(question);
    };

    testCtrl.getCurrentTestExportUrl = function(type) {
        return Test.getCurrentTestExportUrl(type);
    };

    Test.load().then(function(tests) {
        if (!testCtrl.getCurrentTest() && tests.length) {
            Test.setCurrentTest(tests[0].id);
        }else if(!testCtrl.getCurrentTest()) {
            testCtrl.openTestListModal();
        }
    });
});
