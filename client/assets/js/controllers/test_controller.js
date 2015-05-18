angular.module('ph').controller('TestController', function($modal, $http, Question, Test){
    var test = this;
    test.currentTestId = null;
    test.newQuestionType = 'open';
    test.questionFormFocus = false;

    test.getCurrentTest = function() {
        return Test.getTestById(test.currentTestId);
    };

    test.getTests = function() {
        return Test.getList();
    };

    test.getNewQuestionRows = function(){
        return !test.questionFormFocus ? 2 : 5;
    };

    test.submitNewQuestion = function(){
        if (test.newQuestionType == 'open'){
            var answer = {open: test.newOpenAnswer};
        }else{
            var answer = {multipleChoice: {correct: 'correct', incorrect: ['incorrect1', 'incorrect2']}};
        }

        var newQuestion = {
            question: test.newQuestion,
            answer: answer
        };

        Question.create(newQuestion).then(function() {
            var questionId = 'Question-gqimwjlw';
            Test.addQuestion(test.currentTestId, questionId);
        });
    };

    test.resetNewQuestion = function(){
        test.newQuestion = '';
        test.newOpenAnswer = '';
        test.newQuestionType = 'open';
    };

    test.openNewTestModal = function(){
        var modalInstance = $modal.open({
            templateUrl: 'new_test_modal.html',
            controller: 'NewTestController as nt',
            resolve: {context: function(){return 'Mijn context'}},
            keyboard: false
        });

        modalInstance.result.then(function (newTestName) {

            newTest = {
                'name': newTestName,
                'questions': []
            }
            tests.post(newTest).then(function(newTest){
                test.currentTest = newTest;
                test.reloadCurrentTest();
            });
        });
    };

    Test.load().then(function() {
        if (!test.currentTestId && test.getTests().length) {
            test.currentTestId = test.getTests()[0].id;
        }
    });
});
