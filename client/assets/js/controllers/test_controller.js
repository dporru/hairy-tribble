angular.module('ph').controller('TestController', function($modal, $http, Question, Test){
    var test = this;
    test.newQuestionType = 'open';
    test.questionFormFocus = false;

    test.getCurrentTest = function() {
        return Test.getCurrentTest();
    };

    test.getQuestions = function() {
        return Test.getCurrentTestQuestions();
    };

    test.removeQuestion = function(questionId) {
        Test.removeQuestionFromCurrentTest(questionId);
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
            test.resetNewQuestion();
            // var questionId = 'Question-gqimwjlw';
            // Test.addQuestion(Test.getCurrentTest().id, questionId);
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
            Test.createTest(newTest);
        });
    };

    Test.load().then(function(tests) {
        if (!test.getCurrentTest() && tests.length) {
            Test.setCurrentTest(tests[0].id);
        }else if(!test.getCurrentTest()) {
            test.openNewTestModal();
        }
    });
});
