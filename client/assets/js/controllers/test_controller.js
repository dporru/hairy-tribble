angular.module('ph').controller('TestController', function($modal, $http, Question){
    var test = this;
    test.currentTest = null;
    test.newQuestionType = 'open';
    test.questionFormFocus = false;

    test.reloadTests = function(){
        $http.get('/api/v0.0.0/test').then(function(result){
            var tests = result.data.items;
            if (!tests.length) {
                test.openNewTestModal();
            }else if (!test.currentTest && tests.length) {
                test.currentTest = tests[0];
            }
        });        
    }

    test.reloadCurrentTest = function(){
        if (typeof(test.currentTest) != 'undefined'){
            test.currentTest.get().then(function(data){
                test.currentTest = data;
            });
        }
    }

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

        $http.post('/api/v0.0.0/question', newQuestion).then(function(){
            updatedTest = {
                name: test.currentTest.object.name,
                questions: ['Question-gqimwjlw']
            }
            $http.put('/api/v0.0.0/test/id/' + test.currentTest.id, updatedTest).then(function(){
                // test.reloadCurrentTest();
                test.resetNewQuestion();
                Question.load();
            });
        });

        // TESTS.items[0].questions.push(newQuestion);
        // test.reloadCurrentTest();
        // test.resetNewQuestion();
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

    test.reloadTests();
});
