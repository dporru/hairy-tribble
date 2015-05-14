angular.module('ph', ['restangular', 'ui.bootstrap']);

angular.module('ph').config(function(RestangularProvider){
    RestangularProvider.setBaseUrl('/api/v0.0.0');

    RestangularProvider.addResponseInterceptor(function(data, operation, what, url, response, deferred){
        var extractedData;

        if (operation === "getList") {
            extractedData = data.items;
            extractedData.offset = data.offset;
            extractedData.count = data.count;
        } else {
            extractedData = data.data;
        }
        return extractedData;
    });
});

angular.module('ph').controller('TestController', function(Restangular, $modal){
    var test = this;
    test.currentTest = null;
    test.newQuestionType = 'open';
    test.questionFormFocus = false;

    tests = Restangular.all('test');
    questions = Restangular.all('question');

    test.reloadTests = function(){
        tests.getList().then(function(tests){
            if (!tests.length) {
                tests.openNewTestModal();
            }else if (!test.currentTest && tests.length) {
                test.currentTest = tests[0];
                console.log(test.currentTest);
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

        questions.post(newQuestion).then(function(){
            test.reloadCurrentTest();
            test.resetNewQuestion();
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
            controller: 'NewTestModalController as nt',
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

angular.module('ph').controller('NewTestModalController', function ($modalInstance, context) {

    nt = this;

    nt.save = function () {
        $modalInstance.close(nt.newTestName);
    };

    nt.cancel = function () {
        $modalInstance.dismiss('cancel');
    };
});