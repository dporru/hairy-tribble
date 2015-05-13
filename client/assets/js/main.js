QUESTIONS = {
    offset: 0,
    count: 3,
    items: [
        {
            id: 1,
            question: 'Dit is de vraag?',
            answer: {
                open: 'Maar wat is het antwoord?'
            }
        },
        {
            id: 2,
            question: 'Welk antwoord is juist?',
            answer: {
                multipleChoice: {
                    correct: 'Deze natuurlijk',
                    incorrect: ['Deze niet', 'En deze ook niet'],
                    order: [1,2,0]
                }
            }
        }
    ]
};

TESTS = {
    offset: 0, 
    count: 2,
    items: [
        {id: 1,name: 'Daans eerste test', questions: QUESTIONS.items},
        {id: 2, name: 'Een andere mooie test', questions: []}
    ]
};

angular.module('ph', ['restangular']);

angular.module('ph').config(function(RestangularProvider){
    RestangularProvider.setBaseUrl('/api/v0.0.0');

    RestangularProvider.addResponseInterceptor(function(data, operation, what, url, response, deferred){
        var extractedData;
        if (what == 'test') {
            data = TESTS;
        }

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

angular.module('ph').controller('TestController', function(Restangular){
    var test = this;
    test.currentTest = null;
    test.newQuestionType = 'open';
    test.questionFormFocus = false;

    tests = Restangular.all('test');

    test.reloadTests = function(){
        tests.getList().then(function(tests){
            if (!test.currentTest && tests.length) {
                test.currentTest = tests[0];
            }
        },function(){
            test.currentTest = TESTS.items[0];
        });        
    }

    test.reloadCurrentTest = function(){
        if (typeof(test.currentTest) != 'undefined'){
            test.currentTest.get().then(function(data){
                test.currentTest = data;
            }, function(){
                test.currentTest = TESTS.items[0];
            });
        }
    }

    questions = Restangular.all('question');

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

    test.reloadTests();
});