angular.module('ph').directive('question', function(){
    return {
        restrict: 'E',
        templateUrl: 'question_directive.html',
        scope: {
            question: '=',
            placeholder: '@',
            ngSubmit: '=',
            ngCancel: '=',
            ngRemove: '=',
            reset: '='
        },
        link: function(scope, element, attrs) {
            scope.multipleChoice = {correct: '', incorrect: []};
            scope.$watch('question', function(){
                if (typeof scope.question.object == 'undefined') {
                    scope.questionType = 'open';
                    scope.question.object = {answer: {}, question: ''};
                    scope.question.labels = [];
                    scope.multipleChoice = {answers: []};
                }else{
                    if (typeof scope.question.object.answer.multipleChoice !== 'undefined'){
                        scope.questionType = 'multipleChoice';
                        scope.multipleChoice.answers = [];
                        for (var i in scope.question.object.answer.multipleChoice.order) {
                            var question = {};
                            var order = scope.question.object.answer.multipleChoice.order[i];
                            if (order == 0) {
                                question = {
                                    'value': scope.question.object.answer.multipleChoice.correct,
                                    'correct': true
                                }
                            } else {
                                question = {
                                    'value': scope.question.object.answer.multipleChoice.incorrect[order-1],
                                    'correct': false
                                }
                            }
                            scope.multipleChoice.answers.push(question);
                        }
                        scope.updateMultipleAnswers();
                    }else{
                        scope.questionType = 'open';
                    }
                }

                // Full reset of the editor.
                resetEditors();
            });

            var resetEditors = function() {
                element.find('textarea[name=question]').val(scope.question.object.question);
                element.find('textarea[name=open_answer]').val(scope.question.object.answer.open);
                element.find('textarea').each(function(){
                    $(this).trumbowyg('destroy');
                    $(this).trumbowyg({
                        fullscreenable: true,
                        btns: ['bold','italic','|','btnGrp-lists','|','upload'],
                        lang: 'nl'
                    });
                });

            };

            scope.typeClass = function(type) {
                classes = 'btn btn-default';
                if (type == scope.questionType) {
                    classes = classes + ' btn-info';
                }

                return classes;
            };

            scope.multipleChoiceAnswerClass = function(answer) {
                var classes = 'glyphicon';
                var filledClass = ' text-danger';
                if (answer.correct) {
                    classes = classes + ' glyphicon-ok';
                    filledClass = ' text-success';
                } else {
                    classes = classes + ' glyphicon-remove';
                }
                if (answer.value === '') {
                    classes = classes + ' text-muted';
                }else{
                    classes = classes + filledClass;
                }
                return classes;
            };

            scope.save = function() {
                if (scope.questionType === 'open') {
                    delete(scope.question.object.answer.multipleChoice);
                }else{
                    delete(scope.question.object.answer.open);
                }
                scope.ngSubmit(scope.question);
            };

            scope.updateMultipleAnswers = function() {
                if (typeof scope.multipleChoice.answers === 'undefined' || !scope.multipleChoice.answers.length) {
                    scope.multipleChoice.answers = [{value: '', correct: true}];
                }else if (scope.multipleChoice.answers[scope.multipleChoice.answers.length-1].value !== ''){
                    scope.multipleChoice.answers.push({value: '', correct: false});
                } else if (scope.multipleChoice.answers[scope.multipleChoice.answers.length-2].value === '') {
                    scope.multipleChoice.answers.pop(scope.multipleChoice.answers.length-1);
                }
                var answer = scope.question.object.answer;
                if (typeof answer.multipleChoice === 'undefined') {
                    answer.multipleChoice = {};
                }
                scope.question.object.answer.multipleChoice.order = [];
                answer.multipleChoice.incorrect = [];
                var order = 0;
                for (var i in scope.multipleChoice.answers) {
                    var a = scope.multipleChoice.answers[i];
                    if (a.value !== '') {
                        if (a.correct){
                            scope.question.object.answer.multipleChoice.order.push(0);
                            answer.multipleChoice.correct = a.value;
                        } else {
                            scope.question.object.answer.multipleChoice.order.push(order + 1);
                            answer.multipleChoice.incorrect.push(a.value);
                            order++;
                        }
                    }
                }
            };

            scope.cancelEditing = scope.ngCancel;
            scope.remove = scope.ngRemove;

            element.find('textarea[name=question]').on('tbwchange', function () {
                if ($(this).trumbowyg('html')) {
                    scope.question.object.question = $(this).trumbowyg('html');
                }
            }).on('tbwpaste', function () {
                if ($(this).trumbowyg('html')) {
                    scope.question.object.question = $(this).trumbowyg('html');
                }
            });
            element.find('textarea[name=open_answer]').on('tbwchange', function () {
                if ($(this).trumbowyg('html')) {
                    scope.question.object.answer.open = $(this).trumbowyg('html');
                }
            }).on('tbwpaste', function () {
                if ($(this).trumbowyg('html')) {
                    scope.question.object.answer.open = $(this).trumbowyg('html');
                }
            });

        }
    };
});
