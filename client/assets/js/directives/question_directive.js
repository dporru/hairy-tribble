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
                    scope.multipleChoice = {correct: '', incorrect: [], order: []};
                }else{
                    if (typeof scope.question.object.answer.multipleChoice !== 'undefined'){
                        scope.questionType = 'multipleChoice';
                        scope.multipleChoice.correct = scope.question.object.answer.multipleChoice.correct;
                        scope.multipleChoice.order = scope.question.object.answer.multipleChoice.order;
                        if (scope.question.object.answer.multipleChoice.incorrect.length) {
                            for (var i=0;i<scope.question.object.answer.multipleChoice.incorrect.length;i++) {
                                scope.multipleChoice.incorrect.push({value: scope.question.object.answer.multipleChoice.incorrect[i]});
                            }
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
                classes = 'glyphicon glyphicon-remove';
                if (answer.value === '') {
                    classes = classes + ' text-muted';
                }else{
                    classes = classes + ' text-danger';
                }
                return classes;
            };

            scope.save = function() {
                if (scope.questionType === 'open') {
                    delete(scope.question.object.answer.multipleChoice);
                }else{
                    delete(scope.question.object.answer.open);
                    scope.question.object.answer.multipleChoice.order = [];
                }
                scope.ngSubmit(scope.question);
            };

            scope.updateMultipleAnswers = function() {
                if (typeof scope.multipleChoice.incorrect === 'undefined' || !scope.multipleChoice.incorrect.length) {
                    scope.multipleChoice.incorrect = [{value: ''}];
                }else if (scope.multipleChoice.incorrect[scope.multipleChoice.incorrect.length-1].value !== ''){
                    scope.multipleChoice.incorrect.push({value: ''});
                }
                var answer = scope.question.object.answer;
                if (typeof answer.multipleChoice === 'undefined') {
                    answer.multipleChoice = {};
                }
                answer.multipleChoice.correct = scope.multipleChoice.correct;
                answer.multipleChoice.incorrect = [];
                for (var i=0;i<scope.multipleChoice.incorrect.length;i++) {
                    if (scope.multipleChoice.incorrect[i].value !== '') {
                        answer.multipleChoice.incorrect.push(scope.multipleChoice.incorrect[i].value);
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
