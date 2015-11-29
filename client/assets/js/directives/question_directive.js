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
                    scope.question.object = {answer: {open: ''}, question: '', title: {generated: true, titleText: ''}};
                    scope.question.labels = [];
                    scope.multipleChoice = {choices: [[true, '']]};
                }else{
                    if (typeof scope.question.object.answer.multipleChoice !== 'undefined'){
                        scope.questionType = 'multipleChoice';
                        scope.multipleChoice = scope.question.object.answer.multipleChoice
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
                        removeformatPasted: true,
                        autogrow: true,
                        btns: ['bold','italic','underline','|','btnGrp-lists','|','upload'],
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
                if (answer[0]) {
                    classes = classes + ' glyphicon-ok';
                    filledClass = ' text-success';
                } else {
                    classes = classes + ' glyphicon-remove';
                }
                if (answer[1] === '') {
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
                if (typeof scope.multipleChoice.choices === 'undefined' || !scope.multipleChoice.choices.length) {
                    scope.multipleChoice.choices = [[true, '']];
                } else if (scope.multipleChoice.choices[scope.multipleChoice.choices.length-1][1] !== ''){
                    scope.multipleChoice.choices.push([false, '']);
                } else if (scope.multipleChoice.choices[scope.multipleChoice.choices.length-2][1] === '') {
                    scope.multipleChoice.choices.pop(scope.multipleChoice.choices.length-1);
                }

                scope.question.object.answer.multipleChoice = {choices: []};
                scope.question.object.answer.multipleChoice.order = [];

                for (var i in scope.multipleChoice.choices) {
                    if (scope.multipleChoice.choices[i][1] !== '') {
                        scope.question.object.answer.multipleChoice.choices.push(scope.multipleChoice.choices[i]);
                        scope.question.object.answer.multipleChoice.order.push(parseInt(i, 10));
                    }
                }
            };

            scope.switchCorrectAnswer = function(answer) {
                for (var i in scope.multipleChoice.choices) {
                    scope.multipleChoice.choices[i][0] = false;
                }
                answer[0] = true;
                scope.updateMultipleAnswers();
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
