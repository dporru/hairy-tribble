angular.module('ph').controller('QuestionController', ['$modalInstance', '$modal', 'Question', 'question', function ($modalInstance, $modal, Question, question) {

    questionCtrl = this;
    questionCtrl.question = question;

    questionCtrl.save = function (question) {
        Question.save(question).then(function(){
            $modalInstance.close();
        });
    };

    questionCtrl.cancel = function () {
        // Reload to dismiss changes.
        Question.load();
        $modalInstance.dismiss('cancel');
    };

    questionCtrl.getTitle = function() {
        return Question.getQuestionTitle(questionCtrl.question);
    };
}]);