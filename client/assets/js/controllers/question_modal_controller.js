angular.module('ph').controller('QuestionController', ['$modalInstance', '$modal', 'Question', 'question', function ($modalInstance, $modal, Question, question) {

    questionCtrl = this;
    questionCtrl.question = question;

    questionCtrl.save = function (question) {
        Question.save(question).then(function(){
            $modalInstance.close();
        });
    };

    questionCtrl.removeQuestion = function(question) {
        var modalInstance = $modal.open({
            templateUrl: 'remove_confirmation.html',
            controller: 'RemoveConfirmationController as removeCtrl',
            resolve: {
                title: function(){return 'Verwijder ' + question.object.question + '?';},
                content: function(){return 'Weet je zeker dat je ' + question.object.question + ' wilt verwijderen?';}
            },
            keyboard: true
        });

        modalInstance.result.then(function (remove) {
            if (remove) {
                Question.remove(question.id).then(function(){
                    $modalInstance.close();
                });
            }
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
