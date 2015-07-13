angular.module('ph').controller('QuestionListController', ['$http', 'Question', 'Test', '$filter', '$modal', function($http, Question, Test, $filter, $modal){
    var questionList = this;

    questionList.pageNumber = 1;
    questionList.itemsPerPage = 10;
    questionList.getNumberOfQuestions = function() {
        return $filter('filter')(questionList.getQuestions(), questionList.query).length;
    };
    questionList.pageChangeHandler = function(newPageNumber) {
        questionList.pageNumber = newPageNumber;
    };

    questionList.getQuestions = Question.getList;

    questionList.addToCurrentTest = function(questionId) {
        Test.addQuestionToCurrentTest(questionId);
    };

    questionList.isQuestionInCurrentTest = function(questionId) {
        return Test.isQuestionInCurrentTest(questionId);
    };

    questionList.getQuestionClass = function(questionId) {
        if (Test.isQuestionInCurrentTest(questionId)) {
            return 'text-muted';
        }else{
            return '';
        }
    };

    questionList.openQuestionModal = function(question) {
        var modalInstance = $modal.open({
            templateUrl: 'question_modal.html',
            controller: 'QuestionController as questionCtrl',
            resolve: {question: function(){return question;}}
        });
    };

    questionList.getQuestionTitle = function(question) {
        return Question.getQuestionTitle(question);
    };

    Question.load();
}]);
