angular.module('ph').controller('QuestionListController', function($http, Question, $filter){
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

    Question.load();
});
