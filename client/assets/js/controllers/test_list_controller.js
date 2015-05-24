angular.module('ph').controller('TestListController', function ($modalInstance, Test, context) {

    test_list = this;

    test_list.save = function () {
        newTest = {
            'name': test_list.newTestName,
            'questions': []
        }
        Test.createTest(newTest).then(function() {
            test_list.newTestName = '';
        });
    };

    test_list.getTests = function() {
        return Test.getList();
    };

    test_list.isCurrentTest = function(testId) {
        return Test.getCurrentTest().id === testId;
    };

    test_list.setCurrentTest = function(testId) {
        Test.setCurrentTest(testId);
        test_list.cancel();
    };

    test_list.cancel = function () {
        $modalInstance.dismiss('cancel');
    };
});