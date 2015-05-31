angular.module('ph').controller('TestListController', function ($modalInstance, Test, context) {

    test_list = this;

    test_list.save = function () {
        newTest = {
            'name': test_list.newTestName,
            'questions': []
        }
        Test.createTest(newTest).then(function() {
            test_list.newTestName = '';

            // If this is the first test, close the modal
            // and set this test as current.
            if (test_list.getTests().length === 1) {
                test_list.setCurrentTest(test_list.getTests()[0].id);
                test_list.cancel();
            }
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