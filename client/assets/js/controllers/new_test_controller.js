angular.module('ph').controller('NewTestController', function ($modalInstance, context) {

    nt = this;

    nt.save = function () {
        $modalInstance.close(nt.newTestName);
    };

    nt.cancel = function () {
        $modalInstance.dismiss('cancel');
    };
});