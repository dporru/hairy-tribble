angular.module('ph').controller('RemoveConfirmationController', function ($modalInstance, title, content) {

    removeCtrl = this;

    removeCtrl.title = title;
    removeCtrl.content = content;    

    removeCtrl.cancel = function () {
        $modalInstance.dismiss('cancel');
    };

    removeCtrl.remove = function(){
        $modalInstance.close(true);
    };
});