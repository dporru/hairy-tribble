angular.module('ph').controller('RemoveConfirmationController', ['$modalInstance', 'title', 'content', function ($modalInstance, title, content) {

    removeCtrl = this;

    removeCtrl.title = title;
    removeCtrl.content = content;    

    removeCtrl.cancel = function () {
        $modalInstance.dismiss('cancel');
    };

    removeCtrl.remove = function(){
        $modalInstance.close(true);
    };
}]);