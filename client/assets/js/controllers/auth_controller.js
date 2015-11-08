angular.module('ph').controller('AuthCtrl', ['$scope', 'Auth', function ($scope, Auth) {

    $scope.loginNeeded = function() {
        return Auth.getLoginNeeded();
    };
    $scope.loginRedirectUrl = function() {
        return Auth.getLoginRedirectUrl();
    };

}]);
