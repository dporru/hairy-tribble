angular.module('ph').controller('AlertCtrl', ['$scope', 'Alert', function ($scope, Alert) {
    $scope.alerts = Alert.getAlerts();

    $scope.closeAlert = function(id) {
        Alert.remove(id);
    };
}]);