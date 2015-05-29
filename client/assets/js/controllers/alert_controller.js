angular.module('ph').controller('AlertCtrl', function ($scope, Alert) {
    $scope.alerts = Alert.getAlerts();

    $scope.closeAlert = function(id) {
        Alert.remove(id);
    };
});