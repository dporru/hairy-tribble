angular.module('ph').factory('Alert', ['$timeout', function($timeout){
    var alertCount = 0;
    var alerts = {};
    var timeouts = {};

    var methods = {
        getAlerts: function() {
            return alerts;
        },
        remove: function(id) {
            $timeout.cancel(timeouts[id]);
            delete alerts[id];
            delete timeouts[id];
        },
        add: function(msg, type) {
            if (typeof type == 'undefined') {
                type = 'warning';
            }
            alerts[alertCount] = {msg: msg, type: type, id: alertCount};

            timeouts[alertCount] = $timeout((function(id) {
                return function() {
                    methods.remove(id);
                };
            })(alertCount), 5000);

            alertCount++;
        }
    };

    return methods;
}]);