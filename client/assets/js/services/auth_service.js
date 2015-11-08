angular.module('ph').factory('Auth', function() {
    var _loginNeeded = false;
    var _loginRedirectUrl = '';

    var methods = {
    
        setLoginNeeded: function(redirectUrl) {
            _loginNeeded = true;
            _loginRedirectUrl = redirectUrl;
        },
        getLoginNeeded: function() {return _loginNeeded;},
        getLoginRedirectUrl: function() {
            return _loginRedirectUrl;}
    };

    return methods;

});
