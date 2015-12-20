"use strict;"

// module VideoDOM

exports.play = function(id) {
    return function() {
        document.getElementById(id).play();
        return {};
    };
};

exports.pause = function(id) {
    return function() {
        document.getElementById(id).pause();
        return {};
    };
};

exports.setCurrentTime_ = function (id) {
    return function(time) {
        return function() {
            document.getElementById(id).currentTime = time;
            return {};
        };
    };
};
