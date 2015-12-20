"use strict;"

// module Definitions

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
