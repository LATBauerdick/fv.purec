"use strict";

exports.fromCharArray = function (a) {
  return a.join("");
};

exports.length = function (s) {
  return s.length;
};

exports.toCharArray = function (s) {
  return s.split("");
};

exports.singleton = function (c) {
  return c;
};

exports.countPrefix = function (p) {
  return function (s) {
    var i = 0;
    while (i < s.length && p(s.charAt(i))) i++;
    return i;
  };
};

exports.drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};

exports.replace = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(s1, s2);
    };
  };
};
