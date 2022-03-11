"use strict";

exports.touches = function (e) {
  return e.touches;
};

exports.targetTouches = function (e) {
  return e.targetTouches;
};

exports.changedTouches = function (e) {
  return e.changedTouches;
};

exports.altKey = function (e) {
  return e.altKey;
};

exports.metaKey = function (e) {
  return e.metaKey;
};

exports.ctrlKey = function (e) {
  return e.ctrlKey;
};

exports.shiftKey = function (e) {
  return e.shiftKey;
};
