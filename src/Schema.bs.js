// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");

function field($staropt$star, $staropt$star$1, resolve, name, typ) {
  var description = $staropt$star !== undefined ? Js_primitive.valFromOption($staropt$star) : undefined;
  var deprecated = $staropt$star$1 !== undefined ? $staropt$star$1 : /* NotDeprecated */0;
  return /* Field */Block.simpleVariant("Field", [/* record */Block.record([
                "name",
                "typ",
                "resolve",
                "description",
                "deprecated"
              ], [
                name,
                typ,
                resolve,
                description,
                deprecated
              ])]);
}

function scalar($staropt$star, parse, serialize, name) {
  var description = $staropt$star !== undefined ? Js_primitive.valFromOption($staropt$star) : undefined;
  return /* Scalar */Block.variant("Scalar", 0, [/* record */Block.record([
                "name",
                "description",
                "parse",
                "serialize"
              ], [
                name,
                description,
                parse,
                serialize
              ])]);
}

function obj($staropt$star, $staropt$star$1, fields, name) {
  var description = $staropt$star !== undefined ? Js_primitive.valFromOption($staropt$star) : undefined;
  var $$implements = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
  var self = [];
  Caml_obj.caml_update_dummy(self, /* Object */Block.variant("Object", 2, [/* record */Block.record([
              "name",
              "description",
              "fields",
              "implements"
            ], [
              name,
              description,
              Block.__(246, [(function () {
                      return Curry._1(fields, self);
                    })]),
              $$implements
            ])]));
  return self;
}

var string = scalar(undefined, (function (input) {
        if (typeof input === "number" || input[0] !== -976970511) {
          return Pervasives.failwith("Not a string");
        } else {
          return input[1];
        }
      }), (function (str) {
        return /* `String */Block.polyVar("String", [
                  -976970511,
                  str
                ]);
      }), "String");

var $$int = scalar(undefined, (function (input) {
        if (typeof input === "number" || input[0] !== 3654863) {
          return Pervasives.failwith("Not an integer");
        } else {
          return input[1];
        }
      }), (function ($$int) {
        return /* `Int */Block.polyVar("Int", [
                  3654863,
                  $$int
                ]);
      }), "Int");

var $$float = scalar(undefined, (function (input) {
        if (typeof input === "number" || input[0] !== 365180284) {
          return Pervasives.failwith("Not a float");
        } else {
          return input[1];
        }
      }), (function ($$float) {
        return /* `Float */Block.polyVar("Float", [
                  365180284,
                  $$float
                ]);
      }), "Float");

var $$boolean = /* Scalar */Block.variant("Scalar", 0, [/* record */Block.record([
        "name",
        "description",
        "parse",
        "serialize"
      ], [
        "Boolean",
        undefined,
        (function (input) {
            if (typeof input === "number" || input[0] !== -883944824) {
              return Pervasives.failwith("Not a boolean");
            } else {
              return input[1];
            }
          }),
        (function (bool) {
            return /* `Boolean */Block.polyVar("Boolean", [
                      -883944824,
                      bool
                    ]);
          })
      ])]);

var a = /* "d" */100;

var b = 6;

exports.field = field;
exports.scalar = scalar;
exports.obj = obj;
exports.string = string;
exports.$$int = $$int;
exports.$$float = $$float;
exports.$$boolean = $$boolean;
exports.a = a;
exports.b = b;
/* string Not a pure module */
