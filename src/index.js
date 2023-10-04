// output/Data.Functor/foreign.js
var arrayMap = function(f) {
  return function(arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

// output/Control.Semigroupoid/index.js
var semigroupoidFn = {
  compose: function(f) {
    return function(g) {
      return function(x) {
        return f(g(x));
      };
    };
  }
};

// output/Control.Category/index.js
var identity = function(dict) {
  return dict.identity;
};
var categoryFn = {
  identity: function(x) {
    return x;
  },
  Semigroupoid0: function() {
    return semigroupoidFn;
  }
};

// output/Data.Boolean/index.js
var otherwise = true;

// output/Data.Function/index.js
var flip = function(f) {
  return function(b) {
    return function(a) {
      return f(a)(b);
    };
  };
};
var $$const = function(a) {
  return function(v) {
    return a;
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var $$void = function(dictFunctor) {
  return map(dictFunctor)($$const(unit));
};
var voidLeft = function(dictFunctor) {
  var map15 = map(dictFunctor);
  return function(f) {
    return function(x) {
      return map15($$const(x))(f);
    };
  };
};
var voidRight = function(dictFunctor) {
  var map15 = map(dictFunctor);
  return function(x) {
    return map15($$const(x));
  };
};
var functorArray = {
  map: arrayMap
};

// output/Data.Semigroup/foreign.js
var concatArray = function(xs) {
  return function(ys) {
    if (xs.length === 0)
      return ys;
    if (ys.length === 0)
      return xs;
    return xs.concat(ys);
  };
};

// output/Data.Semigroup/index.js
var semigroupArray = {
  append: concatArray
};
var append = function(dict) {
  return dict.append;
};

// output/Control.Alt/index.js
var alt = function(dict) {
  return dict.alt;
};

// output/Control.Apply/index.js
var identity2 = /* @__PURE__ */ identity(categoryFn);
var apply = function(dict) {
  return dict.apply;
};
var applyFirst = function(dictApply) {
  var apply1 = apply(dictApply);
  var map15 = map(dictApply.Functor0());
  return function(a) {
    return function(b) {
      return apply1(map15($$const)(a))(b);
    };
  };
};
var applySecond = function(dictApply) {
  var apply1 = apply(dictApply);
  var map15 = map(dictApply.Functor0());
  return function(a) {
    return function(b) {
      return apply1(map15($$const(identity2))(a))(b);
    };
  };
};
var lift2 = function(dictApply) {
  var apply1 = apply(dictApply);
  var map15 = map(dictApply.Functor0());
  return function(f) {
    return function(a) {
      return function(b) {
        return apply1(map15(f)(a))(b);
      };
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};
var when = function(dictApplicative) {
  var pure15 = pure(dictApplicative);
  return function(v) {
    return function(v1) {
      if (v) {
        return v1;
      }
      ;
      if (!v) {
        return pure15(unit);
      }
      ;
      throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};
var liftA1 = function(dictApplicative) {
  var apply2 = apply(dictApplicative.Apply0());
  var pure15 = pure(dictApplicative);
  return function(f) {
    return function(a) {
      return apply2(pure15(f))(a);
    };
  };
};

// output/Data.Bounded/foreign.js
var topInt = 2147483647;
var bottomInt = -2147483648;
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq4) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq4 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;
var ordStringImpl = unsafeCompareImpl;
var ordCharImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqBooleanImpl = refEq;
var eqIntImpl = refEq;
var eqNumberImpl = refEq;
var eqCharImpl = refEq;
var eqStringImpl = refEq;
var eqArrayImpl = function(f) {
  return function(xs) {
    return function(ys) {
      if (xs.length !== ys.length)
        return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i]))
          return false;
      }
      return true;
    };
  };
};

// output/Data.Eq/index.js
var eqString = {
  eq: eqStringImpl
};
var eqNumber = {
  eq: eqNumberImpl
};
var eqInt = {
  eq: eqIntImpl
};
var eqChar = {
  eq: eqCharImpl
};
var eqBoolean = {
  eq: eqBooleanImpl
};
var eq = function(dict) {
  return dict.eq;
};
var eq2 = /* @__PURE__ */ eq(eqBoolean);
var eqArray = function(dictEq) {
  return {
    eq: eqArrayImpl(eq(dictEq))
  };
};
var notEq = function(dictEq) {
  var eq32 = eq(dictEq);
  return function(x) {
    return function(y) {
      return eq2(eq32(x)(y))(false);
    };
  };
};

// output/Data.Ordering/index.js
var LT = /* @__PURE__ */ function() {
  function LT2() {
  }
  ;
  LT2.value = new LT2();
  return LT2;
}();
var GT = /* @__PURE__ */ function() {
  function GT2() {
  }
  ;
  GT2.value = new GT2();
  return GT2;
}();
var EQ = /* @__PURE__ */ function() {
  function EQ2() {
  }
  ;
  EQ2.value = new EQ2();
  return EQ2;
}();

// output/Data.Ring/foreign.js
var intSub = function(x) {
  return function(y) {
    return x - y | 0;
  };
};

// output/Data.Semiring/foreign.js
var intAdd = function(x) {
  return function(y) {
    return x + y | 0;
  };
};
var intMul = function(x) {
  return function(y) {
    return x * y | 0;
  };
};

// output/Data.Semiring/index.js
var zero = function(dict) {
  return dict.zero;
};
var semiringInt = {
  add: intAdd,
  zero: 0,
  mul: intMul,
  one: 1
};
var one = function(dict) {
  return dict.one;
};
var mul = function(dict) {
  return dict.mul;
};
var add = function(dict) {
  return dict.add;
};

// output/Data.Ring/index.js
var sub = function(dict) {
  return dict.sub;
};
var ringInt = {
  sub: intSub,
  Semiring0: function() {
    return semiringInt;
  }
};
var negate = function(dictRing) {
  var sub12 = sub(dictRing);
  var zero2 = zero(dictRing.Semiring0());
  return function(a) {
    return sub12(zero2)(a);
  };
};

// output/Data.Ord/index.js
var ordString = /* @__PURE__ */ function() {
  return {
    compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqString;
    }
  };
}();
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var ordChar = /* @__PURE__ */ function() {
  return {
    compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqChar;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};
var greaterThan = function(dictOrd) {
  var compare32 = compare(dictOrd);
  return function(a1) {
    return function(a2) {
      var v = compare32(a1)(a2);
      if (v instanceof GT) {
        return true;
      }
      ;
      return false;
    };
  };
};
var greaterThanOrEq = function(dictOrd) {
  var compare32 = compare(dictOrd);
  return function(a1) {
    return function(a2) {
      var v = compare32(a1)(a2);
      if (v instanceof LT) {
        return false;
      }
      ;
      return true;
    };
  };
};
var lessThan = function(dictOrd) {
  var compare32 = compare(dictOrd);
  return function(a1) {
    return function(a2) {
      var v = compare32(a1)(a2);
      if (v instanceof LT) {
        return true;
      }
      ;
      return false;
    };
  };
};
var signum = function(dictOrd) {
  var lessThan1 = lessThan(dictOrd);
  var greaterThan1 = greaterThan(dictOrd);
  return function(dictRing) {
    var Semiring0 = dictRing.Semiring0();
    var zero2 = zero(Semiring0);
    var negate1 = negate(dictRing);
    var one2 = one(Semiring0);
    return function(x) {
      var $89 = lessThan1(x)(zero2);
      if ($89) {
        return negate1(one2);
      }
      ;
      var $90 = greaterThan1(x)(zero2);
      if ($90) {
        return one2;
      }
      ;
      return x;
    };
  };
};
var abs = function(dictOrd) {
  var greaterThanOrEq1 = greaterThanOrEq(dictOrd);
  return function(dictRing) {
    var zero2 = zero(dictRing.Semiring0());
    var negate1 = negate(dictRing);
    return function(x) {
      var $99 = greaterThanOrEq1(x)(zero2);
      if ($99) {
        return x;
      }
      ;
      return negate1(x);
    };
  };
};

// output/Data.Bounded/index.js
var top = function(dict) {
  return dict.top;
};
var boundedInt = {
  top: topInt,
  bottom: bottomInt,
  Ord0: function() {
    return ordInt;
  }
};
var boundedChar = {
  top: topChar,
  bottom: bottomChar,
  Ord0: function() {
    return ordChar;
  }
};
var bottom = function(dict) {
  return dict.bottom;
};

// output/Data.Show/foreign.js
var showIntImpl = function(n) {
  return n.toString();
};
var showNumberImpl = function(n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};
var showCharImpl = function(c) {
  var code = c.charCodeAt(0);
  if (code < 32 || code === 127) {
    switch (c) {
      case "\x07":
        return "'\\a'";
      case "\b":
        return "'\\b'";
      case "\f":
        return "'\\f'";
      case "\n":
        return "'\\n'";
      case "\r":
        return "'\\r'";
      case "	":
        return "'\\t'";
      case "\v":
        return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};
var showStringImpl = function(s) {
  var l = s.length;
  return '"' + s.replace(
    /[\0-\x1F\x7F"\\]/g,
    // eslint-disable-line no-control-regex
    function(c, i) {
      switch (c) {
        case '"':
        case "\\":
          return "\\" + c;
        case "\x07":
          return "\\a";
        case "\b":
          return "\\b";
        case "\f":
          return "\\f";
        case "\n":
          return "\\n";
        case "\r":
          return "\\r";
        case "	":
          return "\\t";
        case "\v":
          return "\\v";
      }
      var k = i + 1;
      var empty4 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty4;
    }
  ) + '"';
};
var showArrayImpl = function(f) {
  return function(xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

// output/Data.Show/index.js
var showString = {
  show: showStringImpl
};
var showNumber = {
  show: showNumberImpl
};
var showInt = {
  show: showIntImpl
};
var showChar = {
  show: showCharImpl
};
var showBoolean = {
  show: function(v) {
    if (v) {
      return "true";
    }
    ;
    if (!v) {
      return "false";
    }
    ;
    throw new Error("Failed pattern match at Data.Show (line 29, column 1 - line 31, column 23): " + [v.constructor.name]);
  }
};
var show = function(dict) {
  return dict.show;
};
var showArray = function(dictShow) {
  return {
    show: showArrayImpl(show(dictShow))
  };
};

// output/Data.Maybe/index.js
var identity3 = /* @__PURE__ */ identity(categoryFn);
var Nothing = /* @__PURE__ */ function() {
  function Nothing2() {
  }
  ;
  Nothing2.value = new Nothing2();
  return Nothing2;
}();
var Just = /* @__PURE__ */ function() {
  function Just2(value0) {
    this.value0 = value0;
  }
  ;
  Just2.create = function(value0) {
    return new Just2(value0);
  };
  return Just2;
}();
var maybe = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Nothing) {
        return v;
      }
      ;
      if (v2 instanceof Just) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
var functorMaybe = {
  map: function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }
      ;
      return Nothing.value;
    };
  }
};
var map2 = /* @__PURE__ */ map(functorMaybe);
var fromMaybe = function(a) {
  return maybe(a)(identity3);
};
var fromJust = function() {
  return function(v) {
    if (v instanceof Just) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
  };
};
var applyMaybe = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return map2(v.value0)(v1);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorMaybe;
  }
};
var bindMaybe = {
  bind: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Apply0: function() {
    return applyMaybe;
  }
};
var applicativeMaybe = /* @__PURE__ */ function() {
  return {
    pure: Just.create,
    Apply0: function() {
      return applyMaybe;
    }
  };
}();

// output/Data.DateTime/foreign.js
var createUTC = function(y, mo, d, h, m, s, ms) {
  var date2 = new Date(Date.UTC(y, mo, d, h, m, s, ms));
  if (y >= 0 && y < 100) {
    date2.setUTCFullYear(y);
  }
  return date2.getTime();
};
function calcDiff(rec1, rec2) {
  var msUTC1 = createUTC(rec1.year, rec1.month - 1, rec1.day, rec1.hour, rec1.minute, rec1.second, rec1.millisecond);
  var msUTC2 = createUTC(rec2.year, rec2.month - 1, rec2.day, rec2.hour, rec2.minute, rec2.second, rec2.millisecond);
  return msUTC1 - msUTC2;
}

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
};
var bind = function(dict) {
  return dict.bind;
};
var bindFlipped = function(dictBind) {
  return flip(bind(dictBind));
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
  }
};

// output/Data.Date/foreign.js
var createDate = function(y, m, d) {
  var date2 = new Date(Date.UTC(y, m, d));
  if (y >= 0 && y < 100) {
    date2.setUTCFullYear(y);
  }
  return date2;
};
function canonicalDateImpl(ctor, y, m, d) {
  var date2 = createDate(y, m - 1, d);
  return ctor(date2.getUTCFullYear())(date2.getUTCMonth() + 1)(date2.getUTCDate());
}

// output/Data.Enum/foreign.js
function toCharCode(c) {
  return c.charCodeAt(0);
}
function fromCharCode(c) {
  return String.fromCharCode(c);
}

// output/Control.Plus/index.js
var empty = function(dict) {
  return dict.empty;
};

// output/Data.Either/index.js
var Left = /* @__PURE__ */ function() {
  function Left2(value0) {
    this.value0 = value0;
  }
  ;
  Left2.create = function(value0) {
    return new Left2(value0);
  };
  return Left2;
}();
var Right = /* @__PURE__ */ function() {
  function Right2(value0) {
    this.value0 = value0;
  }
  ;
  Right2.create = function(value0) {
    return new Right2(value0);
  };
  return Right2;
}();
var functorEither = {
  map: function(f) {
    return function(m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }
      ;
      if (m instanceof Right) {
        return new Right(f(m.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
var map3 = /* @__PURE__ */ map(functorEither);
var either = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Left) {
        return v(v2.value0);
      }
      ;
      if (v2 instanceof Right) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var applyEither = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return map3(v.value0)(v1);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorEither;
  }
};
var bindEither = {
  bind: /* @__PURE__ */ either(function(e) {
    return function(v) {
      return new Left(e);
    };
  })(function(a) {
    return function(f) {
      return f(a);
    };
  }),
  Apply0: function() {
    return applyEither;
  }
};
var applicativeEither = /* @__PURE__ */ function() {
  return {
    pure: Right.create,
    Apply0: function() {
      return applyEither;
    }
  };
}();
var monadEither = {
  Applicative0: function() {
    return applicativeEither;
  },
  Bind1: function() {
    return bindEither;
  }
};

// output/Control.Lazy/index.js
var $runtime_lazy = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var defer = function(dict) {
  return dict.defer;
};
var fix = function(dictLazy) {
  var defer1 = defer(dictLazy);
  return function(f) {
    var $lazy_go = $runtime_lazy("go", "Control.Lazy", function() {
      return defer1(function(v) {
        return f($lazy_go(25));
      });
    });
    var go = $lazy_go(25);
    return go;
  };
};

// output/Data.HeytingAlgebra/foreign.js
var boolConj = function(b1) {
  return function(b2) {
    return b1 && b2;
  };
};
var boolDisj = function(b1) {
  return function(b2) {
    return b1 || b2;
  };
};
var boolNot = function(b) {
  return !b;
};

// output/Data.HeytingAlgebra/index.js
var not = function(dict) {
  return dict.not;
};
var ff = function(dict) {
  return dict.ff;
};
var disj = function(dict) {
  return dict.disj;
};
var heytingAlgebraBoolean = {
  ff: false,
  tt: true,
  implies: function(a) {
    return function(b) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
  },
  conj: boolConj,
  disj: boolDisj,
  not: boolNot
};

// output/Data.EuclideanRing/foreign.js
var intDegree = function(x) {
  return Math.min(Math.abs(x), 2147483647);
};
var intDiv = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};
var intMod = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};

// output/Data.CommutativeRing/index.js
var commutativeRingInt = {
  Ring0: function() {
    return ringInt;
  }
};

// output/Data.EuclideanRing/index.js
var mod = function(dict) {
  return dict.mod;
};
var gcd = function(dictEq) {
  var eq4 = eq(dictEq);
  return function(dictEuclideanRing) {
    var zero2 = zero(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0());
    var mod1 = mod(dictEuclideanRing);
    return function(a) {
      return function(b) {
        var $24 = eq4(b)(zero2);
        if ($24) {
          return a;
        }
        ;
        return gcd(dictEq)(dictEuclideanRing)(b)(mod1(a)(b));
      };
    };
  };
};
var euclideanRingInt = {
  degree: intDegree,
  div: intDiv,
  mod: intMod,
  CommutativeRing0: function() {
    return commutativeRingInt;
  }
};
var div = function(dict) {
  return dict.div;
};

// output/Data.Monoid/index.js
var mempty = function(dict) {
  return dict.mempty;
};

// output/Data.Tuple/index.js
var Tuple = /* @__PURE__ */ function() {
  function Tuple2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Tuple2.create = function(value0) {
    return function(value1) {
      return new Tuple2(value0, value1);
    };
  };
  return Tuple2;
}();
var snd = function(v) {
  return v.value1;
};
var fst = function(v) {
  return v.value0;
};

// output/Data.Unfoldable/foreign.js
var unfoldrArrayImpl = function(isNothing2) {
  return function(fromJust8) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value = b;
            while (true) {
              var maybe2 = f(value);
              if (isNothing2(maybe2))
                return result;
              var tuple = fromJust8(maybe2);
              result.push(fst2(tuple));
              value = snd2(tuple);
            }
          };
        };
      };
    };
  };
};

// output/Data.Traversable/foreign.js
var traverseArrayImpl = function() {
  function array1(a) {
    return [a];
  }
  function array2(a) {
    return function(b) {
      return [a, b];
    };
  }
  function array3(a) {
    return function(b) {
      return function(c) {
        return [a, b, c];
      };
    };
  }
  function concat2(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply2) {
    return function(map15) {
      return function(pure15) {
        return function(f) {
          return function(array) {
            function go(bot, top3) {
              switch (top3 - bot) {
                case 0:
                  return pure15([]);
                case 1:
                  return map15(array1)(f(array[bot]));
                case 2:
                  return apply2(map15(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply2(apply2(map15(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                  return apply2(map15(concat2)(go(bot, pivot)))(go(pivot, top3));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output/Data.Bifunctor/index.js
var identity4 = /* @__PURE__ */ identity(categoryFn);
var bimap = function(dict) {
  return dict.bimap;
};
var lmap = function(dictBifunctor) {
  var bimap1 = bimap(dictBifunctor);
  return function(f) {
    return bimap1(f)(identity4);
  };
};
var bifunctorEither = {
  bimap: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return new Left(v(v2.value0));
        }
        ;
        if (v2 instanceof Right) {
          return new Right(v1(v2.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};

// output/Data.Monoid.Disj/index.js
var Disj = function(x) {
  return x;
};
var semigroupDisj = function(dictHeytingAlgebra) {
  var disj2 = disj(dictHeytingAlgebra);
  return {
    append: function(v) {
      return function(v1) {
        return disj2(v)(v1);
      };
    }
  };
};
var monoidDisj = function(dictHeytingAlgebra) {
  var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
  return {
    mempty: ff(dictHeytingAlgebra),
    Semigroup0: function() {
      return semigroupDisj1;
    }
  };
};

// output/Unsafe.Coerce/foreign.js
var unsafeCoerce2 = function(x) {
  return x;
};

// output/Safe.Coerce/index.js
var coerce = function() {
  return unsafeCoerce2;
};

// output/Data.Newtype/index.js
var coerce2 = /* @__PURE__ */ coerce();
var unwrap = function() {
  return coerce2;
};
var over = function() {
  return function() {
    return function(v) {
      return coerce2;
    };
  };
};
var alaF = function() {
  return function() {
    return function() {
      return function() {
        return function(v) {
          return coerce2;
        };
      };
    };
  };
};

// output/Data.Foldable/index.js
var alaF2 = /* @__PURE__ */ alaF()()()();
var foldr = function(dict) {
  return dict.foldr;
};
var traverse_ = function(dictApplicative) {
  var applySecond4 = applySecond(dictApplicative.Apply0());
  var pure15 = pure(dictApplicative);
  return function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(f) {
      return foldr22(function($454) {
        return applySecond4(f($454));
      })(pure15(unit));
    };
  };
};
var foldl = function(dict) {
  return dict.foldl;
};
var foldMapDefaultR = function(dictFoldable) {
  var foldr22 = foldr(dictFoldable);
  return function(dictMonoid) {
    var append4 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldr22(function(x) {
        return function(acc) {
          return append4(f(x))(acc);
        };
      })(mempty2);
    };
  };
};
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: function(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};
var foldMap = function(dict) {
  return dict.foldMap;
};
var any = function(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function(dictHeytingAlgebra) {
    return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
  };
};
var elem = function(dictFoldable) {
  var any1 = any(dictFoldable)(heytingAlgebraBoolean);
  return function(dictEq) {
    var $462 = eq(dictEq);
    return function($463) {
      return any1($462($463));
    };
  };
};

// output/Data.Identity/index.js
var Identity = function(x) {
  return x;
};
var functorIdentity = {
  map: function(f) {
    return function(m) {
      return f(m);
    };
  }
};
var applyIdentity = {
  apply: function(v) {
    return function(v1) {
      return v(v1);
    };
  },
  Functor0: function() {
    return functorIdentity;
  }
};
var bindIdentity = {
  bind: function(v) {
    return function(f) {
      return f(v);
    };
  },
  Apply0: function() {
    return applyIdentity;
  }
};
var applicativeIdentity = {
  pure: Identity,
  Apply0: function() {
    return applyIdentity;
  }
};
var monadIdentity = {
  Applicative0: function() {
    return applicativeIdentity;
  },
  Bind1: function() {
    return bindIdentity;
  }
};

// output/Data.Traversable/index.js
var identity5 = /* @__PURE__ */ identity(categoryFn);
var traverse = function(dict) {
  return dict.traverse;
};
var sequenceDefault = function(dictTraversable) {
  var traverse22 = traverse(dictTraversable);
  return function(dictApplicative) {
    return traverse22(dictApplicative)(identity5);
  };
};
var traversableArray = {
  traverse: function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
  },
  sequence: function(dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
  },
  Functor0: function() {
    return functorArray;
  },
  Foldable1: function() {
    return foldableArray;
  }
};
var sequence = function(dict) {
  return dict.sequence;
};

// output/Data.Unfoldable1/foreign.js
var unfoldr1ArrayImpl = function(isNothing2) {
  return function(fromJust8) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value = b;
            while (true) {
              var tuple = f(value);
              result.push(fst2(tuple));
              var maybe2 = snd2(tuple);
              if (isNothing2(maybe2))
                return result;
              value = fromJust8(maybe2);
            }
          };
        };
      };
    };
  };
};

// output/Data.Unfoldable1/index.js
var fromJust2 = /* @__PURE__ */ fromJust();
var unfoldable1Array = {
  unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
};

// output/Data.Unfoldable/index.js
var fromJust3 = /* @__PURE__ */ fromJust();
var unfoldr = function(dict) {
  return dict.unfoldr;
};
var unfoldableArray = {
  unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
  Unfoldable10: function() {
    return unfoldable1Array;
  }
};

// output/Data.Enum/index.js
var bottom1 = /* @__PURE__ */ bottom(boundedChar);
var top1 = /* @__PURE__ */ top(boundedChar);
var toEnum = function(dict) {
  return dict.toEnum;
};
var fromEnum = function(dict) {
  return dict.fromEnum;
};
var toEnumWithDefaults = function(dictBoundedEnum) {
  var toEnum1 = toEnum(dictBoundedEnum);
  var fromEnum12 = fromEnum(dictBoundedEnum);
  var bottom22 = bottom(dictBoundedEnum.Bounded0());
  return function(low) {
    return function(high) {
      return function(x) {
        var v = toEnum1(x);
        if (v instanceof Just) {
          return v.value0;
        }
        ;
        if (v instanceof Nothing) {
          var $140 = x < fromEnum12(bottom22);
          if ($140) {
            return low;
          }
          ;
          return high;
        }
        ;
        throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
      };
    };
  };
};
var defaultSucc = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) + 1 | 0);
    };
  };
};
var defaultPred = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) - 1 | 0);
    };
  };
};
var charToEnum = function(v) {
  if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
    return new Just(fromCharCode(v));
  }
  ;
  return Nothing.value;
};
var enumChar = {
  succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
  pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
  Ord0: function() {
    return ordChar;
  }
};
var boundedEnumChar = /* @__PURE__ */ function() {
  return {
    cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
    toEnum: charToEnum,
    fromEnum: toCharCode,
    Bounded0: function() {
      return boundedChar;
    },
    Enum1: function() {
      return enumChar;
    }
  };
}();

// output/Data.Date.Component/index.js
var $runtime_lazy2 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var January = /* @__PURE__ */ function() {
  function January2() {
  }
  ;
  January2.value = new January2();
  return January2;
}();
var February = /* @__PURE__ */ function() {
  function February2() {
  }
  ;
  February2.value = new February2();
  return February2;
}();
var March = /* @__PURE__ */ function() {
  function March2() {
  }
  ;
  March2.value = new March2();
  return March2;
}();
var April = /* @__PURE__ */ function() {
  function April2() {
  }
  ;
  April2.value = new April2();
  return April2;
}();
var May = /* @__PURE__ */ function() {
  function May2() {
  }
  ;
  May2.value = new May2();
  return May2;
}();
var June = /* @__PURE__ */ function() {
  function June2() {
  }
  ;
  June2.value = new June2();
  return June2;
}();
var July = /* @__PURE__ */ function() {
  function July2() {
  }
  ;
  July2.value = new July2();
  return July2;
}();
var August = /* @__PURE__ */ function() {
  function August2() {
  }
  ;
  August2.value = new August2();
  return August2;
}();
var September = /* @__PURE__ */ function() {
  function September2() {
  }
  ;
  September2.value = new September2();
  return September2;
}();
var October = /* @__PURE__ */ function() {
  function October2() {
  }
  ;
  October2.value = new October2();
  return October2;
}();
var November = /* @__PURE__ */ function() {
  function November2() {
  }
  ;
  November2.value = new November2();
  return November2;
}();
var December = /* @__PURE__ */ function() {
  function December2() {
  }
  ;
  December2.value = new December2();
  return December2;
}();
var ordYear = ordInt;
var ordDay = ordInt;
var eqMonth = {
  eq: function(x) {
    return function(y) {
      if (x instanceof January && y instanceof January) {
        return true;
      }
      ;
      if (x instanceof February && y instanceof February) {
        return true;
      }
      ;
      if (x instanceof March && y instanceof March) {
        return true;
      }
      ;
      if (x instanceof April && y instanceof April) {
        return true;
      }
      ;
      if (x instanceof May && y instanceof May) {
        return true;
      }
      ;
      if (x instanceof June && y instanceof June) {
        return true;
      }
      ;
      if (x instanceof July && y instanceof July) {
        return true;
      }
      ;
      if (x instanceof August && y instanceof August) {
        return true;
      }
      ;
      if (x instanceof September && y instanceof September) {
        return true;
      }
      ;
      if (x instanceof October && y instanceof October) {
        return true;
      }
      ;
      if (x instanceof November && y instanceof November) {
        return true;
      }
      ;
      if (x instanceof December && y instanceof December) {
        return true;
      }
      ;
      return false;
    };
  }
};
var ordMonth = {
  compare: function(x) {
    return function(y) {
      if (x instanceof January && y instanceof January) {
        return EQ.value;
      }
      ;
      if (x instanceof January) {
        return LT.value;
      }
      ;
      if (y instanceof January) {
        return GT.value;
      }
      ;
      if (x instanceof February && y instanceof February) {
        return EQ.value;
      }
      ;
      if (x instanceof February) {
        return LT.value;
      }
      ;
      if (y instanceof February) {
        return GT.value;
      }
      ;
      if (x instanceof March && y instanceof March) {
        return EQ.value;
      }
      ;
      if (x instanceof March) {
        return LT.value;
      }
      ;
      if (y instanceof March) {
        return GT.value;
      }
      ;
      if (x instanceof April && y instanceof April) {
        return EQ.value;
      }
      ;
      if (x instanceof April) {
        return LT.value;
      }
      ;
      if (y instanceof April) {
        return GT.value;
      }
      ;
      if (x instanceof May && y instanceof May) {
        return EQ.value;
      }
      ;
      if (x instanceof May) {
        return LT.value;
      }
      ;
      if (y instanceof May) {
        return GT.value;
      }
      ;
      if (x instanceof June && y instanceof June) {
        return EQ.value;
      }
      ;
      if (x instanceof June) {
        return LT.value;
      }
      ;
      if (y instanceof June) {
        return GT.value;
      }
      ;
      if (x instanceof July && y instanceof July) {
        return EQ.value;
      }
      ;
      if (x instanceof July) {
        return LT.value;
      }
      ;
      if (y instanceof July) {
        return GT.value;
      }
      ;
      if (x instanceof August && y instanceof August) {
        return EQ.value;
      }
      ;
      if (x instanceof August) {
        return LT.value;
      }
      ;
      if (y instanceof August) {
        return GT.value;
      }
      ;
      if (x instanceof September && y instanceof September) {
        return EQ.value;
      }
      ;
      if (x instanceof September) {
        return LT.value;
      }
      ;
      if (y instanceof September) {
        return GT.value;
      }
      ;
      if (x instanceof October && y instanceof October) {
        return EQ.value;
      }
      ;
      if (x instanceof October) {
        return LT.value;
      }
      ;
      if (y instanceof October) {
        return GT.value;
      }
      ;
      if (x instanceof November && y instanceof November) {
        return EQ.value;
      }
      ;
      if (x instanceof November) {
        return LT.value;
      }
      ;
      if (y instanceof November) {
        return GT.value;
      }
      ;
      if (x instanceof December && y instanceof December) {
        return EQ.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
    };
  },
  Eq0: function() {
    return eqMonth;
  }
};
var boundedYear = /* @__PURE__ */ function() {
  return {
    bottom: -271820 | 0,
    top: 275759,
    Ord0: function() {
      return ordYear;
    }
  };
}();
var boundedMonth = /* @__PURE__ */ function() {
  return {
    bottom: January.value,
    top: December.value,
    Ord0: function() {
      return ordMonth;
    }
  };
}();
var boundedEnumYear = {
  cardinality: 547580,
  toEnum: function(n) {
    if (n >= (-271820 | 0) && n <= 275759) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedYear;
  },
  Enum1: function() {
    return $lazy_enumYear(0);
  }
};
var $lazy_enumYear = /* @__PURE__ */ $runtime_lazy2("enumYear", "Data.Date.Component", function() {
  return {
    succ: function() {
      var $55 = toEnum(boundedEnumYear);
      var $56 = fromEnum(boundedEnumYear);
      return function($57) {
        return $55(function(v) {
          return v + 1 | 0;
        }($56($57)));
      };
    }(),
    pred: function() {
      var $58 = toEnum(boundedEnumYear);
      var $59 = fromEnum(boundedEnumYear);
      return function($60) {
        return $58(function(v) {
          return v - 1 | 0;
        }($59($60)));
      };
    }(),
    Ord0: function() {
      return ordYear;
    }
  };
});
var boundedEnumMonth = {
  cardinality: 12,
  toEnum: function(v) {
    if (v === 1) {
      return new Just(January.value);
    }
    ;
    if (v === 2) {
      return new Just(February.value);
    }
    ;
    if (v === 3) {
      return new Just(March.value);
    }
    ;
    if (v === 4) {
      return new Just(April.value);
    }
    ;
    if (v === 5) {
      return new Just(May.value);
    }
    ;
    if (v === 6) {
      return new Just(June.value);
    }
    ;
    if (v === 7) {
      return new Just(July.value);
    }
    ;
    if (v === 8) {
      return new Just(August.value);
    }
    ;
    if (v === 9) {
      return new Just(September.value);
    }
    ;
    if (v === 10) {
      return new Just(October.value);
    }
    ;
    if (v === 11) {
      return new Just(November.value);
    }
    ;
    if (v === 12) {
      return new Just(December.value);
    }
    ;
    return Nothing.value;
  },
  fromEnum: function(v) {
    if (v instanceof January) {
      return 1;
    }
    ;
    if (v instanceof February) {
      return 2;
    }
    ;
    if (v instanceof March) {
      return 3;
    }
    ;
    if (v instanceof April) {
      return 4;
    }
    ;
    if (v instanceof May) {
      return 5;
    }
    ;
    if (v instanceof June) {
      return 6;
    }
    ;
    if (v instanceof July) {
      return 7;
    }
    ;
    if (v instanceof August) {
      return 8;
    }
    ;
    if (v instanceof September) {
      return 9;
    }
    ;
    if (v instanceof October) {
      return 10;
    }
    ;
    if (v instanceof November) {
      return 11;
    }
    ;
    if (v instanceof December) {
      return 12;
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [v.constructor.name]);
  },
  Bounded0: function() {
    return boundedMonth;
  },
  Enum1: function() {
    return $lazy_enumMonth(0);
  }
};
var $lazy_enumMonth = /* @__PURE__ */ $runtime_lazy2("enumMonth", "Data.Date.Component", function() {
  return {
    succ: function() {
      var $67 = toEnum(boundedEnumMonth);
      var $68 = fromEnum(boundedEnumMonth);
      return function($69) {
        return $67(function(v) {
          return v + 1 | 0;
        }($68($69)));
      };
    }(),
    pred: function() {
      var $70 = toEnum(boundedEnumMonth);
      var $71 = fromEnum(boundedEnumMonth);
      return function($72) {
        return $70(function(v) {
          return v - 1 | 0;
        }($71($72)));
      };
    }(),
    Ord0: function() {
      return ordMonth;
    }
  };
});
var boundedDay = {
  bottom: 1,
  top: 31,
  Ord0: function() {
    return ordDay;
  }
};
var boundedEnumDay = {
  cardinality: 31,
  toEnum: function(n) {
    if (n >= 1 && n <= 31) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedDay;
  },
  Enum1: function() {
    return $lazy_enumDay(0);
  }
};
var $lazy_enumDay = /* @__PURE__ */ $runtime_lazy2("enumDay", "Data.Date.Component", function() {
  return {
    succ: function() {
      var $73 = toEnum(boundedEnumDay);
      var $74 = fromEnum(boundedEnumDay);
      return function($75) {
        return $73(function(v) {
          return v + 1 | 0;
        }($74($75)));
      };
    }(),
    pred: function() {
      var $76 = toEnum(boundedEnumDay);
      var $77 = fromEnum(boundedEnumDay);
      return function($78) {
        return $76(function(v) {
          return v - 1 | 0;
        }($77($78)));
      };
    }(),
    Ord0: function() {
      return ordDay;
    }
  };
});

// output/Data.Int/foreign.js
var fromNumberImpl = function(just) {
  return function(nothing) {
    return function(n) {
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};
var toNumber = function(n) {
  return n;
};

// output/Data.Number/foreign.js
var isFiniteImpl = isFinite;
var floor = Math.floor;
var pow = function(n) {
  return function(p) {
    return Math.pow(n, p);
  };
};
var round = Math.round;
var sin = Math.sin;

// output/Data.Number/index.js
var pi = 3.141592653589793;

// output/Data.Int/index.js
var top2 = /* @__PURE__ */ top(boundedInt);
var bottom2 = /* @__PURE__ */ bottom(boundedInt);
var fromNumber = /* @__PURE__ */ function() {
  return fromNumberImpl(Just.create)(Nothing.value);
}();
var unsafeClamp = function(x) {
  if (!isFiniteImpl(x)) {
    return 0;
  }
  ;
  if (x >= toNumber(top2)) {
    return top2;
  }
  ;
  if (x <= toNumber(bottom2)) {
    return bottom2;
  }
  ;
  if (otherwise) {
    return fromMaybe(0)(fromNumber(x));
  }
  ;
  throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
};
var round2 = function($37) {
  return unsafeClamp(round($37));
};
var floor2 = function($39) {
  return unsafeClamp(floor($39));
};

// output/Data.Time.Duration/index.js
var over2 = /* @__PURE__ */ over()();
var identity6 = /* @__PURE__ */ identity(categoryFn);
var Seconds = function(x) {
  return x;
};
var Milliseconds = function(x) {
  return x;
};
var toDuration = function(dict) {
  return dict.toDuration;
};
var durationSeconds = {
  fromDuration: /* @__PURE__ */ over2(Seconds)(function(v) {
    return v * 1e3;
  }),
  toDuration: /* @__PURE__ */ over2(Milliseconds)(function(v) {
    return v / 1e3;
  })
};
var durationMilliseconds = {
  fromDuration: identity6,
  toDuration: identity6
};

// output/Data.Date/index.js
var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
var fromJust4 = /* @__PURE__ */ fromJust();
var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumMonth);
var $$Date = /* @__PURE__ */ function() {
  function $$Date2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  $$Date2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new $$Date2(value0, value1, value2);
      };
    };
  };
  return $$Date2;
}();
var year = function(v) {
  return v.value0;
};
var month = function(v) {
  return v.value1;
};
var day = function(v) {
  return v.value2;
};
var canonicalDate = function(y) {
  return function(m) {
    return function(d) {
      var mkDate = function(y$prime) {
        return function(m$prime) {
          return function(d$prime) {
            return new $$Date(y$prime, fromJust4(toEnum2(m$prime)), d$prime);
          };
        };
      };
      return canonicalDateImpl(mkDate, y, fromEnum2(m), d);
    };
  };
};

// output/Data.Time.Component/index.js
var $runtime_lazy3 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var ordSecond = ordInt;
var ordMinute = ordInt;
var ordMillisecond = ordInt;
var ordHour = ordInt;
var boundedSecond = {
  bottom: 0,
  top: 59,
  Ord0: function() {
    return ordSecond;
  }
};
var boundedMinute = {
  bottom: 0,
  top: 59,
  Ord0: function() {
    return ordMinute;
  }
};
var boundedMillisecond = {
  bottom: 0,
  top: 999,
  Ord0: function() {
    return ordMillisecond;
  }
};
var boundedHour = {
  bottom: 0,
  top: 23,
  Ord0: function() {
    return ordHour;
  }
};
var boundedEnumSecond = {
  cardinality: 60,
  toEnum: function(n) {
    if (n >= 0 && n <= 59) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 90, column 1 - line 95, column 26): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedSecond;
  },
  Enum1: function() {
    return $lazy_enumSecond(0);
  }
};
var $lazy_enumSecond = /* @__PURE__ */ $runtime_lazy3("enumSecond", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $36 = toEnum(boundedEnumSecond);
      var $37 = fromEnum(boundedEnumSecond);
      return function($38) {
        return $36(function(v) {
          return v + 1 | 0;
        }($37($38)));
      };
    }(),
    pred: function() {
      var $39 = toEnum(boundedEnumSecond);
      var $40 = fromEnum(boundedEnumSecond);
      return function($41) {
        return $39(function(v) {
          return v - 1 | 0;
        }($40($41)));
      };
    }(),
    Ord0: function() {
      return ordSecond;
    }
  };
});
var boundedEnumMinute = {
  cardinality: 60,
  toEnum: function(n) {
    if (n >= 0 && n <= 59) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 61, column 1 - line 66, column 26): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedMinute;
  },
  Enum1: function() {
    return $lazy_enumMinute(0);
  }
};
var $lazy_enumMinute = /* @__PURE__ */ $runtime_lazy3("enumMinute", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $42 = toEnum(boundedEnumMinute);
      var $43 = fromEnum(boundedEnumMinute);
      return function($44) {
        return $42(function(v) {
          return v + 1 | 0;
        }($43($44)));
      };
    }(),
    pred: function() {
      var $45 = toEnum(boundedEnumMinute);
      var $46 = fromEnum(boundedEnumMinute);
      return function($47) {
        return $45(function(v) {
          return v - 1 | 0;
        }($46($47)));
      };
    }(),
    Ord0: function() {
      return ordMinute;
    }
  };
});
var boundedEnumMillisecond = {
  cardinality: 1e3,
  toEnum: function(n) {
    if (n >= 0 && n <= 999) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 120, column 1 - line 125, column 31): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedMillisecond;
  },
  Enum1: function() {
    return $lazy_enumMillisecond(0);
  }
};
var $lazy_enumMillisecond = /* @__PURE__ */ $runtime_lazy3("enumMillisecond", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $48 = toEnum(boundedEnumMillisecond);
      var $49 = fromEnum(boundedEnumMillisecond);
      return function($50) {
        return $48(function(v) {
          return v + 1 | 0;
        }($49($50)));
      };
    }(),
    pred: function() {
      var $51 = toEnum(boundedEnumMillisecond);
      var $52 = fromEnum(boundedEnumMillisecond);
      return function($53) {
        return $51(function(v) {
          return v - 1 | 0;
        }($52($53)));
      };
    }(),
    Ord0: function() {
      return ordMillisecond;
    }
  };
});
var boundedEnumHour = {
  cardinality: 24,
  toEnum: function(n) {
    if (n >= 0 && n <= 23) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 32, column 1 - line 37, column 24): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedHour;
  },
  Enum1: function() {
    return $lazy_enumHour(0);
  }
};
var $lazy_enumHour = /* @__PURE__ */ $runtime_lazy3("enumHour", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $54 = toEnum(boundedEnumHour);
      var $55 = fromEnum(boundedEnumHour);
      return function($56) {
        return $54(function(v) {
          return v + 1 | 0;
        }($55($56)));
      };
    }(),
    pred: function() {
      var $57 = toEnum(boundedEnumHour);
      var $58 = fromEnum(boundedEnumHour);
      return function($59) {
        return $57(function(v) {
          return v - 1 | 0;
        }($58($59)));
      };
    }(),
    Ord0: function() {
      return ordHour;
    }
  };
});

// output/Data.Time/index.js
var Time = /* @__PURE__ */ function() {
  function Time2(value0, value1, value2, value3) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
  }
  ;
  Time2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return new Time2(value0, value1, value2, value3);
        };
      };
    };
  };
  return Time2;
}();
var second = function(v) {
  return v.value2;
};
var minute = function(v) {
  return v.value1;
};
var millisecond = function(v) {
  return v.value3;
};
var hour = function(v) {
  return v.value0;
};

// output/Data.DateTime/index.js
var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumYear);
var fromEnum1 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
var fromEnum22 = /* @__PURE__ */ fromEnum(boundedEnumDay);
var fromEnum32 = /* @__PURE__ */ fromEnum(boundedEnumHour);
var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumMinute);
var fromEnum5 = /* @__PURE__ */ fromEnum(boundedEnumSecond);
var fromEnum6 = /* @__PURE__ */ fromEnum(boundedEnumMillisecond);
var DateTime = /* @__PURE__ */ function() {
  function DateTime2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  DateTime2.create = function(value0) {
    return function(value1) {
      return new DateTime2(value0, value1);
    };
  };
  return DateTime2;
}();
var toRecord = function(v) {
  return {
    year: fromEnum3(year(v.value0)),
    month: fromEnum1(month(v.value0)),
    day: fromEnum22(day(v.value0)),
    hour: fromEnum32(hour(v.value1)),
    minute: fromEnum4(minute(v.value1)),
    second: fromEnum5(second(v.value1)),
    millisecond: fromEnum6(millisecond(v.value1))
  };
};
var diff = function(dictDuration) {
  var toDuration2 = toDuration(dictDuration);
  return function(dt1) {
    return function(dt2) {
      return toDuration2(calcDiff(toRecord(dt1), toRecord(dt2)));
    };
  };
};

// output/Data.DateTime.Instant/foreign.js
var createDateTime = function(y, m, d, h, mi, s, ms) {
  var dateTime = new Date(Date.UTC(y, m, d, h, mi, s, ms));
  if (y >= 0 && y < 100) {
    dateTime.setUTCFullYear(y);
  }
  return dateTime;
};
function fromDateTimeImpl(y, mo, d, h, mi, s, ms) {
  return createDateTime(y, mo - 1, d, h, mi, s, ms).getTime();
}
function toDateTimeImpl(ctor) {
  return function(instant2) {
    var dt = new Date(instant2);
    return ctor(dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds());
  };
}

// output/Data.DateTime.Instant/index.js
var fromJust5 = /* @__PURE__ */ fromJust();
var toEnum3 = /* @__PURE__ */ toEnum(boundedEnumMonth);
var fromEnum7 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
var unInstant = function(v) {
  return v;
};
var toDateTime = /* @__PURE__ */ function() {
  var mkDateTime = function(y) {
    return function(mo) {
      return function(d) {
        return function(h) {
          return function(mi) {
            return function(s) {
              return function(ms) {
                return new DateTime(canonicalDate(y)(fromJust5(toEnum3(mo)))(d), new Time(h, mi, s, ms));
              };
            };
          };
        };
      };
    };
  };
  return toDateTimeImpl(mkDateTime);
}();
var instant = function(v) {
  if (v >= -86399778816e5 && v <= 8639977881599999) {
    return new Just(v);
  }
  ;
  if (otherwise) {
    return Nothing.value;
  }
  ;
  throw new Error("Failed pattern match at Data.DateTime.Instant (line 44, column 1 - line 44, column 41): " + [v.constructor.name]);
};
var fromDateTime = function(v) {
  return fromDateTimeImpl(year(v.value0), fromEnum7(month(v.value0)), day(v.value0), hour(v.value1), minute(v.value1), second(v.value1), millisecond(v.value1));
};

// output/Data.Ratio/index.js
var Ratio = /* @__PURE__ */ function() {
  function Ratio2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Ratio2.create = function(value0) {
    return function(value1) {
      return new Ratio2(value0, value1);
    };
  };
  return Ratio2;
}();
var reduce = function(dictOrd) {
  var gcd2 = gcd(dictOrd.Eq0());
  var signum2 = signum(dictOrd);
  var abs3 = abs(dictOrd);
  return function(dictEuclideanRing) {
    var gcd1 = gcd2(dictEuclideanRing);
    var div3 = div(dictEuclideanRing);
    var Ring0 = dictEuclideanRing.CommutativeRing0().Ring0();
    var mul4 = mul(Ring0.Semiring0());
    var signum1 = signum2(Ring0);
    var abs1 = abs3(Ring0);
    return function(n) {
      return function(d) {
        var g = gcd1(n)(d);
        var d$prime = div3(d)(g);
        return new Ratio(mul4(div3(n)(g))(signum1(d$prime)), abs1(d$prime));
      };
    };
  };
};
var numerator = function(v) {
  return v.value0;
};
var denominator = function(v) {
  return v.value1;
};

// output/JS.BigInt/foreign.js
var fromInt = (n) => BigInt(n);
var toNumber2 = (n) => Number(n);
var biAdd = (x) => (y) => x + y;
var biMul = (x) => (y) => x * y;
var biSub = (x) => (y) => x - y;
var biMod = (x) => (y) => {
  if (y === 0n)
    return 0n;
  const yy = y < 0n ? -y : y;
  return (x % yy + yy) % yy;
};
var biDiv = (x) => (y) => {
  if (y === 0n)
    return 0n;
  return (x - biMod(x)(y)) / y;
};
var biDegree = (x) => {
  return x < 0n ? -x : x;
};
var biZero = 0n;
var biOne = 1n;
var biEquals = (x) => (y) => x == y;
var biCompare = (x) => (y) => {
  if (x === y)
    return 0;
  else if (x > y)
    return 1;
  else
    return -1;
};

// output/JS.BigInt/index.js
var semiringBigInt = {
  add: biAdd,
  zero: biZero,
  mul: biMul,
  one: biOne
};
var ringBigInt = {
  sub: biSub,
  Semiring0: function() {
    return semiringBigInt;
  }
};
var eqBigInt = {
  eq: biEquals
};
var ordBigInt = {
  compare: function(x) {
    return function(y) {
      var v = biCompare(x)(y);
      if (v === 1) {
        return GT.value;
      }
      ;
      if (v === 0) {
        return EQ.value;
      }
      ;
      return LT.value;
    };
  },
  Eq0: function() {
    return eqBigInt;
  }
};
var commutativeRingBigInt = {
  Ring0: function() {
    return ringBigInt;
  }
};
var euclideanRingBigInt = {
  degree: biDegree,
  div: biDiv,
  mod: biMod,
  CommutativeRing0: function() {
    return commutativeRingBigInt;
  }
};

// output/Data.Rational/index.js
var reduce2 = /* @__PURE__ */ reduce(ordBigInt)(euclideanRingBigInt);
var toRationalInt = {
  toRational: function(a) {
    return function(b) {
      return reduce2(fromInt(a))(fromInt(b));
    };
  }
};
var toRationalBigInt = {
  toRational: function(a) {
    return function(b) {
      return reduce2(a)(b);
    };
  }
};
var toRational = function(dict) {
  return dict.toRational;
};
var toNumber3 = function(v) {
  return toNumber2(numerator(v)) / toNumber2(denominator(v));
};
var fromInt2 = function(i) {
  return reduce2(fromInt(i))(fromInt(1));
};

// output/Effect.Now/foreign.js
function now() {
  return Date.now();
}

// output/Effect/foreign.js
var pureE = function(a) {
  return function() {
    return a;
  };
};
var bindE = function(a) {
  return function(f) {
    return function() {
      return f(a())();
    };
  };
};

// output/Control.Monad/index.js
var ap = function(dictMonad) {
  var bind15 = bind(dictMonad.Bind1());
  var pure15 = pure(dictMonad.Applicative0());
  return function(f) {
    return function(a) {
      return bind15(f)(function(f$prime) {
        return bind15(a)(function(a$prime) {
          return pure15(f$prime(a$prime));
        });
      });
    };
  };
};

// output/Effect/index.js
var $runtime_lazy4 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var monadEffect = {
  Applicative0: function() {
    return applicativeEffect;
  },
  Bind1: function() {
    return bindEffect;
  }
};
var bindEffect = {
  bind: bindE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var applicativeEffect = {
  pure: pureE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy4("functorEffect", "Effect", function() {
  return {
    map: liftA1(applicativeEffect)
  };
});
var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy4("applyEffect", "Effect", function() {
  return {
    apply: ap(monadEffect),
    Functor0: function() {
      return $lazy_functorEffect(0);
    }
  };
});
var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

// output/Effect.Now/index.js
var map4 = /* @__PURE__ */ map(functorEffect);
var nowDateTime = /* @__PURE__ */ map4(toDateTime)(now);

// output/Data.Tempo/index.js
var unwrap2 = /* @__PURE__ */ unwrap();
var diff2 = /* @__PURE__ */ diff(durationMilliseconds);
var fromJust6 = /* @__PURE__ */ fromJust();
var toRational1 = /* @__PURE__ */ toRational(toRationalBigInt);
var timeToCountNumber = function(x) {
  return function(t) {
    var timeDiff = unwrap2(diff2(t)(x.time));
    var df = timeDiff * toNumber3(x.freq) / 1e3;
    return df + toNumber3(x.count);
  };
};
var newTempo = function(freq) {
  return function __do2() {
    var time3 = nowDateTime();
    return {
      freq,
      time: time3,
      count: fromInt2(0)
    };
  };
};
var fromForeignTempo = function(x) {
  var time3 = toDateTime(fromJust6(instant(x.time)));
  var freq = toRational1(x.freqNumerator)(x.freqDenominator);
  var count2 = toRational1(x.countNumerator)(x.countDenominator);
  return {
    freq,
    time: time3,
    count: count2
  };
};

// output/Effect.Ref/foreign.js
var _new = function(val) {
  return function() {
    return { value: val };
  };
};
var read = function(ref) {
  return function() {
    return ref.value;
  };
};
var write = function(val) {
  return function(ref) {
    return function() {
      ref.value = val;
    };
  };
};

// output/Effect.Ref/index.js
var $$new = _new;

// output/Control.Monad.Reader.Class/index.js
var ask = function(dict) {
  return dict.ask;
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};
var monadThrowEither = /* @__PURE__ */ function() {
  return {
    throwError: Left.create,
    Monad0: function() {
      return monadEither;
    }
  };
}();

// output/Control.Monad.Rec.Class/index.js
var Loop = /* @__PURE__ */ function() {
  function Loop2(value0) {
    this.value0 = value0;
  }
  ;
  Loop2.create = function(value0) {
    return new Loop2(value0);
  };
  return Loop2;
}();
var Done = /* @__PURE__ */ function() {
  function Done2(value0) {
    this.value0 = value0;
  }
  ;
  Done2.create = function(value0) {
    return new Done2(value0);
  };
  return Done2;
}();
var tailRecM = function(dict) {
  return dict.tailRecM;
};
var tailRec = function(f) {
  var go = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Loop) {
        $copy_v = f(v.value0);
        return;
      }
      ;
      if (v instanceof Done) {
        $tco_done = true;
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  return function($85) {
    return go(f($85));
  };
};
var monadRecIdentity = {
  tailRecM: function(f) {
    var runIdentity = function(v) {
      return v;
    };
    var $86 = tailRec(function($88) {
      return runIdentity(f($88));
    });
    return function($87) {
      return Identity($86($87));
    };
  },
  Monad0: function() {
    return monadIdentity;
  }
};
var bifunctorStep = {
  bimap: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Loop) {
          return new Loop(v(v2.value0));
        }
        ;
        if (v2 instanceof Done) {
          return new Done(v1(v2.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 33, column 1 - line 35, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};

// output/Control.Monad.State.Class/index.js
var state = function(dict) {
  return dict.state;
};
var put = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(s) {
    return state1(function(v) {
      return new Tuple(unit, s);
    });
  };
};
var modify_ = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(f) {
    return state1(function(s) {
      return new Tuple(unit, f(s));
    });
  };
};
var modify = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(f) {
    return state1(function(s) {
      var s$prime = f(s);
      return new Tuple(s$prime, s$prime);
    });
  };
};
var get = function(dictMonadState) {
  return state(dictMonadState)(function(s) {
    return new Tuple(s, s);
  });
};

// output/Control.Monad.Trans.Class/index.js
var lift = function(dict) {
  return dict.lift;
};

// output/Effect.Class/index.js
var monadEffectEffect = {
  liftEffect: /* @__PURE__ */ identity(categoryFn),
  Monad0: function() {
    return monadEffect;
  }
};
var liftEffect = function(dict) {
  return dict.liftEffect;
};

// output/Control.Monad.Reader.Trans/index.js
var ReaderT = function(x) {
  return x;
};
var runReaderT = function(v) {
  return v;
};
var monadTransReaderT = {
  lift: function(dictMonad) {
    return function($147) {
      return ReaderT($$const($147));
    };
  }
};
var lift3 = /* @__PURE__ */ lift(monadTransReaderT);
var mapReaderT = function(f) {
  return function(v) {
    return function($148) {
      return f(v($148));
    };
  };
};
var functorReaderT = function(dictFunctor) {
  return {
    map: function() {
      var $149 = map(dictFunctor);
      return function($150) {
        return mapReaderT($149($150));
      };
    }()
  };
};
var applyReaderT = function(dictApply) {
  var apply2 = apply(dictApply);
  var functorReaderT1 = functorReaderT(dictApply.Functor0());
  return {
    apply: function(v) {
      return function(v1) {
        return function(r) {
          return apply2(v(r))(v1(r));
        };
      };
    },
    Functor0: function() {
      return functorReaderT1;
    }
  };
};
var bindReaderT = function(dictBind) {
  var bind15 = bind(dictBind);
  var applyReaderT1 = applyReaderT(dictBind.Apply0());
  return {
    bind: function(v) {
      return function(k) {
        return function(r) {
          return bind15(v(r))(function(a) {
            var v1 = k(a);
            return v1(r);
          });
        };
      };
    },
    Apply0: function() {
      return applyReaderT1;
    }
  };
};
var applicativeReaderT = function(dictApplicative) {
  var applyReaderT1 = applyReaderT(dictApplicative.Apply0());
  return {
    pure: function() {
      var $154 = pure(dictApplicative);
      return function($155) {
        return ReaderT($$const($154($155)));
      };
    }(),
    Apply0: function() {
      return applyReaderT1;
    }
  };
};
var monadReaderT = function(dictMonad) {
  var applicativeReaderT1 = applicativeReaderT(dictMonad.Applicative0());
  var bindReaderT1 = bindReaderT(dictMonad.Bind1());
  return {
    Applicative0: function() {
      return applicativeReaderT1;
    },
    Bind1: function() {
      return bindReaderT1;
    }
  };
};
var monadAskReaderT = function(dictMonad) {
  var monadReaderT1 = monadReaderT(dictMonad);
  return {
    ask: pure(dictMonad.Applicative0()),
    Monad0: function() {
      return monadReaderT1;
    }
  };
};
var monadEffectReader = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadReaderT1 = monadReaderT(Monad0);
  return {
    liftEffect: function() {
      var $157 = lift3(Monad0);
      var $158 = liftEffect(dictMonadEffect);
      return function($159) {
        return $157($158($159));
      };
    }(),
    Monad0: function() {
      return monadReaderT1;
    }
  };
};

// output/Control.Monad.State.Trans/index.js
var monadTransStateT = {
  lift: function(dictMonad) {
    var bind15 = bind(dictMonad.Bind1());
    var pure15 = pure(dictMonad.Applicative0());
    return function(m) {
      return function(s) {
        return bind15(m)(function(x) {
          return pure15(new Tuple(x, s));
        });
      };
    };
  }
};
var lift4 = /* @__PURE__ */ lift(monadTransStateT);
var functorStateT = function(dictFunctor) {
  var map15 = map(dictFunctor);
  return {
    map: function(f) {
      return function(v) {
        return function(s) {
          return map15(function(v1) {
            return new Tuple(f(v1.value0), v1.value1);
          })(v(s));
        };
      };
    }
  };
};
var execStateT = function(dictFunctor) {
  var map15 = map(dictFunctor);
  return function(v) {
    return function(s) {
      return map15(snd)(v(s));
    };
  };
};
var evalStateT = function(dictFunctor) {
  var map15 = map(dictFunctor);
  return function(v) {
    return function(s) {
      return map15(fst)(v(s));
    };
  };
};
var monadStateT = function(dictMonad) {
  return {
    Applicative0: function() {
      return applicativeStateT(dictMonad);
    },
    Bind1: function() {
      return bindStateT(dictMonad);
    }
  };
};
var bindStateT = function(dictMonad) {
  var bind15 = bind(dictMonad.Bind1());
  return {
    bind: function(v) {
      return function(f) {
        return function(s) {
          return bind15(v(s))(function(v1) {
            var v3 = f(v1.value0);
            return v3(v1.value1);
          });
        };
      };
    },
    Apply0: function() {
      return applyStateT(dictMonad);
    }
  };
};
var applyStateT = function(dictMonad) {
  var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: ap(monadStateT(dictMonad)),
    Functor0: function() {
      return functorStateT1;
    }
  };
};
var applicativeStateT = function(dictMonad) {
  var pure15 = pure(dictMonad.Applicative0());
  return {
    pure: function(a) {
      return function(s) {
        return pure15(new Tuple(a, s));
      };
    },
    Apply0: function() {
      return applyStateT(dictMonad);
    }
  };
};
var monadAskStateT = function(dictMonadAsk) {
  var Monad0 = dictMonadAsk.Monad0();
  var monadStateT1 = monadStateT(Monad0);
  return {
    ask: lift4(Monad0)(ask(dictMonadAsk)),
    Monad0: function() {
      return monadStateT1;
    }
  };
};
var monadEffectState = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var monadStateT1 = monadStateT(Monad0);
  return {
    liftEffect: function() {
      var $197 = lift4(Monad0);
      var $198 = liftEffect(dictMonadEffect);
      return function($199) {
        return $197($198($199));
      };
    }(),
    Monad0: function() {
      return monadStateT1;
    }
  };
};
var monadStateStateT = function(dictMonad) {
  var pure15 = pure(dictMonad.Applicative0());
  var monadStateT1 = monadStateT(dictMonad);
  return {
    state: function(f) {
      return function($200) {
        return pure15(f($200));
      };
    },
    Monad0: function() {
      return monadStateT1;
    }
  };
};
var monadThrowStateT = function(dictMonadThrow) {
  var Monad0 = dictMonadThrow.Monad0();
  var lift1 = lift4(Monad0);
  var throwError5 = throwError(dictMonadThrow);
  var monadStateT1 = monadStateT(Monad0);
  return {
    throwError: function(e) {
      return lift1(throwError5(e));
    },
    Monad0: function() {
      return monadStateT1;
    }
  };
};

// output/Data.Array/foreign.js
var replicateFill = function(count2, value) {
  if (count2 < 1) {
    return [];
  }
  var result = new Array(count2);
  return result.fill(value);
};
var replicatePolyfill = function(count2, value) {
  var result = [];
  var n = 0;
  for (var i = 0; i < count2; i++) {
    result[n++] = value;
  }
  return result;
};
var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var fromFoldableImpl = function() {
  function Cons3(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  }
  var emptyList = {};
  function curryCons(head4) {
    return function(tail2) {
      return new Cons3(head4, tail2);
    };
  }
  function listToArray(list) {
    var result = [];
    var count2 = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count2++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }
  return function(foldr3, xs) {
    return listToArray(foldr3(curryCons)(emptyList)(xs));
  };
}();
var length = function(xs) {
  return xs.length;
};
var unconsImpl = function(empty4, next, xs) {
  return xs.length === 0 ? empty4({}) : next(xs[0])(xs.slice(1));
};
var indexImpl = function(just, nothing, xs, i) {
  return i < 0 || i >= xs.length ? nothing : just(xs[i]);
};
var findIndexImpl = function(just, nothing, f, xs) {
  for (var i = 0, l = xs.length; i < l; i++) {
    if (f(xs[i]))
      return just(i);
  }
  return nothing;
};
var _updateAt = function(just, nothing, i, a, l) {
  if (i < 0 || i >= l.length)
    return nothing;
  var l1 = l.slice();
  l1[i] = a;
  return just(l1);
};
var sortByImpl = function() {
  function mergeFromTo(compare4, fromOrdering, xs1, xs2, from3, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from3 + (to - from3 >> 1);
    if (mid - from3 > 1)
      mergeFromTo(compare4, fromOrdering, xs2, xs1, from3, mid);
    if (to - mid > 1)
      mergeFromTo(compare4, fromOrdering, xs2, xs1, mid, to);
    i = from3;
    j = mid;
    k = from3;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare4(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare4, fromOrdering, xs) {
    var out;
    if (xs.length < 2)
      return xs;
    out = xs.slice(0);
    mergeFromTo(compare4, fromOrdering, out, xs.slice(0), 0, xs.length);
    return out;
  };
}();
var sliceImpl = function(s, e, l) {
  return l.slice(s, e);
};
var zipWithImpl = function(f, xs, ys) {
  var l = xs.length < ys.length ? xs.length : ys.length;
  var result = new Array(l);
  for (var i = 0; i < l; i++) {
    result[i] = f(xs[i])(ys[i]);
  }
  return result;
};
var unsafeIndexImpl = function(xs, n) {
  return xs[n];
};

// output/Data.Array.ST/foreign.js
var pushAllImpl = function(as, xs) {
  return xs.push.apply(xs, as);
};
function unsafeFreezeThawImpl(xs) {
  return xs;
}
var unsafeFreezeImpl = unsafeFreezeThawImpl;
function copyImpl(xs) {
  return xs.slice();
}
var thawImpl = copyImpl;
var sortByImpl2 = function() {
  function mergeFromTo(compare4, fromOrdering, xs1, xs2, from3, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from3 + (to - from3 >> 1);
    if (mid - from3 > 1)
      mergeFromTo(compare4, fromOrdering, xs2, xs1, from3, mid);
    if (to - mid > 1)
      mergeFromTo(compare4, fromOrdering, xs2, xs1, mid, to);
    i = from3;
    j = mid;
    k = from3;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare4(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare4, fromOrdering, xs) {
    if (xs.length < 2)
      return xs;
    mergeFromTo(compare4, fromOrdering, xs, xs.slice(0), 0, xs.length);
    return xs;
  };
}();

// output/Control.Monad.ST.Uncurried/foreign.js
var runSTFn1 = function runSTFn12(fn) {
  return function(a) {
    return function() {
      return fn(a);
    };
  };
};
var runSTFn2 = function runSTFn22(fn) {
  return function(a) {
    return function(b) {
      return function() {
        return fn(a, b);
      };
    };
  };
};

// output/Data.Array.ST/index.js
var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
var thaw = /* @__PURE__ */ runSTFn1(thawImpl);
var withArray = function(f) {
  return function(xs) {
    return function __do2() {
      var result = thaw(xs)();
      f(result)();
      return unsafeFreeze(result)();
    };
  };
};
var push = function(a) {
  return runSTFn2(pushAllImpl)([a]);
};

// output/Data.Function.Uncurried/foreign.js
var mkFn5 = function(fn) {
  return function(a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};
var runFn2 = function(fn) {
  return function(a) {
    return function(b) {
      return fn(a, b);
    };
  };
};
var runFn3 = function(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return fn(a, b, c);
      };
    };
  };
};
var runFn4 = function(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};
var runFn5 = function(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

// output/Data.FunctorWithIndex/foreign.js
var mapWithIndexArray = function(f) {
  return function(xs) {
    var l = xs.length;
    var result = Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(i)(xs[i]);
    }
    return result;
  };
};

// output/Data.FunctorWithIndex/index.js
var mapWithIndex = function(dict) {
  return dict.mapWithIndex;
};
var functorWithIndexArray = {
  mapWithIndex: mapWithIndexArray,
  Functor0: function() {
    return functorArray;
  }
};

// output/Data.Array/index.js
var append2 = /* @__PURE__ */ append(semigroupArray);
var zipWith = /* @__PURE__ */ runFn3(zipWithImpl);
var zip = /* @__PURE__ */ function() {
  return zipWith(Tuple.create);
}();
var updateAt = /* @__PURE__ */ function() {
  return runFn5(_updateAt)(Just.create)(Nothing.value);
}();
var unsafeIndex = function() {
  return runFn2(unsafeIndexImpl);
};
var uncons = /* @__PURE__ */ function() {
  return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
    return function(xs) {
      return new Just({
        head: x,
        tail: xs
      });
    };
  });
}();
var sortBy = function(comp) {
  return runFn3(sortByImpl)(comp)(function(v) {
    if (v instanceof GT) {
      return 1;
    }
    ;
    if (v instanceof EQ) {
      return 0;
    }
    ;
    if (v instanceof LT) {
      return -1 | 0;
    }
    ;
    throw new Error("Failed pattern match at Data.Array (line 897, column 38 - line 900, column 11): " + [v.constructor.name]);
  });
};
var sort = function(dictOrd) {
  var compare4 = compare(dictOrd);
  return function(xs) {
    return sortBy(compare4)(xs);
  };
};
var snoc = function(xs) {
  return function(x) {
    return withArray(push(x))(xs)();
  };
};
var slice = /* @__PURE__ */ runFn3(sliceImpl);
var take = function(n) {
  return function(xs) {
    var $152 = n < 1;
    if ($152) {
      return [];
    }
    ;
    return slice(0)(n)(xs);
  };
};
var replicate = /* @__PURE__ */ runFn2(replicateImpl);
var index = /* @__PURE__ */ function() {
  return runFn4(indexImpl)(Just.create)(Nothing.value);
}();
var findIndex = /* @__PURE__ */ function() {
  return runFn4(findIndexImpl)(Just.create)(Nothing.value);
}();
var elemIndex = function(dictEq) {
  var eq22 = eq(dictEq);
  return function(x) {
    return findIndex(function(v) {
      return eq22(v)(x);
    });
  };
};
var notElem2 = function(dictEq) {
  var elemIndex1 = elemIndex(dictEq);
  return function(a) {
    return function(arr) {
      return isNothing(elemIndex1(a)(arr));
    };
  };
};
var elem2 = function(dictEq) {
  var elemIndex1 = elemIndex(dictEq);
  return function(a) {
    return function(arr) {
      return isJust(elemIndex1(a)(arr));
    };
  };
};
var drop = function(n) {
  return function(xs) {
    var $173 = n < 1;
    if ($173) {
      return xs;
    }
    ;
    return slice(n)(length(xs))(xs);
  };
};
var cons = function(x) {
  return function(xs) {
    return append2([x])(xs);
  };
};
var some = function(dictAlternative) {
  var apply1 = apply(dictAlternative.Applicative0().Apply0());
  var map32 = map(dictAlternative.Plus1().Alt0().Functor0());
  return function(dictLazy) {
    var defer4 = defer(dictLazy);
    return function(v) {
      return apply1(map32(cons)(v))(defer4(function(v1) {
        return many(dictAlternative)(dictLazy)(v);
      }));
    };
  };
};
var many = function(dictAlternative) {
  var alt6 = alt(dictAlternative.Plus1().Alt0());
  var pure15 = pure(dictAlternative.Applicative0());
  return function(dictLazy) {
    return function(v) {
      return alt6(some(dictAlternative)(dictLazy)(v))(pure15([]));
    };
  };
};

// output/Data.FoldableWithIndex/index.js
var foldr8 = /* @__PURE__ */ foldr(foldableArray);
var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
var foldl8 = /* @__PURE__ */ foldl(foldableArray);
var foldrWithIndex = function(dict) {
  return dict.foldrWithIndex;
};
var traverseWithIndex_ = function(dictApplicative) {
  var applySecond4 = applySecond(dictApplicative.Apply0());
  var pure15 = pure(dictApplicative);
  return function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(f) {
      return foldrWithIndex1(function(i) {
        var $289 = f(i);
        return function($290) {
          return applySecond4($289($290));
        };
      })(pure15(unit));
    };
  };
};
var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
  var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
  return function(dictMonoid) {
    var append4 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldrWithIndex1(function(i) {
        return function(x) {
          return function(acc) {
            return append4(f(i)(x))(acc);
          };
        };
      })(mempty2);
    };
  };
};
var foldableWithIndexArray = {
  foldrWithIndex: function(f) {
    return function(z) {
      var $291 = foldr8(function(v) {
        return function(y) {
          return f(v.value0)(v.value1)(y);
        };
      })(z);
      var $292 = mapWithIndex2(Tuple.create);
      return function($293) {
        return $291($292($293));
      };
    };
  },
  foldlWithIndex: function(f) {
    return function(z) {
      var $294 = foldl8(function(y) {
        return function(v) {
          return f(v.value0)(y)(v.value1);
        };
      })(z);
      var $295 = mapWithIndex2(Tuple.create);
      return function($296) {
        return $294($295($296));
      };
    };
  },
  foldMapWithIndex: function(dictMonoid) {
    return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
  },
  Foldable0: function() {
    return foldableArray;
  }
};

// output/Data.TraversableWithIndex/index.js
var traverseWithIndexDefault = function(dictTraversableWithIndex) {
  var sequence2 = sequence(dictTraversableWithIndex.Traversable2());
  var mapWithIndex4 = mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
  return function(dictApplicative) {
    var sequence12 = sequence2(dictApplicative);
    return function(f) {
      var $174 = mapWithIndex4(f);
      return function($175) {
        return sequence12($174($175));
      };
    };
  };
};
var traverseWithIndex = function(dict) {
  return dict.traverseWithIndex;
};
var traversableWithIndexArray = {
  traverseWithIndex: function(dictApplicative) {
    return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
  },
  FunctorWithIndex0: function() {
    return functorWithIndexArray;
  },
  FoldableWithIndex1: function() {
    return foldableWithIndexArray;
  },
  Traversable2: function() {
    return traversableArray;
  }
};

// output/Data.NonEmpty/index.js
var NonEmpty = /* @__PURE__ */ function() {
  function NonEmpty2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  NonEmpty2.create = function(value0) {
    return function(value1) {
      return new NonEmpty2(value0, value1);
    };
  };
  return NonEmpty2;
}();

// output/Data.List.Types/index.js
var identity7 = /* @__PURE__ */ identity(categoryFn);
var Nil = /* @__PURE__ */ function() {
  function Nil3() {
  }
  ;
  Nil3.value = new Nil3();
  return Nil3;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons3.create = function(value0) {
    return function(value1) {
      return new Cons3(value0, value1);
    };
  };
  return Cons3;
}();
var listMap = function(f) {
  var chunkedRevMap = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
          $tco_var_v = new Cons(v1, v);
          $copy_v1 = v1.value1.value1.value1;
          return;
        }
        ;
        var unrolledMap = function(v2) {
          if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
            return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
          }
          ;
          if (v2 instanceof Cons && v2.value1 instanceof Nil) {
            return new Cons(f(v2.value0), Nil.value);
          }
          ;
          return Nil.value;
        };
        var reverseUnrolledMap = function($copy_v2) {
          return function($copy_v3) {
            var $tco_var_v2 = $copy_v2;
            var $tco_done1 = false;
            var $tco_result2;
            function $tco_loop2(v2, v3) {
              if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                $tco_var_v2 = v2.value1;
                $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                return;
              }
              ;
              $tco_done1 = true;
              return v3;
            }
            ;
            while (!$tco_done1) {
              $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
            }
            ;
            return $tco_result2;
          };
        };
        $tco_done = true;
        return reverseUnrolledMap(v)(unrolledMap(v1));
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return chunkedRevMap(Nil.value);
};
var functorList = {
  map: listMap
};
var foldableList = {
  foldr: function(f) {
    return function(b) {
      var rev = function() {
        var go = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return v;
              }
              ;
              if (v1 instanceof Cons) {
                $tco_var_v = new Cons(v1.value0, v);
                $copy_v1 = v1.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return go(Nil.value);
      }();
      var $284 = foldl(foldableList)(flip(f))(b);
      return function($285) {
        return $284(rev($285));
      };
    };
  },
  foldl: function(f) {
    var go = function($copy_b) {
      return function($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done1 = true;
            return b;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go;
  },
  foldMap: function(dictMonoid) {
    var append22 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $286 = append22(acc);
        return function($287) {
          return $286(f($287));
        };
      })(mempty2);
    };
  }
};
var foldl2 = /* @__PURE__ */ foldl(foldableList);
var traversableList = {
  traverse: function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    var map15 = map(Apply0.Functor0());
    var lift22 = lift2(Apply0);
    var pure15 = pure(dictApplicative);
    return function(f) {
      var $301 = map15(foldl2(flip(Cons.create))(Nil.value));
      var $302 = foldl2(function(acc) {
        var $304 = lift22(flip(Cons.create))(acc);
        return function($305) {
          return $304(f($305));
        };
      })(pure15(Nil.value));
      return function($303) {
        return $301($302($303));
      };
    };
  },
  sequence: function(dictApplicative) {
    return traverse(traversableList)(dictApplicative)(identity7);
  },
  Functor0: function() {
    return functorList;
  },
  Foldable1: function() {
    return foldableList;
  }
};

// output/Data.Map.Internal/index.js
var $runtime_lazy5 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var identity8 = /* @__PURE__ */ identity(categoryFn);
var Leaf = /* @__PURE__ */ function() {
  function Leaf2() {
  }
  ;
  Leaf2.value = new Leaf2();
  return Leaf2;
}();
var Node = /* @__PURE__ */ function() {
  function Node2(value0, value1, value2, value3, value4, value5) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
    this.value4 = value4;
    this.value5 = value5;
  }
  ;
  Node2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return function(value4) {
            return function(value5) {
              return new Node2(value0, value1, value2, value3, value4, value5);
            };
          };
        };
      };
    };
  };
  return Node2;
}();
var Split = /* @__PURE__ */ function() {
  function Split2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Split2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Split2(value0, value1, value2);
      };
    };
  };
  return Split2;
}();
var SplitLast = /* @__PURE__ */ function() {
  function SplitLast2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  SplitLast2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new SplitLast2(value0, value1, value2);
      };
    };
  };
  return SplitLast2;
}();
var unsafeNode = function(k, v, l, r) {
  if (l instanceof Leaf) {
    if (r instanceof Leaf) {
      return new Node(1, 1, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 680, column 5 - line 684, column 39): " + [r.constructor.name]);
  }
  ;
  if (l instanceof Node) {
    if (r instanceof Leaf) {
      return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + function() {
        var $277 = l.value0 > r.value0;
        if ($277) {
          return l.value0;
        }
        ;
        return r.value0;
      }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 686, column 5 - line 690, column 68): " + [r.constructor.name]);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 678, column 32 - line 690, column 68): " + [l.constructor.name]);
};
var size = function(v) {
  if (v instanceof Leaf) {
    return 0;
  }
  ;
  if (v instanceof Node) {
    return v.value1;
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 616, column 8 - line 618, column 24): " + [v.constructor.name]);
};
var singleton3 = function(k) {
  return function(v) {
    return new Node(1, 1, k, v, Leaf.value, Leaf.value);
  };
};
var unsafeBalancedNode = /* @__PURE__ */ function() {
  var height = function(v) {
    if (v instanceof Leaf) {
      return 0;
    }
    ;
    if (v instanceof Node) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 735, column 12 - line 737, column 26): " + [v.constructor.name]);
  };
  var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
    if (rl instanceof Node && rl.value0 > height(rr)) {
      return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
    }
    ;
    return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
  };
  var rotateRight = function(k, v, lk, lv, ll, lr, r) {
    if (lr instanceof Node && height(ll) <= lr.value0) {
      return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
    }
    ;
    return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
  };
  return function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return singleton3(k)(v);
      }
      ;
      if (r instanceof Node && r.value0 > 1) {
        return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Node) {
        if (r.value0 > (l.value0 + 1 | 0)) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        if (l.value0 > (r.value0 + 1 | 0)) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
      }
      ;
      if (r instanceof Leaf && l.value0 > 1) {
        return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 695, column 40 - line 716, column 34): " + [l.constructor.name]);
  };
}();
var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy5("unsafeSplit", "Data.Map.Internal", function() {
  return function(comp, k, m) {
    if (m instanceof Leaf) {
      return new Split(Nothing.value, Leaf.value, Leaf.value);
    }
    ;
    if (m instanceof Node) {
      var v = comp(k)(m.value2);
      if (v instanceof LT) {
        var v1 = $lazy_unsafeSplit(771)(comp, k, m.value4);
        return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
      }
      ;
      if (v instanceof GT) {
        var v1 = $lazy_unsafeSplit(774)(comp, k, m.value5);
        return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
      }
      ;
      if (v instanceof EQ) {
        return new Split(new Just(m.value3), m.value4, m.value5);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 769, column 5 - line 777, column 30): " + [v.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 765, column 34 - line 777, column 30): " + [m.constructor.name]);
  };
});
var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(764);
var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy5("unsafeSplitLast", "Data.Map.Internal", function() {
  return function(k, v, l, r) {
    if (r instanceof Leaf) {
      return new SplitLast(k, v, l);
    }
    ;
    if (r instanceof Node) {
      var v1 = $lazy_unsafeSplitLast(757)(r.value2, r.value3, r.value4, r.value5);
      return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 754, column 37 - line 758, column 57): " + [r.constructor.name]);
  };
});
var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(753);
var unsafeJoinNodes = function(v, v1) {
  if (v instanceof Leaf) {
    return v1;
  }
  ;
  if (v instanceof Node) {
    var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
    return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 742, column 25 - line 746, column 38): " + [v.constructor.name, v1.constructor.name]);
};
var $lazy_unsafeUnionWith = /* @__PURE__ */ $runtime_lazy5("unsafeUnionWith", "Data.Map.Internal", function() {
  return function(comp, app, l, r) {
    if (l instanceof Leaf) {
      return r;
    }
    ;
    if (r instanceof Leaf) {
      return l;
    }
    ;
    if (r instanceof Node) {
      var v = unsafeSplit(comp, r.value2, l);
      var l$prime = $lazy_unsafeUnionWith(787)(comp, app, v.value1, r.value4);
      var r$prime = $lazy_unsafeUnionWith(788)(comp, app, v.value2, r.value5);
      if (v.value0 instanceof Just) {
        return unsafeBalancedNode(r.value2, app(v.value0.value0)(r.value3), l$prime, r$prime);
      }
      ;
      if (v.value0 instanceof Nothing) {
        return unsafeBalancedNode(r.value2, r.value3, l$prime, r$prime);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 789, column 5 - line 793, column 46): " + [v.value0.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 782, column 42 - line 793, column 46): " + [l.constructor.name, r.constructor.name]);
  };
});
var unsafeUnionWith = /* @__PURE__ */ $lazy_unsafeUnionWith(781);
var unionWith = function(dictOrd) {
  var compare4 = compare(dictOrd);
  return function(app) {
    return function(m1) {
      return function(m2) {
        return unsafeUnionWith(compare4, app, m1, m2);
      };
    };
  };
};
var union = function(dictOrd) {
  return unionWith(dictOrd)($$const);
};
var mapMaybeWithKey = function(dictOrd) {
  return function(f) {
    var go = function(v) {
      if (v instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (v instanceof Node) {
        var v2 = f(v.value2)(v.value3);
        if (v2 instanceof Just) {
          return unsafeBalancedNode(v.value2, v2.value0, go(v.value4), go(v.value5));
        }
        ;
        if (v2 instanceof Nothing) {
          return unsafeJoinNodes(go(v.value4), go(v.value5));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 659, column 7 - line 663, column 47): " + [v2.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 656, column 8 - line 663, column 47): " + [v.constructor.name]);
    };
    return go;
  };
};
var mapMaybe = function(dictOrd) {
  var $763 = mapMaybeWithKey(dictOrd);
  return function($764) {
    return $763($$const($764));
  };
};
var lookup = function(dictOrd) {
  var compare4 = compare(dictOrd);
  return function(k) {
    var go = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Leaf) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare4(k)(v.value2);
          if (v1 instanceof LT) {
            $copy_v = v.value4;
            return;
          }
          ;
          if (v1 instanceof GT) {
            $copy_v = v.value5;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return new Just(v.value3);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 281, column 7 - line 284, column 22): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 278, column 8 - line 284, column 22): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return go;
  };
};
var insert = function(dictOrd) {
  var compare4 = compare(dictOrd);
  return function(k) {
    return function(v) {
      var go = function(v1) {
        if (v1 instanceof Leaf) {
          return singleton3(k)(v);
        }
        ;
        if (v1 instanceof Node) {
          var v2 = compare4(k)(v1.value2);
          if (v2 instanceof LT) {
            return unsafeBalancedNode(v1.value2, v1.value3, go(v1.value4), v1.value5);
          }
          ;
          if (v2 instanceof GT) {
            return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go(v1.value5));
          }
          ;
          if (v2 instanceof EQ) {
            return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 469, column 7 - line 472, column 35): " + [v2.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 466, column 8 - line 472, column 35): " + [v1.constructor.name]);
      };
      return go;
    };
  };
};
var functorMap = {
  map: function(f) {
    var go = function(v) {
      if (v instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (v instanceof Node) {
        return new Node(v.value0, v.value1, v.value2, f(v.value3), go(v.value4), go(v.value5));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 145, column 10 - line 148, column 39): " + [v.constructor.name]);
    };
    return go;
  }
};
var foldableMap = {
  foldr: function(f) {
    return function(z) {
      var $lazy_go = $runtime_lazy5("go", "Data.Map.Internal", function() {
        return function(m$prime, z$prime) {
          if (m$prime instanceof Leaf) {
            return z$prime;
          }
          ;
          if (m$prime instanceof Node) {
            return $lazy_go(170)(m$prime.value4, f(m$prime.value3)($lazy_go(170)(m$prime.value5, z$prime)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 167, column 26 - line 170, column 43): " + [m$prime.constructor.name]);
        };
      });
      var go = $lazy_go(167);
      return function(m) {
        return go(m, z);
      };
    };
  },
  foldl: function(f) {
    return function(z) {
      var $lazy_go = $runtime_lazy5("go", "Data.Map.Internal", function() {
        return function(z$prime, m$prime) {
          if (m$prime instanceof Leaf) {
            return z$prime;
          }
          ;
          if (m$prime instanceof Node) {
            return $lazy_go(176)(f($lazy_go(176)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 173, column 26 - line 176, column 43): " + [m$prime.constructor.name]);
        };
      });
      var go = $lazy_go(173);
      return function(m) {
        return go(z, m);
      };
    };
  },
  foldMap: function(dictMonoid) {
    var mempty2 = mempty(dictMonoid);
    var append1 = append(dictMonoid.Semigroup0());
    return function(f) {
      var go = function(v) {
        if (v instanceof Leaf) {
          return mempty2;
        }
        ;
        if (v instanceof Node) {
          return append1(go(v.value4))(append1(f(v.value3))(go(v.value5)));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 179, column 10 - line 182, column 28): " + [v.constructor.name]);
      };
      return go;
    };
  }
};
var empty2 = /* @__PURE__ */ function() {
  return Leaf.value;
}();
var fromFoldable = function(dictOrd) {
  var insert1 = insert(dictOrd);
  return function(dictFoldable) {
    return foldl(dictFoldable)(function(m) {
      return function(v) {
        return insert1(v.value0)(v.value1)(m);
      };
    })(empty2);
  };
};
var unions = function(dictOrd) {
  var union1 = union(dictOrd);
  return function(dictFoldable) {
    return foldl(dictFoldable)(union1)(empty2);
  };
};
var $$delete = function(dictOrd) {
  var compare4 = compare(dictOrd);
  return function(k) {
    var go = function(v) {
      if (v instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (v instanceof Node) {
        var v1 = compare4(k)(v.value2);
        if (v1 instanceof LT) {
          return unsafeBalancedNode(v.value2, v.value3, go(v.value4), v.value5);
        }
        ;
        if (v1 instanceof GT) {
          return unsafeBalancedNode(v.value2, v.value3, v.value4, go(v.value5));
        }
        ;
        if (v1 instanceof EQ) {
          return unsafeJoinNodes(v.value4, v.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 496, column 7 - line 499, column 43): " + [v1.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 493, column 8 - line 499, column 43): " + [v.constructor.name]);
    };
    return go;
  };
};
var catMaybes = function(dictOrd) {
  return mapMaybe(dictOrd)(identity8);
};

// output/Effect.Console/foreign.js
var log2 = function(s) {
  return function() {
    console.log(s);
  };
};

// output/MaybeRef/index.js
var whenMaybeRef = function(dictMonadEffect) {
  var Monad0 = dictMonadEffect.Monad0();
  var bind15 = bind(Monad0.Bind1());
  var liftEffect6 = liftEffect(dictMonadEffect);
  var pure15 = pure(Monad0.Applicative0());
  return function(mRef) {
    return function(f) {
      return bind15(liftEffect6(read(mRef)))(function(m) {
        if (m instanceof Just) {
          return f(m.value0);
        }
        ;
        if (m instanceof Nothing) {
          return pure15(unit);
        }
        ;
        throw new Error("Failed pattern match at MaybeRef (line 15, column 3 - line 17, column 25): " + [m.constructor.name]);
      });
    };
  };
};

// output/ThreeJS/foreign.js
var newObject3D = () => new THREE.Object3D();
var newScene = () => new THREE.Scene();
var newPerspectiveCamera = (fov) => (aspect) => (near) => (far) => () => new THREE.PerspectiveCamera(fov, aspect, near, far);
var setAspect = (pCamera) => (aspect) => () => pCamera.aspect = aspect;
var newWebGLRenderer = (params) => () => new THREE.WebGLRenderer(params);
var render = (renderer) => (scene) => (camera) => () => renderer.render(scene, camera);
var setSize = (renderer) => (w) => (h) => (updateStyle) => () => renderer.setSize(w, h, updateStyle);
var setClearColor = (renderer) => (c) => (a) => () => renderer.setClearColor(c, a);
var newGLTFLoader = () => new THREE.GLTFLoader();
var loadGLTF1 = (loader) => (url) => (cb) => () => loader.load(url, (x) => cb(x)());
var newDRACOLoader = () => new THREE.DRACOLoader();
var setDecoderPath = (dracoLoader) => (modulePath) => () => dracoLoader.setDecoderPath(modulePath);
var setDRACOLoader = (gltfLoader) => (dracoLoader) => () => gltfLoader.setDRACOLoader(dracoLoader);
var newPlaneGeometry = (width) => (height) => (widthSegments) => (heightSegments) => () => new THREE.PlaneGeometry(width, height, widthSegments, heightSegments);
var newMesh = (geometry) => (material) => () => new THREE.Mesh(geometry, material);
var setReceiveShadow = (mesh) => (boolean) => () => mesh.receiveShadow = boolean;
var setColorInt = (thing) => (color) => () => thing.color = new THREE.Color(color);
var addAnything = (a) => (b) => () => a.add(b);
var removeObject3D = (parent) => (child) => () => parent.remove(child);
var removeFromParent = (obj3D) => () => obj3D.removeFromParent();
var setScaleOfAnything = (thing) => (x) => (y) => (z) => () => thing.scale.set(x, y, z);
var playAnything = (thing) => () => thing.play();
var newHemisphereLight = (skyColor) => (groundColor) => (intensity) => () => new THREE.HemisphereLight(skyColor, groundColor, intensity);
var setGroundColor = (x) => (y) => () => x.groundColor = new THREE.Color(y);
var newAmbientLight = (rgb) => (intensity) => () => new THREE.AmbientLight(rgb, intensity);
var newDirectionalLight = (rgb) => (intensity) => () => new THREE.DirectionalLight(rgb, intensity);
var newPointLight = (rgb) => (intensity) => (distance) => (decay) => () => new THREE.PointLight(rgb, intensity, distance, decay);
var newRectAreaLight = (rgb) => (intensity) => (width) => (height) => () => new THREE.RectAreaLight(rgb, intensity, width, height);
var setWidth = (x) => (y) => () => x.width = y;
var setHeight = (x) => (y) => () => x.height = y;
var newSpotLight = (rgb) => (intensity) => (distance) => (angle) => (penumbra) => (decay) => () => new THREE.SpotLight(rgb, intensity, distance, angle, penumbra, decay);
var setAngle = (x) => (y) => () => x.angle = y;
var setPenumbra = (x) => (y) => () => x.penumbra = y;
var windowInnerWidth = () => window.innerWidth;
var windowInnerHeight = () => window.innerHeight;
var newAnimationMixer = (object3D) => () => new THREE.AnimationMixer(object3D);
var updateAnimationMixer = (mixer) => (delta) => () => mixer.update(delta);
var clipAction = (animationMixer) => (clip) => () => animationMixer.clipAction(clip);
var setEffectiveTimeScale = (animationAction) => (t) => () => animationAction.setEffectiveTimeScale(t);
var setDuration = (animationAction) => (durationInSeconds) => () => animationAction.setDuration(durationInSeconds);
var stop = (animationAction) => () => animationAction.stop();
var newMeshPhongMaterial = (parameters) => () => new THREE.MeshPhongMaterial(parameters);
var clampToEdgeWrapping = THREE.ClampToEdgeWrapping;
var repeatWrapping = THREE.RepeatWrapping;
var mirroredRepeatWrapping = THREE.MirroredRepeatWrapping;
var nearestFilter = THREE.NearestFilter;
var linearFilter = THREE.LinearFilter;

// output/ThreeJS.Unsafe/foreign.js
var lookAt = (thing) => (x) => (y) => (z) => () => thing.lookAt(x, y, z);
var setPosition = (thing) => (x) => (y) => (z) => () => thing.position.set(x, y, z);
var getRotationX = (thing) => () => thing.rotation.x;
var getRotationY = (thing) => () => thing.rotation.y;
var getRotationZ = (thing) => () => thing.rotation.z;
var setRotation = (thing) => (x) => (y) => (z) => () => thing.rotation.set(x, y, z);
var setLightIntensity = (l) => (x) => () => l.intensity = x;
var setDistance = (x) => (y) => () => x.distance = y;
var setDecay = (x) => (y) => () => x.decay = y;
var setTarget = (a) => (t) => () => a.target = t;

// output/ThreeJS/index.js
var setTarget2 = function() {
  return function() {
    return setTarget;
  };
};
var setRotation2 = function() {
  return setRotation;
};
var setPosition2 = function() {
  return setPosition;
};
var setLightIntensity2 = function() {
  return setLightIntensity;
};
var setDistance2 = function() {
  return setDistance;
};
var setDecay2 = function() {
  return setDecay;
};
var lookAt2 = function() {
  return lookAt;
};
var loadGLTF_DRACO = function(pathToDracoModules) {
  return function(url) {
    return function(cb) {
      return function __do2() {
        var gltfLoader = newGLTFLoader();
        var dracoLoader = newDRACOLoader();
        setDecoderPath(dracoLoader)(pathToDracoModules)();
        setDRACOLoader(gltfLoader)(dracoLoader)();
        loadGLTF1(gltfLoader)(url)(cb)();
        return gltfLoader;
      };
    };
  };
};
var getRotationZ2 = function() {
  return getRotationZ;
};
var getRotationY2 = function() {
  return getRotationY;
};
var getRotationX2 = function() {
  return getRotationX;
};

// output/ElementType/index.js
var Dancer = /* @__PURE__ */ function() {
  function Dancer2() {
  }
  ;
  Dancer2.value = new Dancer2();
  return Dancer2;
}();
var Plane = /* @__PURE__ */ function() {
  function Plane2() {
  }
  ;
  Plane2.value = new Plane2();
  return Plane2;
}();
var Ambient = /* @__PURE__ */ function() {
  function Ambient2() {
  }
  ;
  Ambient2.value = new Ambient2();
  return Ambient2;
}();
var Directional = /* @__PURE__ */ function() {
  function Directional2() {
  }
  ;
  Directional2.value = new Directional2();
  return Directional2;
}();
var Hemisphere = /* @__PURE__ */ function() {
  function Hemisphere2() {
  }
  ;
  Hemisphere2.value = new Hemisphere2();
  return Hemisphere2;
}();
var Point = /* @__PURE__ */ function() {
  function Point2() {
  }
  ;
  Point2.value = new Point2();
  return Point2;
}();
var RectArea = /* @__PURE__ */ function() {
  function RectArea2() {
  }
  ;
  RectArea2.value = new RectArea2();
  return RectArea2;
}();
var Spot = /* @__PURE__ */ function() {
  function Spot2() {
  }
  ;
  Spot2.value = new Spot2();
  return Spot2;
}();
var eqElementType = {
  eq: function(x) {
    return function(y) {
      if (x instanceof Dancer && y instanceof Dancer) {
        return true;
      }
      ;
      if (x instanceof Plane && y instanceof Plane) {
        return true;
      }
      ;
      if (x instanceof Ambient && y instanceof Ambient) {
        return true;
      }
      ;
      if (x instanceof Directional && y instanceof Directional) {
        return true;
      }
      ;
      if (x instanceof Hemisphere && y instanceof Hemisphere) {
        return true;
      }
      ;
      if (x instanceof Point && y instanceof Point) {
        return true;
      }
      ;
      if (x instanceof RectArea && y instanceof RectArea) {
        return true;
      }
      ;
      if (x instanceof Spot && y instanceof Spot) {
        return true;
      }
      ;
      return false;
    };
  }
};
var isLight = function(v) {
  if (v instanceof Ambient) {
    return true;
  }
  ;
  if (v instanceof Directional) {
    return true;
  }
  ;
  if (v instanceof Hemisphere) {
    return true;
  }
  ;
  if (v instanceof Point) {
    return true;
  }
  ;
  if (v instanceof RectArea) {
    return true;
  }
  ;
  if (v instanceof Spot) {
    return true;
  }
  ;
  return false;
};

// output/Variable/index.js
var ConstantVariable = /* @__PURE__ */ function() {
  function ConstantVariable2(value0) {
    this.value0 = value0;
  }
  ;
  ConstantVariable2.create = function(value0) {
    return new ConstantVariable2(value0);
  };
  return ConstantVariable2;
}();
var Osc = /* @__PURE__ */ function() {
  function Osc3(value0) {
    this.value0 = value0;
  }
  ;
  Osc3.create = function(value0) {
    return new Osc3(value0);
  };
  return Osc3;
}();
var Sum = /* @__PURE__ */ function() {
  function Sum3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Sum3.create = function(value0) {
    return function(value1) {
      return new Sum3(value0, value1);
    };
  };
  return Sum3;
}();
var Sub = /* @__PURE__ */ function() {
  function Sub2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Sub2.create = function(value0) {
    return function(value1) {
      return new Sub2(value0, value1);
    };
  };
  return Sub2;
}();
var Product2 = /* @__PURE__ */ function() {
  function Product4(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Product4.create = function(value0) {
    return function(value1) {
      return new Product4(value0, value1);
    };
  };
  return Product4;
}();
var Divide = /* @__PURE__ */ function() {
  function Divide3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Divide3.create = function(value0) {
    return function(value1) {
      return new Divide3(value0, value1);
    };
  };
  return Divide3;
}();
var semiringVariable = /* @__PURE__ */ function() {
  return {
    zero: new ConstantVariable(0),
    one: new ConstantVariable(1),
    add: function(x) {
      return function(y) {
        return new Sum(x, y);
      };
    },
    mul: function(x) {
      return function(y) {
        return new Product2(x, y);
      };
    }
  };
}();
var ringVariable = {
  sub: function(x) {
    return function(y) {
      return new Sub(x, y);
    };
  },
  Semiring0: function() {
    return semiringVariable;
  }
};
var safeDivide = function(v) {
  return function(v1) {
    if (v1 === 0) {
      return 0;
    }
    ;
    return v / v1;
  };
};
var osc = function(nCycles) {
  return function(f) {
    return sin(f * nCycles * 2 * pi);
  };
};
var realizeVariable = function(v) {
  return function(v1) {
    if (v1 instanceof ConstantVariable) {
      return v1.value0;
    }
    ;
    if (v1 instanceof Osc) {
      return osc(v)(realizeVariable(v)(v1.value0));
    }
    ;
    if (v1 instanceof Sum) {
      return realizeVariable(v)(v1.value0) + realizeVariable(v)(v1.value1);
    }
    ;
    if (v1 instanceof Sub) {
      return realizeVariable(v)(v1.value0) - realizeVariable(v)(v1.value1);
    }
    ;
    if (v1 instanceof Product2) {
      return realizeVariable(v)(v1.value0) * realizeVariable(v)(v1.value1);
    }
    ;
    if (v1 instanceof Divide) {
      return safeDivide(realizeVariable(v)(v1.value0))(realizeVariable(v)(v1.value1));
    }
    ;
    throw new Error("Failed pattern match at Variable (line 19, column 1 - line 19, column 48): " + [v.constructor.name, v1.constructor.name]);
  };
};

// output/Value/index.js
var show2 = /* @__PURE__ */ show(showNumber);
var show22 = /* @__PURE__ */ show(showInt);
var show3 = /* @__PURE__ */ show(showBoolean);
var pure2 = /* @__PURE__ */ pure(applicativeEither);
var add2 = /* @__PURE__ */ add(semiringVariable);
var mul2 = /* @__PURE__ */ mul(semiringVariable);
var sub2 = /* @__PURE__ */ sub(ringVariable);
var union2 = /* @__PURE__ */ union(ordString);
var identity9 = /* @__PURE__ */ identity(categoryFn);
var lookup2 = /* @__PURE__ */ lookup(ordString);
var bind2 = /* @__PURE__ */ bind(bindEither);
var ValueNumber = /* @__PURE__ */ function() {
  function ValueNumber2(value0) {
    this.value0 = value0;
  }
  ;
  ValueNumber2.create = function(value0) {
    return new ValueNumber2(value0);
  };
  return ValueNumber2;
}();
var ValueString = /* @__PURE__ */ function() {
  function ValueString2(value0) {
    this.value0 = value0;
  }
  ;
  ValueString2.create = function(value0) {
    return new ValueString2(value0);
  };
  return ValueString2;
}();
var ValueInt = /* @__PURE__ */ function() {
  function ValueInt2(value0) {
    this.value0 = value0;
  }
  ;
  ValueInt2.create = function(value0) {
    return new ValueInt2(value0);
  };
  return ValueInt2;
}();
var ValueBoolean = /* @__PURE__ */ function() {
  function ValueBoolean2(value0) {
    this.value0 = value0;
  }
  ;
  ValueBoolean2.create = function(value0) {
    return new ValueBoolean2(value0);
  };
  return ValueBoolean2;
}();
var ValueVariable = /* @__PURE__ */ function() {
  function ValueVariable2(value0) {
    this.value0 = value0;
  }
  ;
  ValueVariable2.create = function(value0) {
    return new ValueVariable2(value0);
  };
  return ValueVariable2;
}();
var ValueTransformer = /* @__PURE__ */ function() {
  function ValueTransformer2(value0) {
    this.value0 = value0;
  }
  ;
  ValueTransformer2.create = function(value0) {
    return new ValueTransformer2(value0);
  };
  return ValueTransformer2;
}();
var ValueFunction = /* @__PURE__ */ function() {
  function ValueFunction2(value0) {
    this.value0 = value0;
  }
  ;
  ValueFunction2.create = function(value0) {
    return new ValueFunction2(value0);
  };
  return ValueFunction2;
}();
var ValueElement = /* @__PURE__ */ function() {
  function ValueElement2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  ValueElement2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new ValueElement2(value0, value1, value2);
      };
    };
  };
  return ValueElement2;
}();
var ValueCamera = /* @__PURE__ */ function() {
  function ValueCamera2() {
  }
  ;
  ValueCamera2.value = new ValueCamera2();
  return ValueCamera2;
}();
var ValueClear = /* @__PURE__ */ function() {
  function ValueClear2() {
  }
  ;
  ValueClear2.value = new ValueClear2();
  return ValueClear2;
}();
var valueToVariable = function(v) {
  if (v instanceof ValueNumber) {
    return new ConstantVariable(v.value0);
  }
  ;
  if (v instanceof ValueInt) {
    return new ConstantVariable(toNumber(v.value0));
  }
  ;
  if (v instanceof ValueBoolean && v.value0) {
    return new ConstantVariable(1);
  }
  ;
  if (v instanceof ValueVariable) {
    return v.value0;
  }
  ;
  return new ConstantVariable(0);
};
var valueToString = function(v) {
  if (v instanceof ValueNumber) {
    return show2(v.value0);
  }
  ;
  if (v instanceof ValueString) {
    return v.value0;
  }
  ;
  if (v instanceof ValueInt) {
    return show22(v.value0);
  }
  ;
  if (v instanceof ValueBoolean) {
    return show3(v.value0);
  }
  ;
  return "";
};
var valueToNumber = function(v) {
  if (v instanceof ValueNumber) {
    return v.value0;
  }
  ;
  if (v instanceof ValueInt) {
    return toNumber(v.value0);
  }
  ;
  if (v instanceof ValueBoolean && v.value0) {
    return 1;
  }
  ;
  if (v instanceof ValueBoolean && !v.value0) {
    return 0;
  }
  ;
  return 0;
};
var semiringValue = /* @__PURE__ */ function() {
  return {
    zero: new ValueInt(0),
    one: new ValueInt(1),
    add: function(v) {
      return function(v1) {
        if (v instanceof ValueVariable && v1 instanceof ValueVariable) {
          return new ValueVariable(add2(v.value0)(v1.value0));
        }
        ;
        if (v instanceof ValueVariable) {
          return new ValueVariable(add2(v.value0)(new ConstantVariable(valueToNumber(v1))));
        }
        ;
        if (v1 instanceof ValueVariable) {
          return new ValueVariable(add2(new ConstantVariable(valueToNumber(v)))(v1.value0));
        }
        ;
        if (v instanceof ValueInt && v1 instanceof ValueInt) {
          return new ValueInt(v.value0 + v1.value0 | 0);
        }
        ;
        return new ValueNumber(valueToNumber(v) + valueToNumber(v1));
      };
    },
    mul: function(v) {
      return function(v1) {
        if (v instanceof ValueVariable && v1 instanceof ValueVariable) {
          return new ValueVariable(mul2(v.value0)(v1.value0));
        }
        ;
        if (v instanceof ValueVariable) {
          return new ValueVariable(mul2(v.value0)(new ConstantVariable(valueToNumber(v1))));
        }
        ;
        if (v1 instanceof ValueVariable) {
          return new ValueVariable(mul2(new ConstantVariable(valueToNumber(v)))(v1.value0));
        }
        ;
        if (v instanceof ValueInt && v1 instanceof ValueInt) {
          return new ValueInt(v.value0 * v1.value0 | 0);
        }
        ;
        return new ValueNumber(valueToNumber(v) * valueToNumber(v1));
      };
    }
  };
}();
var ringValue = {
  sub: function(v) {
    return function(v1) {
      if (v instanceof ValueVariable && v1 instanceof ValueVariable) {
        return new ValueVariable(sub2(v.value0)(v1.value0));
      }
      ;
      if (v instanceof ValueVariable) {
        return new ValueVariable(sub2(v.value0)(new ConstantVariable(valueToNumber(v1))));
      }
      ;
      if (v1 instanceof ValueVariable) {
        return new ValueVariable(sub2(new ConstantVariable(valueToNumber(v)))(v1.value0));
      }
      ;
      if (v instanceof ValueInt && v1 instanceof ValueInt) {
        return new ValueInt(v.value0 - v1.value0 | 0);
      }
      ;
      return new ValueNumber(valueToNumber(v) - valueToNumber(v1));
    };
  },
  Semiring0: function() {
    return semiringValue;
  }
};
var valueToInt = function(v) {
  if (v instanceof ValueNumber) {
    return floor2(v.value0);
  }
  ;
  if (v instanceof ValueInt) {
    return v.value0;
  }
  ;
  if (v instanceof ValueBoolean && v.value0) {
    return 1;
  }
  ;
  if (v instanceof ValueBoolean && !v.value0) {
    return 0;
  }
  ;
  return 0;
};
var valueToBoolean = function(v) {
  if (v instanceof ValueNumber) {
    return v.value0 !== 0;
  }
  ;
  if (v instanceof ValueString && v.value0 === "true") {
    return true;
  }
  ;
  if (v instanceof ValueInt) {
    return v.value0 !== 0;
  }
  ;
  if (v instanceof ValueBoolean) {
    return v.value0;
  }
  ;
  return false;
};
var valueMapToTransformer = function(vmNew) {
  return function(vmOld) {
    return pure2(union2(vmNew)(vmOld));
  };
};
var lookupValue = function(d) {
  return function(k) {
    return function(m) {
      return maybe(d)(identity9)(lookup2(k)(m));
    };
  };
};
var lookupString = function(d) {
  return function(k) {
    return function(m) {
      return valueToString(lookupValue(new ValueString(d))(k)(m));
    };
  };
};
var lookupNumber = function(d) {
  return function(k) {
    return function(m) {
      return valueToNumber(lookupValue(new ValueNumber(d))(k)(m));
    };
  };
};
var lookupInt = function(d) {
  return function(k) {
    return function(m) {
      return valueToInt(lookupValue(new ValueInt(d))(k)(m));
    };
  };
};
var lookupBoolean = function(d) {
  return function(k) {
    return function(m) {
      return valueToBoolean(lookupValue(new ValueBoolean(d))(k)(m));
    };
  };
};
var divideValues = function(x) {
  return function(y) {
    var f = function(v) {
      return function(v1) {
        if (v1 === 0) {
          return 0;
        }
        ;
        return v / v1;
      };
    };
    return new ValueNumber(f(valueToNumber(x))(valueToNumber(y)));
  };
};
var appendTransformers = function(fx) {
  return function(fy) {
    return function(thisMap) {
      return bind2(fx(thisMap))(fy);
    };
  };
};

// output/Model/index.js
var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
var elemIndex2 = /* @__PURE__ */ elemIndex(eqString);
var map5 = /* @__PURE__ */ map(functorArray);
var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEffect);
var intToMixerState = function(nAnimations) {
  return function(n) {
    var n$prime = mod2(n)(nAnimations);
    var allZeros = replicate(nAnimations)(0);
    return fromMaybe(allZeros)(updateAt(n$prime)(1)(allZeros));
  };
};
var valueToMixerState = function(v) {
  return function(v1) {
    if (v1 instanceof ValueInt) {
      return intToMixerState(length(v.actions))(v1.value0);
    }
    ;
    if (v1 instanceof ValueNumber) {
      return intToMixerState(length(v.actions))(floor2(v1.value0));
    }
    ;
    if (v1 instanceof ValueString) {
      var n$prime = fromMaybe(0)(elemIndex2(v1.value0)(v.clipNames));
      var allZeros = replicate(length(v.actions))(0);
      return fromMaybe(allZeros)(updateAt(n$prime)(1)(allZeros));
    }
    ;
    return [];
  };
};
var gltfToModel = function(gltf) {
  var clipNames = map5(function(v) {
    return v.name;
  })(gltf.animations);
  return function __do2() {
    var mixer = newAnimationMixer(gltf.scene)();
    var actions = traverse2(clipAction(mixer))(gltf.animations)();
    var mixerState = $$new([])();
    var durState = $$new(-999)();
    return {
      scene: gltf.scene,
      clips: gltf.animations,
      clipNames,
      mixer,
      actions,
      mixerState,
      durState
    };
  };
};

// output/R/index.js
var monadReaderT2 = /* @__PURE__ */ monadReaderT(monadEffect);
var bindStateT2 = /* @__PURE__ */ bindStateT(monadReaderT2);
var bind3 = /* @__PURE__ */ bind(bindStateT2);
var ask2 = /* @__PURE__ */ ask(/* @__PURE__ */ monadAskStateT(/* @__PURE__ */ monadAskReaderT(monadEffect)));
var pure3 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadReaderT2));
var liftEffect2 = /* @__PURE__ */ liftEffect(/* @__PURE__ */ monadEffectState(/* @__PURE__ */ monadEffectReader(monadEffectEffect)));
var setPosition3 = /* @__PURE__ */ setPosition2();
var lookup3 = /* @__PURE__ */ lookup(ordString);
var discard2 = /* @__PURE__ */ discard(discardUnit)(bindStateT2);
var lookAt3 = /* @__PURE__ */ lookAt2();
var getRotationX3 = /* @__PURE__ */ getRotationX2();
var getRotationY3 = /* @__PURE__ */ getRotationY2();
var getRotationZ3 = /* @__PURE__ */ getRotationZ2();
var setRotation3 = /* @__PURE__ */ setRotation2();
var execStateT2 = /* @__PURE__ */ execStateT(/* @__PURE__ */ functorReaderT(functorEffect));
var ElementDancer = /* @__PURE__ */ function() {
  function ElementDancer2(value0) {
    this.value0 = value0;
  }
  ;
  ElementDancer2.create = function(value0) {
    return new ElementDancer2(value0);
  };
  return ElementDancer2;
}();
var ElementPlane = /* @__PURE__ */ function() {
  function ElementPlane2(value0) {
    this.value0 = value0;
  }
  ;
  ElementPlane2.create = function(value0) {
    return new ElementPlane2(value0);
  };
  return ElementPlane2;
}();
var ElementAmbient = /* @__PURE__ */ function() {
  function ElementAmbient2(value0) {
    this.value0 = value0;
  }
  ;
  ElementAmbient2.create = function(value0) {
    return new ElementAmbient2(value0);
  };
  return ElementAmbient2;
}();
var ElementDirectional = /* @__PURE__ */ function() {
  function ElementDirectional2(value0) {
    this.value0 = value0;
  }
  ;
  ElementDirectional2.create = function(value0) {
    return new ElementDirectional2(value0);
  };
  return ElementDirectional2;
}();
var ElementHemisphere = /* @__PURE__ */ function() {
  function ElementHemisphere2(value0) {
    this.value0 = value0;
  }
  ;
  ElementHemisphere2.create = function(value0) {
    return new ElementHemisphere2(value0);
  };
  return ElementHemisphere2;
}();
var ElementPoint = /* @__PURE__ */ function() {
  function ElementPoint2(value0) {
    this.value0 = value0;
  }
  ;
  ElementPoint2.create = function(value0) {
    return new ElementPoint2(value0);
  };
  return ElementPoint2;
}();
var ElementRectArea = /* @__PURE__ */ function() {
  function ElementRectArea2(value0) {
    this.value0 = value0;
  }
  ;
  ElementRectArea2.create = function(value0) {
    return new ElementRectArea2(value0);
  };
  return ElementRectArea2;
}();
var ElementSpot = /* @__PURE__ */ function() {
  function ElementSpot2(value0) {
    this.value0 = value0;
  }
  ;
  ElementSpot2.create = function(value0) {
    return new ElementSpot2(value0);
  };
  return ElementSpot2;
}();
var realizeNumber = function(k) {
  return function(def) {
    return function(valueMap) {
      var v = lookupValue(new ValueNumber(def))(k)(valueMap);
      if (v instanceof ValueVariable) {
        return bind3(ask2)(function(env) {
          return pure3(realizeVariable(env.nCycles)(v.value0));
        });
      }
      ;
      return pure3(valueToNumber(v));
    };
  };
};
var updatePosition = function() {
  return function(vm) {
    return function(a) {
      return bind3(realizeNumber("x")(0)(vm))(function(x) {
        return bind3(realizeNumber("y")(0)(vm))(function(y) {
          return bind3(realizeNumber("z")(0)(vm))(function(z) {
            return liftEffect2(setPosition3(a)(x)(y)(z));
          });
        });
      });
    };
  };
};
var updateRotation = function() {
  return function(vm) {
    return function(a) {
      var mlx = lookup3("lx")(vm);
      var mly = lookup3("ly")(vm);
      var mlz = lookup3("lz")(vm);
      var v = isJust(mlx) || (isJust(mly) || isJust(mlz));
      if (v) {
        return bind3(realizeNumber("lx")(0)(vm))(function(lx) {
          return bind3(realizeNumber("ly")(0)(vm))(function(ly) {
            return bind3(realizeNumber("lz")(0)(vm))(function(lz) {
              return bind3(realizeNumber("rx")(0)(vm))(function(rxDelta) {
                return bind3(realizeNumber("ry")(0)(vm))(function(ryDelta) {
                  return bind3(realizeNumber("rz")(0)(vm))(function(rzDelta) {
                    return discard2(liftEffect2(lookAt3(a)(lx)(ly)(lz)))(function() {
                      return bind3(liftEffect2(getRotationX3(a)))(function(rx0) {
                        return bind3(liftEffect2(getRotationY3(a)))(function(ry0) {
                          return bind3(liftEffect2(getRotationZ3(a)))(function(rz0) {
                            var rx = rx0 + rxDelta * pi / 180;
                            var ry = ry0 + ryDelta * pi / 180;
                            var rz = rz0 + rzDelta * pi / 180;
                            return liftEffect2(setRotation3(a)(rx)(ry)(rz));
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (!v) {
        return bind3(realizeNumber("rx")(0)(vm))(function(rx) {
          return bind3(realizeNumber("ry")(0)(vm))(function(ry) {
            return bind3(realizeNumber("rz")(0)(vm))(function(rz) {
              var rx$prime = rx * pi / 180;
              var ry$prime = ry * pi / 180;
              var rz$prime = rz * pi / 180;
              return liftEffect2(setRotation3(a)(rx$prime)(ry$prime)(rz$prime));
            });
          });
        });
      }
      ;
      throw new Error("Failed pattern match at R (line 91, column 3 - line 115, column 51): " + [v.constructor.name]);
    };
  };
};
var updateScale = function() {
  return function(vm) {
    return function(a) {
      return bind3(realizeNumber("sx")(1)(vm))(function(sx) {
        return bind3(realizeNumber("sy")(1)(vm))(function(sy) {
          return bind3(realizeNumber("sz")(1)(vm))(function(sz) {
            return bind3(realizeNumber("size")(1)(vm))(function(size2) {
              return liftEffect2(setScaleOfAnything(a)(sx * size2)(sy * size2)(sz * size2));
            });
          });
        });
      });
    };
  };
};
var execR = function(rEnv) {
  return function(zState) {
    return function(r) {
      return runReaderT(execStateT2(r)(zState))(rEnv);
    };
  };
};
var elementType = function(v) {
  if (v instanceof ElementDancer) {
    return Dancer.value;
  }
  ;
  if (v instanceof ElementPlane) {
    return Plane.value;
  }
  ;
  if (v instanceof ElementAmbient) {
    return Ambient.value;
  }
  ;
  if (v instanceof ElementDirectional) {
    return Directional.value;
  }
  ;
  if (v instanceof ElementHemisphere) {
    return Hemisphere.value;
  }
  ;
  if (v instanceof ElementPoint) {
    return Point.value;
  }
  ;
  if (v instanceof ElementRectArea) {
    return RectArea.value;
  }
  ;
  if (v instanceof ElementSpot) {
    return Spot.value;
  }
  ;
  throw new Error("Failed pattern match at R (line 128, column 1 - line 128, column 38): " + [v.constructor.name]);
};
var defaultZoneState = {
  elements: []
};

// output/Data.String.CodePoints/foreign.js
var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";
var _unsafeCodePointAt0 = function(fallback) {
  return hasCodePointAt ? function(str) {
    return str.codePointAt(0);
  } : fallback;
};
var _codePointAt = function(fallback) {
  return function(Just2) {
    return function(Nothing2) {
      return function(unsafeCodePointAt02) {
        return function(index3) {
          return function(str) {
            var length5 = str.length;
            if (index3 < 0 || index3 >= length5)
              return Nothing2;
            if (hasStringIterator) {
              var iter = str[Symbol.iterator]();
              for (var i = index3; ; --i) {
                var o = iter.next();
                if (o.done)
                  return Nothing2;
                if (i === 0)
                  return Just2(unsafeCodePointAt02(o.value));
              }
            }
            return fallback(index3)(str);
          };
        };
      };
    };
  };
};
var _fromCodePointArray = function(singleton8) {
  return hasFromCodePoint ? function(cps) {
    if (cps.length < 1e4) {
      return String.fromCodePoint.apply(String, cps);
    }
    return cps.map(singleton8).join("");
  } : function(cps) {
    return cps.map(singleton8).join("");
  };
};
var _singleton = function(fallback) {
  return hasFromCodePoint ? String.fromCodePoint : fallback;
};
var _take = function(fallback) {
  return function(n) {
    if (hasStringIterator) {
      return function(str) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done)
            return accum;
          accum += o.value;
        }
        return accum;
      };
    }
    return fallback(n);
  };
};
var _toCodePointArray = function(fallback) {
  return function(unsafeCodePointAt02) {
    if (hasArrayFrom) {
      return function(str) {
        return Array.from(str, unsafeCodePointAt02);
      };
    }
    return fallback;
  };
};

// output/Data.String.CodeUnits/foreign.js
var fromCharArray = function(a) {
  return a.join("");
};
var toCharArray = function(s) {
  return s.split("");
};
var singleton4 = function(c) {
  return c;
};
var _toChar = function(just) {
  return function(nothing) {
    return function(s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};
var length2 = function(s) {
  return s.length;
};
var drop2 = function(n) {
  return function(s) {
    return s.substring(n);
  };
};
var splitAt = function(i) {
  return function(s) {
    return { before: s.substring(0, i), after: s.substring(i) };
  };
};

// output/Data.String.Unsafe/foreign.js
var charAt = function(i) {
  return function(s) {
    if (i >= 0 && i < s.length)
      return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

// output/Data.String.CodeUnits/index.js
var uncons2 = function(v) {
  if (v === "") {
    return Nothing.value;
  }
  ;
  return new Just({
    head: charAt(0)(v),
    tail: drop2(1)(v)
  });
};
var toChar = /* @__PURE__ */ function() {
  return _toChar(Just.create)(Nothing.value);
}();
var stripPrefix = function(v) {
  return function(str) {
    var v1 = splitAt(length2(v))(str);
    var $20 = v1.before === v;
    if ($20) {
      return new Just(v1.after);
    }
    ;
    return Nothing.value;
  };
};

// output/Data.String.Common/foreign.js
var toLower = function(s) {
  return s.toLowerCase();
};
var trim = function(s) {
  return s.trim();
};

// output/Data.String.Common/index.js
var $$null = function(s) {
  return s === "";
};

// output/Data.String.CodePoints/index.js
var $runtime_lazy6 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var fromEnum8 = /* @__PURE__ */ fromEnum(boundedEnumChar);
var map6 = /* @__PURE__ */ map(functorMaybe);
var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
var div2 = /* @__PURE__ */ div(euclideanRingInt);
var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
var compare2 = /* @__PURE__ */ compare(ordInt);
var CodePoint = function(x) {
  return x;
};
var unsurrogate = function(lead) {
  return function(trail) {
    return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
  };
};
var isTrail = function(cu) {
  return 56320 <= cu && cu <= 57343;
};
var isLead = function(cu) {
  return 55296 <= cu && cu <= 56319;
};
var uncons3 = function(s) {
  var v = length2(s);
  if (v === 0) {
    return Nothing.value;
  }
  ;
  if (v === 1) {
    return new Just({
      head: fromEnum8(charAt(0)(s)),
      tail: ""
    });
  }
  ;
  var cu1 = fromEnum8(charAt(1)(s));
  var cu0 = fromEnum8(charAt(0)(s));
  var $43 = isLead(cu0) && isTrail(cu1);
  if ($43) {
    return new Just({
      head: unsurrogate(cu0)(cu1),
      tail: drop2(2)(s)
    });
  }
  ;
  return new Just({
    head: cu0,
    tail: drop2(1)(s)
  });
};
var unconsButWithTuple = function(s) {
  return map6(function(v) {
    return new Tuple(v.head, v.tail);
  })(uncons3(s));
};
var toCodePointArrayFallback = function(s) {
  return unfoldr2(unconsButWithTuple)(s);
};
var unsafeCodePointAt0Fallback = function(s) {
  var cu0 = fromEnum8(charAt(0)(s));
  var $47 = isLead(cu0) && length2(s) > 1;
  if ($47) {
    var cu1 = fromEnum8(charAt(1)(s));
    var $48 = isTrail(cu1);
    if ($48) {
      return unsurrogate(cu0)(cu1);
    }
    ;
    return cu0;
  }
  ;
  return cu0;
};
var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
var fromCharCode2 = /* @__PURE__ */ function() {
  var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
  return function($76) {
    return singleton4($75($76));
  };
}();
var singletonFallback = function(v) {
  if (v <= 65535) {
    return fromCharCode2(v);
  }
  ;
  var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
  var trail = mod3(v - 65536 | 0)(1024) + 56320 | 0;
  return fromCharCode2(lead) + fromCharCode2(trail);
};
var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
var singleton5 = /* @__PURE__ */ _singleton(singletonFallback);
var takeFallback = function(v) {
  return function(v1) {
    if (v < 1) {
      return "";
    }
    ;
    var v2 = uncons3(v1);
    if (v2 instanceof Just) {
      return singleton5(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
    }
    ;
    return v1;
  };
};
var take3 = /* @__PURE__ */ _take(takeFallback);
var eqCodePoint = {
  eq: function(x) {
    return function(y) {
      return x === y;
    };
  }
};
var ordCodePoint = {
  compare: function(x) {
    return function(y) {
      return compare2(x)(y);
    };
  },
  Eq0: function() {
    return eqCodePoint;
  }
};
var codePointFromChar = function($77) {
  return CodePoint(fromEnum8($77));
};
var codePointAtFallback = function($copy_n) {
  return function($copy_s) {
    var $tco_var_n = $copy_n;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(n, s) {
      var v = uncons3(s);
      if (v instanceof Just) {
        var $66 = n === 0;
        if ($66) {
          $tco_done = true;
          return new Just(v.value0.head);
        }
        ;
        $tco_var_n = n - 1 | 0;
        $copy_s = v.value0.tail;
        return;
      }
      ;
      $tco_done = true;
      return Nothing.value;
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_n, $copy_s);
    }
    ;
    return $tco_result;
  };
};
var codePointAt = function(v) {
  return function(v1) {
    if (v < 0) {
      return Nothing.value;
    }
    ;
    if (v === 0 && v1 === "") {
      return Nothing.value;
    }
    ;
    if (v === 0) {
      return new Just(unsafeCodePointAt0(v1));
    }
    ;
    return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
  };
};
var boundedCodePoint = {
  bottom: 0,
  top: 1114111,
  Ord0: function() {
    return ordCodePoint;
  }
};
var boundedEnumCodePoint = /* @__PURE__ */ function() {
  return {
    cardinality: 1114111 + 1 | 0,
    fromEnum: function(v) {
      return v;
    },
    toEnum: function(n) {
      if (n >= 0 && n <= 1114111) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
    },
    Bounded0: function() {
      return boundedCodePoint;
    },
    Enum1: function() {
      return $lazy_enumCodePoint(0);
    }
  };
}();
var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy6("enumCodePoint", "Data.String.CodePoints", function() {
  return {
    succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    Ord0: function() {
      return ordCodePoint;
    }
  };
});

// output/URL/index.js
var defaultAssets = "https://dktr0.github.io/LocoMotion/models/";
var resolveURL = function(x) {
  var f = function(x1) {
    if (take3(7)(x1) === "http://") {
      return x1;
    }
    ;
    if (take3(8)(x1) === "https://") {
      return x1;
    }
    ;
    if (otherwise) {
      return defaultAssets + x1;
    }
    ;
    throw new Error("Failed pattern match at URL (line 10, column 3 - line 12, column 39): " + [x1.constructor.name]);
  };
  return f(trim(x));
};

// output/Dancer/index.js
var discard3 = /* @__PURE__ */ discard(discardUnit);
var monadReaderT3 = /* @__PURE__ */ monadReaderT(monadEffect);
var bindStateT3 = /* @__PURE__ */ bindStateT(monadReaderT3);
var bind4 = /* @__PURE__ */ bind(bindStateT3);
var ask3 = /* @__PURE__ */ ask(/* @__PURE__ */ monadAskStateT(/* @__PURE__ */ monadAskReaderT(monadEffect)));
var monadEffectState2 = /* @__PURE__ */ monadEffectState(/* @__PURE__ */ monadEffectReader(monadEffectEffect));
var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectState2);
var whenMaybeRef2 = /* @__PURE__ */ whenMaybeRef(monadEffectEffect);
var unwrap3 = /* @__PURE__ */ unwrap();
var mod4 = /* @__PURE__ */ mod(euclideanRingInt);
var applicativeStateT2 = /* @__PURE__ */ applicativeStateT(monadReaderT3);
var pure1 = /* @__PURE__ */ pure(applicativeStateT2);
var show4 = /* @__PURE__ */ show(showInt);
var show1 = /* @__PURE__ */ show(showString);
var discard22 = /* @__PURE__ */ discard3(bindStateT3);
var traverseWithIndex_2 = /* @__PURE__ */ traverseWithIndex_(applicativeEffect)(foldableWithIndexArray);
var when2 = /* @__PURE__ */ when(applicativeStateT2);
var lookup4 = /* @__PURE__ */ lookup(ordString);
var whenMaybeRef1 = /* @__PURE__ */ whenMaybeRef(monadEffectState2);
var notEq1 = /* @__PURE__ */ notEq(/* @__PURE__ */ eqArray(eqNumber));
var updatePosition2 = /* @__PURE__ */ updatePosition();
var updateScale2 = /* @__PURE__ */ updateScale();
var updateRotation2 = /* @__PURE__ */ updateRotation();
var updateAnimationAction = function(m) {
  return function(dur) {
    return function(i) {
      return function(weight) {
        var v = index(m.actions)(i);
        if (v instanceof Just) {
          if (weight === 0) {
            return stop(v.value0);
          }
          ;
          return function __do2() {
            setEffectiveTimeScale(v.value0)(1)();
            playAnything(v.value0)();
            return setDuration(v.value0)(dur)();
          };
        }
        ;
        if (v instanceof Nothing) {
          return log2("strange error in LocoMotion: updateAnimationAction, should not be possible");
        }
        ;
        throw new Error("Failed pattern match at Dancer (line 122, column 3 - line 133, column 96): " + [v.constructor.name]);
      };
    };
  };
};
var removeDancer = function(d) {
  return bind4(ask3)(function(env) {
    return liftEffect3(whenMaybeRef2(d.model)(function(m) {
      return removeObject3D(env.scene)(m.scene);
    }));
  });
};
var newDancer = /* @__PURE__ */ liftEffect3(function __do() {
  var url = $$new("")();
  var model = $$new(Nothing.value)();
  return {
    url,
    model
  };
});
var models = ["cactus.glb", "Daffy.glb", "Lily.glb", "NatureGirl.glb", "StoneFigure.glb", "Willy.glb", "Woman-NLA.glb", "ant.glb", "Branch.glb", "crackman.glb", "Diver.glb", "fossegrim.glb", "leafy.glb", "Oak.glb", "raccoon.glb", "wireman.glb"];
var randomAnimation = function(zone) {
  return function(increment) {
    return function(nAnims) {
      return bind4(ask3)(function(env) {
        var nModels = length(models);
        var secs = unwrap3(unInstant(fromDateTime(env.tempo.time))) / (1e3 * toNumber(nModels));
        var nBase = round2((secs - floor(secs)) * toNumber(nAnims));
        var n = mod4((nBase + zone | 0) + increment | 0)(nAnims);
        return pure1(new ValueInt(n));
      });
    };
  };
};
var randomModel = function(zone) {
  return function(increment) {
    return bind4(ask3)(function(env) {
      var secs = unwrap3(unInstant(fromDateTime(env.tempo.time))) / 1e3;
      var nModels = length(models);
      var nBase = round2((secs - floor(secs)) * toNumber(nModels));
      var n = mod4((nBase + zone | 0) + increment | 0)(nModels);
      return pure1(fromMaybe("raccoon.glb")(index(models)(n)));
    });
  };
};
var logAnimation = function(i) {
  return function(x) {
    return log2(" " + (show4(i) + (": " + show1(x.name))));
  };
};
var loadModel = function(url) {
  return function(s) {
    return bind4(ask3)(function(env) {
      return discard22(liftEffect3(write(url)(s.url)))(function() {
        var url$prime = resolveURL(url);
        return bind4(liftEffect3(loadGLTF_DRACO("https://dktr0.github.io/LocoMotion/threejs/")(url$prime)(function(gltf) {
          return function __do2() {
            log2("model " + (url$prime + (" loaded with " + (show4(length(gltf.animations)) + " animations"))))();
            traverseWithIndex_2(logAnimation)(gltf.animations)();
            addAnything(env.scene)(gltf.scene)();
            var m = gltfToModel(gltf)();
            return write(new Just(m))(s.model)();
          };
        })))(function() {
          return pure1(unit);
        });
      });
    });
  };
};
var calculateModelURL = function(zone) {
  return function(vm) {
    var v = lookupString("")("url")(vm);
    if (v === "") {
      return randomModel(zone)(0);
    }
    ;
    return pure1(v);
  };
};
var updateModel = function(zone) {
  return function(vm) {
    return function(d) {
      return bind4(calculateModelURL(zone)(vm))(function(urlProg) {
        return bind4(liftEffect3(read(d.url)))(function(urlState) {
          return discard22(when2(urlProg !== urlState)(discard22(removeDancer(d))(function() {
            return loadModel(urlProg)(d);
          })))(function() {
            return pure1(d);
          });
        });
      });
    };
  };
};
var calculateAnimation = function(zone) {
  return function(nAnims) {
    return function(vm) {
      var v = lookup4("animation")(vm);
      if (v instanceof Nothing) {
        return randomAnimation(zone)(0)(nAnims);
      }
      ;
      if (v instanceof Just) {
        return pure1(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Dancer (line 106, column 3 - line 108, column 21): " + [v.constructor.name]);
    };
  };
};
var updateAnimation = function(zone) {
  return function(vm) {
    return function(s) {
      return whenMaybeRef1(s.model)(function(m) {
        return bind4(ask3)(function(env) {
          return bind4(liftEffect3(read(m.mixerState)))(function(prevMixerState) {
            var nAnims = length(m.actions);
            return bind4(calculateAnimation(zone)(nAnims)(vm))(function(animationValue) {
              var newMixerState = valueToMixerState(m)(animationValue);
              return bind4(liftEffect3(read(m.durState)))(function(prevDurState) {
                var dur = lookupNumber(1)("dur")(vm) * env.cycleDur;
                return discard22(when2(notEq1(prevMixerState)(newMixerState) || prevDurState !== dur)(liftEffect3(function __do2() {
                  traverseWithIndex_2(updateAnimationAction(m)(dur))(newMixerState)();
                  write(newMixerState)(m.mixerState)();
                  return write(dur)(m.durState)();
                })))(function() {
                  return liftEffect3(updateAnimationMixer(m.mixer)(env.delta));
                });
              });
            });
          });
        });
      });
    };
  };
};
var updateDancer = function(zone) {
  return function(vm) {
    return function(x) {
      return bind4(updateModel(zone)(vm)(x))(function(y) {
        return discard22(whenMaybeRef1(y.model)(function(m) {
          return discard22(updatePosition2(vm)(m.scene))(function() {
            return discard22(updateScale2(vm)(m.scene))(function() {
              return discard22(updateRotation2(vm)(m.scene))(function() {
                return updateAnimation(zone)(vm)(y);
              });
            });
          });
        }))(function() {
          return pure1(x);
        });
      });
    };
  };
};

// output/Lights/index.js
var monadReaderT4 = /* @__PURE__ */ monadReaderT(monadEffect);
var bindStateT4 = /* @__PURE__ */ bindStateT(monadReaderT4);
var bind5 = /* @__PURE__ */ bind(bindStateT4);
var liftEffect4 = /* @__PURE__ */ liftEffect(/* @__PURE__ */ monadEffectState(/* @__PURE__ */ monadEffectReader(monadEffectEffect)));
var setPosition4 = /* @__PURE__ */ setPosition2();
var discard4 = /* @__PURE__ */ discard(discardUnit);
var discard1 = /* @__PURE__ */ discard4(bindStateT4);
var setLightIntensity3 = /* @__PURE__ */ setLightIntensity2();
var updatePosition3 = /* @__PURE__ */ updatePosition();
var pure4 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadReaderT4));
var setDistance3 = /* @__PURE__ */ setDistance2();
var setDecay3 = /* @__PURE__ */ setDecay2();
var updateScale3 = /* @__PURE__ */ updateScale();
var updateRotation3 = /* @__PURE__ */ updateRotation();
var ask4 = /* @__PURE__ */ ask(/* @__PURE__ */ monadAskStateT(/* @__PURE__ */ monadAskReaderT(monadEffect)));
var setTarget3 = /* @__PURE__ */ setTarget2()();
var updateVirtualTarget = function() {
  return function(vm) {
    return function(t) {
      return bind5(realizeNumber("lx")(0)(vm))(function(lx) {
        return bind5(realizeNumber("ly")(0)(vm))(function(ly) {
          return bind5(realizeNumber("lz")(0)(vm))(function(lz) {
            return liftEffect4(setPosition4(t)(lx)(ly)(lz));
          });
        });
      });
    };
  };
};
var updateVirtualTarget1 = /* @__PURE__ */ updateVirtualTarget();
var updateColourAndIntensity = function() {
  return function(vm) {
    return function(a) {
      var colour = lookupInt(16777215)("colour")(vm);
      return bind5(realizeNumber("intensity")(1)(vm))(function(intensity) {
        return discard1(liftEffect4(setColorInt(a)(colour)))(function() {
          return liftEffect4(setLightIntensity3(a)(intensity));
        });
      });
    };
  };
};
var updateColourAndIntensity1 = /* @__PURE__ */ updateColourAndIntensity();
var updateDirectional = function(vm) {
  return function(x) {
    return discard1(updateColourAndIntensity1(vm)(x.directionalLight))(function() {
      return discard1(updatePosition3(vm)(x.directionalLight))(function() {
        return discard1(updateVirtualTarget1(vm)(x.virtualTarget))(function() {
          return pure4(x);
        });
      });
    });
  };
};
var updateHemisphere = function(vm) {
  return function(x) {
    var groundColour = lookupInt(16777215)("ground")(vm);
    return discard1(liftEffect4(setGroundColor(x.hemisphereLight)(groundColour)))(function() {
      return discard1(updateColourAndIntensity1(vm)(x.hemisphereLight))(function() {
        return discard1(updatePosition3(vm)(x.hemisphereLight))(function() {
          return pure4(x);
        });
      });
    });
  };
};
var updatePoint = function(vm) {
  return function(x) {
    return bind5(realizeNumber("distance")(0)(vm))(function(distance) {
      return bind5(realizeNumber("decay")(2)(vm))(function(decay) {
        return discard1(liftEffect4(function __do2() {
          setDistance3(x.pointLight)(distance)();
          return setDecay3(x.pointLight)(decay)();
        }))(function() {
          return discard1(updateColourAndIntensity1(vm)(x.pointLight))(function() {
            return discard1(updatePosition3(vm)(x.pointLight))(function() {
              return pure4(x);
            });
          });
        });
      });
    });
  };
};
var updateRectArea = function(vm) {
  return function(x) {
    return bind5(realizeNumber("width")(10)(vm))(function(width) {
      return bind5(realizeNumber("height")(10)(vm))(function(height) {
        return discard1(liftEffect4(function __do2() {
          setWidth(x.rectAreaLight)(width)();
          return setHeight(x.rectAreaLight)(height)();
        }))(function() {
          return discard1(updateColourAndIntensity1(vm)(x.rectAreaLight))(function() {
            return discard1(updatePosition3(vm)(x.rectAreaLight))(function() {
              return discard1(updateScale3(vm)(x.rectAreaLight))(function() {
                return discard1(updateRotation3(vm)(x.rectAreaLight))(function() {
                  return pure4(x);
                });
              });
            });
          });
        });
      });
    });
  };
};
var updateSpot = function(vm) {
  return function(x) {
    return bind5(realizeNumber("distance")(0)(vm))(function(distance) {
      return bind5(realizeNumber("angle")(90)(vm))(function(angle) {
        return bind5(realizeNumber("penumbra")(0)(vm))(function(penumbra) {
          return bind5(realizeNumber("decay")(0)(vm))(function(decay) {
            return discard1(liftEffect4(function __do2() {
              setDistance3(x.spotLight)(distance)();
              setAngle(x.spotLight)(angle * pi / 180)();
              setPenumbra(x.spotLight)(penumbra)();
              return setDecay3(x.spotLight)(decay)();
            }))(function() {
              return discard1(updateColourAndIntensity1(vm)(x.spotLight))(function() {
                return discard1(updatePosition3(vm)(x.spotLight))(function() {
                  return discard1(updateVirtualTarget1(vm)(x.virtualTarget))(function() {
                    return pure4(x);
                  });
                });
              });
            });
          });
        });
      });
    });
  };
};
var updateAmbient = function(vm) {
  return function(x) {
    return discard1(updateColourAndIntensity1(vm)(x.ambientLight))(function() {
      return pure4(x);
    });
  };
};
var removeSpot = function(x) {
  return liftEffect4(function __do2() {
    removeFromParent(x.spotLight)();
    return removeFromParent(x.virtualTarget)();
  });
};
var removeRectArea = function(x) {
  return liftEffect4(removeFromParent(x.rectAreaLight));
};
var removePoint = function(x) {
  return liftEffect4(removeFromParent(x.pointLight));
};
var removeHemisphere = function(x) {
  return liftEffect4(removeFromParent(x.hemisphereLight));
};
var removeDirectional = function(x) {
  return liftEffect4(function __do2() {
    removeFromParent(x.directionalLight)();
    return removeFromParent(x.virtualTarget)();
  });
};
var removeAmbient = function(x) {
  return liftEffect4(removeFromParent(x.ambientLight));
};
var newSpot = /* @__PURE__ */ bind5(ask4)(function(env) {
  return liftEffect4(function __do2() {
    var spotLight = newSpotLight(16777215)(0)(0)(pi / 2)(0)(0)();
    addAnything(env.scene)(spotLight)();
    var virtualTarget = newObject3D();
    addAnything(env.scene)(virtualTarget)();
    setTarget3(spotLight)(virtualTarget)();
    return {
      spotLight,
      virtualTarget
    };
  });
});
var newRectArea = /* @__PURE__ */ bind5(ask4)(function(env) {
  return liftEffect4(function __do2() {
    var rectAreaLight = newRectAreaLight(0)(0)(10)(10)();
    addAnything(env.scene)(rectAreaLight)();
    return {
      rectAreaLight
    };
  });
});
var newPoint = /* @__PURE__ */ bind5(ask4)(function(env) {
  return liftEffect4(function __do2() {
    var pointLight = newPointLight(0)(0)(0)(2)();
    addAnything(env.scene)(pointLight)();
    return {
      pointLight
    };
  });
});
var newHemisphere = /* @__PURE__ */ bind5(ask4)(function(env) {
  return liftEffect4(function __do2() {
    var hemisphereLight = newHemisphereLight(0)(16777215)(0)();
    addAnything(env.scene)(hemisphereLight)();
    return {
      hemisphereLight
    };
  });
});
var newDirectional = /* @__PURE__ */ bind5(ask4)(function(env) {
  return liftEffect4(function __do2() {
    var directionalLight = newDirectionalLight(0)(0)();
    addAnything(env.scene)(directionalLight)();
    var virtualTarget = newObject3D();
    addAnything(env.scene)(virtualTarget)();
    setTarget3(directionalLight)(virtualTarget)();
    return {
      directionalLight,
      virtualTarget
    };
  });
});
var newAmbient = /* @__PURE__ */ bind5(ask4)(function(env) {
  return liftEffect4(function __do2() {
    var ambientLight = newAmbientLight(0)(0)();
    addAnything(env.scene)(ambientLight)();
    return {
      ambientLight
    };
  });
});

// output/Data.Lazy/foreign.js
var defer2 = function(thunk) {
  var v = null;
  return function() {
    if (thunk === void 0)
      return v;
    v = thunk();
    thunk = void 0;
    return v;
  };
};
var force = function(l) {
  return l();
};

// output/Parsing/index.js
var $runtime_lazy7 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var unwrap4 = /* @__PURE__ */ unwrap();
var ParseState = /* @__PURE__ */ function() {
  function ParseState2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  ParseState2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new ParseState2(value0, value1, value2);
      };
    };
  };
  return ParseState2;
}();
var ParseError = /* @__PURE__ */ function() {
  function ParseError2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ParseError2.create = function(value0) {
    return function(value1) {
      return new ParseError2(value0, value1);
    };
  };
  return ParseError2;
}();
var More = /* @__PURE__ */ function() {
  function More2(value0) {
    this.value0 = value0;
  }
  ;
  More2.create = function(value0) {
    return new More2(value0);
  };
  return More2;
}();
var Lift = /* @__PURE__ */ function() {
  function Lift2(value0) {
    this.value0 = value0;
  }
  ;
  Lift2.create = function(value0) {
    return new Lift2(value0);
  };
  return Lift2;
}();
var Stop = /* @__PURE__ */ function() {
  function Stop2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Stop2.create = function(value0) {
    return function(value1) {
      return new Stop2(value0, value1);
    };
  };
  return Stop2;
}();
var lazyParserT = {
  defer: function(f) {
    var m = defer2(f);
    return function(state1, more, lift1, $$throw, done) {
      var v = force(m);
      return v(state1, more, lift1, $$throw, done);
    };
  }
};
var functorParserT = {
  map: function(f) {
    return function(v) {
      return function(state1, more, lift1, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift1, $$throw, function(state2, a) {
            return more(function(v2) {
              return done(state2, f(a));
            });
          });
        });
      };
    };
  }
};
var applyParserT = {
  apply: function(v) {
    return function(v1) {
      return function(state1, more, lift1, $$throw, done) {
        return more(function(v2) {
          return v(state1, more, lift1, $$throw, function(state2, f) {
            return more(function(v3) {
              return v1(state2, more, lift1, $$throw, function(state3, a) {
                return more(function(v4) {
                  return done(state3, f(a));
                });
              });
            });
          });
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var bindParserT = {
  bind: function(v) {
    return function(next) {
      return function(state1, more, lift1, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift1, $$throw, function(state2, a) {
            return more(function(v2) {
              var v3 = next(a);
              return v3(state2, more, lift1, $$throw, done);
            });
          });
        });
      };
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindParserT);
var applicativeParserT = {
  pure: function(a) {
    return function(state1, v, v1, v2, done) {
      return done(state1, a);
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var monadParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Bind1: function() {
    return bindParserT;
  }
};
var monadRecParserT = {
  tailRecM: function(next) {
    return function(initArg) {
      return function(state1, more, lift1, $$throw, done) {
        var $lazy_loop = $runtime_lazy7("loop", "Parsing", function() {
          return function(state2, arg, gas) {
            var v = next(arg);
            return v(state2, more, lift1, $$throw, function(state3, step2) {
              if (step2 instanceof Loop) {
                var $206 = gas === 0;
                if ($206) {
                  return more(function(v1) {
                    return $lazy_loop(277)(state3, step2.value0, 30);
                  });
                }
                ;
                return $lazy_loop(279)(state3, step2.value0, gas - 1 | 0);
              }
              ;
              if (step2 instanceof Done) {
                return done(state3, step2.value0);
              }
              ;
              throw new Error("Failed pattern match at Parsing (line 273, column 39 - line 281, column 43): " + [step2.constructor.name]);
            });
          };
        });
        var loop = $lazy_loop(270);
        return loop(state1, initArg, 30);
      };
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var monadThrowParseErrorParse = {
  throwError: function(err) {
    return function(state1, v, v1, $$throw, v2) {
      return $$throw(state1, err);
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var throwError2 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
var altParserT = {
  alt: function(v) {
    return function(v1) {
      return function(v2, more, lift1, $$throw, done) {
        return more(function(v3) {
          return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
            return more(function(v5) {
              if (v4.value2) {
                return $$throw(v4, err);
              }
              ;
              return v1(v2, more, lift1, $$throw, done);
            });
          }, done);
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var stateParserT = function(k) {
  return function(state1, v, v1, v2, done) {
    var v3 = k(state1);
    return done(v3.value1, v3.value0);
  };
};
var runParserT$prime = function(dictMonadRec) {
  var Monad0 = dictMonadRec.Monad0();
  var map15 = map(Monad0.Bind1().Apply0().Functor0());
  var pure15 = pure(Monad0.Applicative0());
  var tailRecM4 = tailRecM(dictMonadRec);
  return function(state1) {
    return function(v) {
      var go = function($copy_step) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(step2) {
          var v1 = step2(unit);
          if (v1 instanceof More) {
            $copy_step = v1.value0;
            return;
          }
          ;
          if (v1 instanceof Lift) {
            $tco_done = true;
            return map15(Loop.create)(v1.value0);
          }
          ;
          if (v1 instanceof Stop) {
            $tco_done = true;
            return pure15(new Done(new Tuple(v1.value1, v1.value0)));
          }
          ;
          throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_step);
        }
        ;
        return $tco_result;
      };
      return tailRecM4(go)(function(v1) {
        return v(state1, More.create, Lift.create, function(state2, err) {
          return new Stop(state2, new Left(err));
        }, function(state2, res) {
          return new Stop(state2, new Right(res));
        });
      });
    };
  };
};
var position = /* @__PURE__ */ stateParserT(function(v) {
  return new Tuple(v.value1, v);
});
var initialPos = {
  index: 0,
  line: 1,
  column: 1
};
var runParserT = function(dictMonadRec) {
  var map15 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
  var runParserT$prime1 = runParserT$prime(dictMonadRec);
  return function(s) {
    return function(p) {
      var initialState = new ParseState(s, initialPos, false);
      return map15(fst)(runParserT$prime1(initialState)(p));
    };
  };
};
var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
var runParser = function(s) {
  var $281 = runParserT1(s);
  return function($282) {
    return unwrap4($281($282));
  };
};
var failWithPosition = function(message2) {
  return function(pos) {
    return throwError2(new ParseError(message2, pos));
  };
};
var fail = function(message2) {
  return bindFlipped2(failWithPosition(message2))(position);
};
var plusParserT = {
  empty: /* @__PURE__ */ fail("No alternative"),
  Alt0: function() {
    return altParserT;
  }
};
var alternativeParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Plus1: function() {
    return plusParserT;
  }
};

// output/Data.List/index.js
var map7 = /* @__PURE__ */ map(functorMaybe);
var bimap2 = /* @__PURE__ */ bimap(bifunctorStep);
var uncons4 = function(v) {
  if (v instanceof Nil) {
    return Nothing.value;
  }
  ;
  if (v instanceof Cons) {
    return new Just({
      head: v.value0,
      tail: v.value1
    });
  }
  ;
  throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
};
var toUnfoldable = function(dictUnfoldable) {
  return unfoldr(dictUnfoldable)(function(xs) {
    return map7(function(rec) {
      return new Tuple(rec.head, rec.tail);
    })(uncons4(xs));
  });
};
var reverse2 = /* @__PURE__ */ function() {
  var go = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Nil) {
          $tco_done = true;
          return v;
        }
        ;
        if (v1 instanceof Cons) {
          $tco_var_v = new Cons(v1.value0, v);
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return go(Nil.value);
}();
var manyRec = function(dictMonadRec) {
  var bind15 = bind(dictMonadRec.Monad0().Bind1());
  var tailRecM4 = tailRecM(dictMonadRec);
  return function(dictAlternative) {
    var Alt0 = dictAlternative.Plus1().Alt0();
    var alt6 = alt(Alt0);
    var map15 = map(Alt0.Functor0());
    var pure15 = pure(dictAlternative.Applicative0());
    return function(p) {
      var go = function(acc) {
        return bind15(alt6(map15(Loop.create)(p))(pure15(new Done(unit))))(function(aa) {
          return pure15(bimap2(function(v) {
            return new Cons(v, acc);
          })(function(v) {
            return reverse2(acc);
          })(aa));
        });
      };
      return tailRecM4(go)(Nil.value);
    };
  };
};
var some2 = function(dictAlternative) {
  var apply2 = apply(dictAlternative.Applicative0().Apply0());
  var map15 = map(dictAlternative.Plus1().Alt0().Functor0());
  return function(dictLazy) {
    var defer4 = defer(dictLazy);
    return function(v) {
      return apply2(map15(Cons.create)(v))(defer4(function(v1) {
        return many2(dictAlternative)(dictLazy)(v);
      }));
    };
  };
};
var many2 = function(dictAlternative) {
  var alt6 = alt(dictAlternative.Plus1().Alt0());
  var pure15 = pure(dictAlternative.Applicative0());
  return function(dictLazy) {
    return function(v) {
      return alt6(some2(dictAlternative)(dictLazy)(v))(pure15(Nil.value));
    };
  };
};

// output/Data.List.NonEmpty/index.js
var toList = function(v) {
  return new Cons(v.value0, v.value1);
};
var cons$prime = function(x) {
  return function(xs) {
    return new NonEmpty(x, xs);
  };
};

// output/Parsing.Combinators/index.js
var alt2 = /* @__PURE__ */ alt(altParserT);
var defer3 = /* @__PURE__ */ defer(lazyParserT);
var voidLeft2 = /* @__PURE__ */ voidLeft(functorParserT);
var pure5 = /* @__PURE__ */ pure(applicativeParserT);
var applySecond2 = /* @__PURE__ */ applySecond(applyParserT);
var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecParserT);
var bind6 = /* @__PURE__ */ bind(bindParserT);
var map8 = /* @__PURE__ */ map(functorParserT);
var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
var applyFirst2 = /* @__PURE__ */ applyFirst(applyParserT);
var empty3 = /* @__PURE__ */ empty(plusParserT);
var withLazyErrorMessage = function(p) {
  return function(msg) {
    return alt2(p)(defer3(function(v) {
      return fail("Expected " + msg(unit));
    }));
  };
};
var withErrorMessage = function(p) {
  return function(msg) {
    return alt2(p)(fail("Expected " + msg));
  };
};
var $$try = function(v) {
  return function(v1, more, lift5, $$throw, done) {
    return v(v1, more, lift5, function(v2, err) {
      return $$throw(new ParseState(v2.value0, v2.value1, v1.value2), err);
    }, done);
  };
};
var skipMany1 = function(p) {
  var go = function(v) {
    return alt2(voidLeft2(p)(new Loop(unit)))(pure5(new Done(unit)));
  };
  return applySecond2(p)(tailRecM3(go)(unit));
};
var skipMany = function(p) {
  return alt2(skipMany1(p))(pure5(unit));
};
var sepBy1 = function(p) {
  return function(sep) {
    return bind6(p)(function(a) {
      return bind6(manyRec2(applySecond2(sep)(p)))(function(as) {
        return pure5(cons$prime(a)(as));
      });
    });
  };
};
var sepBy = function(p) {
  return function(sep) {
    return alt2(map8(toList)(sepBy1(p)(sep)))(pure5(Nil.value));
  };
};
var option = function(a) {
  return function(p) {
    return alt2(p)(pure5(a));
  };
};
var notFollowedBy = function(p) {
  return $$try(alt2(applySecond2($$try(p))(fail("Negated parser succeeded")))(pure5(unit)));
};
var many3 = manyRec2;
var lookAhead = function(v) {
  return function(state1, more, lift5, $$throw, done) {
    return v(state1, more, lift5, function(v1, err) {
      return $$throw(state1, err);
    }, function(v1, res) {
      return done(state1, res);
    });
  };
};
var choice = function(dictFoldable) {
  var go = function(p1) {
    return function(v) {
      if (v instanceof Nothing) {
        return new Just(p1);
      }
      ;
      if (v instanceof Just) {
        return new Just(alt2(p1)(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Parsing.Combinators (line 358, column 11 - line 360, column 32): " + [v.constructor.name]);
    };
  };
  var $95 = fromMaybe(empty3);
  var $96 = foldr(dictFoldable)(go)(Nothing.value);
  return function($97) {
    return $95($96($97));
  };
};
var chainl1 = function(p) {
  return function(f) {
    var go = function(a) {
      return alt2(bind6(f)(function(op) {
        return bind6(p)(function(a$prime) {
          return pure5(new Loop(op(a)(a$prime)));
        });
      }))(pure5(new Done(a)));
    };
    return bind6(p)(function(a) {
      return tailRecM3(go)(a);
    });
  };
};
var between = function(open) {
  return function(close) {
    return function(p) {
      return applyFirst2(applySecond2(open)(p))(close);
    };
  };
};
var asErrorMessage = /* @__PURE__ */ flip(withErrorMessage);

// output/Data.Array.NonEmpty.Internal/foreign.js
var traverse1Impl = function() {
  function Cont(fn) {
    this.fn = fn;
  }
  var emptyList = {};
  var ConsCell = function(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  };
  function finalCell(head4) {
    return new ConsCell(head4, emptyList);
  }
  function consList(x) {
    return function(xs) {
      return new ConsCell(x, xs);
    };
  }
  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }
  return function(apply2, map15, f) {
    var buildFrom = function(x, ys) {
      return apply2(map15(consList)(f(x)))(ys);
    };
    var go = function(acc, currentLen, xs) {
      if (currentLen === 0) {
        return acc;
      } else {
        var last3 = xs[currentLen - 1];
        return new Cont(function() {
          var built = go(buildFrom(last3, acc), currentLen - 1, xs);
          return built;
        });
      }
    };
    return function(array) {
      var acc = map15(finalCell)(f(array[array.length - 1]));
      var result = go(acc, array.length - 1, array);
      while (result instanceof Cont) {
        result = result.fn();
      }
      return map15(listToArray)(result);
    };
  };
}();

// output/Parsing.String/index.js
var fromEnum9 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
var mod5 = /* @__PURE__ */ mod(euclideanRingInt);
var fromJust7 = /* @__PURE__ */ fromJust();
var toEnum4 = /* @__PURE__ */ toEnum(boundedEnumChar);
var show12 = /* @__PURE__ */ show(showString);
var show23 = /* @__PURE__ */ show(showChar);
var updatePosSingle = function(v) {
  return function(cp) {
    return function(after) {
      var v1 = fromEnum9(cp);
      if (v1 === 10) {
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 13) {
        var v2 = codePointAt(0)(after);
        if (v2 instanceof Just && fromEnum9(v2.value0) === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: v.column
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 9) {
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: (v.column + 8 | 0) - mod5(v.column - 1 | 0)(8) | 0
        };
      }
      ;
      return {
        index: v.index + 1 | 0,
        line: v.line,
        column: v.column + 1 | 0
      };
    };
  };
};
var updatePosString = function($copy_pos) {
  return function($copy_before) {
    return function($copy_after) {
      var $tco_var_pos = $copy_pos;
      var $tco_var_before = $copy_before;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(pos, before, after) {
        var v = uncons3(before);
        if (v instanceof Nothing) {
          $tco_done = true;
          return pos;
        }
        ;
        if (v instanceof Just) {
          var newPos = function() {
            if ($$null(v.value0.tail)) {
              return updatePosSingle(pos)(v.value0.head)(after);
            }
            ;
            if (otherwise) {
              return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 165, column 7 - line 167, column 52): " + []);
          }();
          $tco_var_pos = newPos;
          $tco_var_before = v.value0.tail;
          $copy_after = after;
          return;
        }
        ;
        throw new Error("Failed pattern match at Parsing.String (line 161, column 36 - line 168, column 38): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
      }
      ;
      return $tco_result;
    };
  };
};
var satisfyCodePoint = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = uncons3(v.value0);
            if (v3 instanceof Nothing) {
              return $$throw(v, new ParseError("Unexpected EOF", v.value1));
            }
            ;
            if (v3 instanceof Just) {
              var $76 = f(v3.value0.head);
              if ($76) {
                return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
              }
              ;
              return $$throw(v, new ParseError("Predicate unsatisfied", v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 136, column 7 - line 143, column 73): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var satisfy = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = uncons3(v.value0);
            if (v3 instanceof Nothing) {
              return $$throw(v, new ParseError("Unexpected EOF", v.value1));
            }
            ;
            if (v3 instanceof Just) {
              var cp = fromEnum9(v3.value0.head);
              var $85 = cp < 0 || cp > 65535;
              if ($85) {
                return $$throw(v, new ParseError("Expected Char", v.value1));
              }
              ;
              var ch = fromJust7(toEnum4(cp));
              var $86 = f(ch);
              if ($86) {
                return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
              }
              ;
              return $$throw(v, new ParseError("Predicate unsatisfied", v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var eof = /* @__PURE__ */ mkFn5(function(v) {
  return function(v1) {
    return function(v2) {
      return function($$throw) {
        return function(done) {
          var $133 = $$null(v.value0);
          if ($133) {
            return done(new ParseState(v.value0, v.value1, true), unit);
          }
          ;
          return $$throw(v, new ParseError("Expected EOF", v.value1));
        };
      };
    };
  };
});
var consumeWith = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = f(v.value0);
            if (v3 instanceof Left) {
              return $$throw(v, new ParseError(v3.value0, v.value1));
            }
            ;
            if (v3 instanceof Right) {
              return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null(v3.value0.consumed)), v3.value0.value);
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 286, column 7 - line 290, column 121): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var string = function(str) {
  return consumeWith(function(input) {
    var v = stripPrefix(str)(input);
    if (v instanceof Just) {
      return new Right({
        value: str,
        consumed: str,
        remainder: v.value0
      });
    }
    ;
    return new Left("Expected " + show12(str));
  });
};
var $$char = function(c) {
  return withErrorMessage(satisfy(function(v) {
    return v === c;
  }))(show23(c));
};

// output/Data.Char/index.js
var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
var fromCharCode3 = /* @__PURE__ */ toEnum(boundedEnumChar);

// output/Data.CodePoint.Unicode.Internal/index.js
var unsafeIndex2 = /* @__PURE__ */ unsafeIndex();
var elemIndex3 = /* @__PURE__ */ elemIndex(eqInt);
var NUMCAT_LU = /* @__PURE__ */ function() {
  function NUMCAT_LU2() {
  }
  ;
  NUMCAT_LU2.value = new NUMCAT_LU2();
  return NUMCAT_LU2;
}();
var NUMCAT_LL = /* @__PURE__ */ function() {
  function NUMCAT_LL2() {
  }
  ;
  NUMCAT_LL2.value = new NUMCAT_LL2();
  return NUMCAT_LL2;
}();
var NUMCAT_LT = /* @__PURE__ */ function() {
  function NUMCAT_LT2() {
  }
  ;
  NUMCAT_LT2.value = new NUMCAT_LT2();
  return NUMCAT_LT2;
}();
var NUMCAT_LM = /* @__PURE__ */ function() {
  function NUMCAT_LM2() {
  }
  ;
  NUMCAT_LM2.value = new NUMCAT_LM2();
  return NUMCAT_LM2;
}();
var NUMCAT_LO = /* @__PURE__ */ function() {
  function NUMCAT_LO2() {
  }
  ;
  NUMCAT_LO2.value = new NUMCAT_LO2();
  return NUMCAT_LO2;
}();
var NUMCAT_MN = /* @__PURE__ */ function() {
  function NUMCAT_MN2() {
  }
  ;
  NUMCAT_MN2.value = new NUMCAT_MN2();
  return NUMCAT_MN2;
}();
var NUMCAT_MC = /* @__PURE__ */ function() {
  function NUMCAT_MC2() {
  }
  ;
  NUMCAT_MC2.value = new NUMCAT_MC2();
  return NUMCAT_MC2;
}();
var NUMCAT_ME = /* @__PURE__ */ function() {
  function NUMCAT_ME2() {
  }
  ;
  NUMCAT_ME2.value = new NUMCAT_ME2();
  return NUMCAT_ME2;
}();
var NUMCAT_ND = /* @__PURE__ */ function() {
  function NUMCAT_ND2() {
  }
  ;
  NUMCAT_ND2.value = new NUMCAT_ND2();
  return NUMCAT_ND2;
}();
var NUMCAT_NL = /* @__PURE__ */ function() {
  function NUMCAT_NL2() {
  }
  ;
  NUMCAT_NL2.value = new NUMCAT_NL2();
  return NUMCAT_NL2;
}();
var NUMCAT_NO = /* @__PURE__ */ function() {
  function NUMCAT_NO2() {
  }
  ;
  NUMCAT_NO2.value = new NUMCAT_NO2();
  return NUMCAT_NO2;
}();
var NUMCAT_PC = /* @__PURE__ */ function() {
  function NUMCAT_PC2() {
  }
  ;
  NUMCAT_PC2.value = new NUMCAT_PC2();
  return NUMCAT_PC2;
}();
var NUMCAT_PD = /* @__PURE__ */ function() {
  function NUMCAT_PD2() {
  }
  ;
  NUMCAT_PD2.value = new NUMCAT_PD2();
  return NUMCAT_PD2;
}();
var NUMCAT_PS = /* @__PURE__ */ function() {
  function NUMCAT_PS2() {
  }
  ;
  NUMCAT_PS2.value = new NUMCAT_PS2();
  return NUMCAT_PS2;
}();
var NUMCAT_PE = /* @__PURE__ */ function() {
  function NUMCAT_PE2() {
  }
  ;
  NUMCAT_PE2.value = new NUMCAT_PE2();
  return NUMCAT_PE2;
}();
var NUMCAT_PI = /* @__PURE__ */ function() {
  function NUMCAT_PI2() {
  }
  ;
  NUMCAT_PI2.value = new NUMCAT_PI2();
  return NUMCAT_PI2;
}();
var NUMCAT_PF = /* @__PURE__ */ function() {
  function NUMCAT_PF2() {
  }
  ;
  NUMCAT_PF2.value = new NUMCAT_PF2();
  return NUMCAT_PF2;
}();
var NUMCAT_PO = /* @__PURE__ */ function() {
  function NUMCAT_PO2() {
  }
  ;
  NUMCAT_PO2.value = new NUMCAT_PO2();
  return NUMCAT_PO2;
}();
var NUMCAT_SM = /* @__PURE__ */ function() {
  function NUMCAT_SM2() {
  }
  ;
  NUMCAT_SM2.value = new NUMCAT_SM2();
  return NUMCAT_SM2;
}();
var NUMCAT_SC = /* @__PURE__ */ function() {
  function NUMCAT_SC2() {
  }
  ;
  NUMCAT_SC2.value = new NUMCAT_SC2();
  return NUMCAT_SC2;
}();
var NUMCAT_SK = /* @__PURE__ */ function() {
  function NUMCAT_SK2() {
  }
  ;
  NUMCAT_SK2.value = new NUMCAT_SK2();
  return NUMCAT_SK2;
}();
var NUMCAT_SO = /* @__PURE__ */ function() {
  function NUMCAT_SO2() {
  }
  ;
  NUMCAT_SO2.value = new NUMCAT_SO2();
  return NUMCAT_SO2;
}();
var NUMCAT_ZS = /* @__PURE__ */ function() {
  function NUMCAT_ZS2() {
  }
  ;
  NUMCAT_ZS2.value = new NUMCAT_ZS2();
  return NUMCAT_ZS2;
}();
var NUMCAT_ZL = /* @__PURE__ */ function() {
  function NUMCAT_ZL2() {
  }
  ;
  NUMCAT_ZL2.value = new NUMCAT_ZL2();
  return NUMCAT_ZL2;
}();
var NUMCAT_ZP = /* @__PURE__ */ function() {
  function NUMCAT_ZP2() {
  }
  ;
  NUMCAT_ZP2.value = new NUMCAT_ZP2();
  return NUMCAT_ZP2;
}();
var NUMCAT_CC = /* @__PURE__ */ function() {
  function NUMCAT_CC2() {
  }
  ;
  NUMCAT_CC2.value = new NUMCAT_CC2();
  return NUMCAT_CC2;
}();
var NUMCAT_CF = /* @__PURE__ */ function() {
  function NUMCAT_CF2() {
  }
  ;
  NUMCAT_CF2.value = new NUMCAT_CF2();
  return NUMCAT_CF2;
}();
var NUMCAT_CS = /* @__PURE__ */ function() {
  function NUMCAT_CS2() {
  }
  ;
  NUMCAT_CS2.value = new NUMCAT_CS2();
  return NUMCAT_CS2;
}();
var NUMCAT_CO = /* @__PURE__ */ function() {
  function NUMCAT_CO2() {
  }
  ;
  NUMCAT_CO2.value = new NUMCAT_CO2();
  return NUMCAT_CO2;
}();
var NUMCAT_CN = /* @__PURE__ */ function() {
  function NUMCAT_CN2() {
  }
  ;
  NUMCAT_CN2.value = new NUMCAT_CN2();
  return NUMCAT_CN2;
}();
var numSpaceBlocks = 7;
var numLat1Blocks = 63;
var numConvBlocks = 1332;
var numBlocks = 3396;
var gencatZS = 2;
var rule1 = /* @__PURE__ */ function() {
  return {
    category: gencatZS,
    unicodeCat: NUMCAT_ZS.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var spacechars = [{
  start: 32,
  length: 1,
  convRule: rule1
}, {
  start: 160,
  length: 1,
  convRule: rule1
}, {
  start: 5760,
  length: 1,
  convRule: rule1
}, {
  start: 8192,
  length: 11,
  convRule: rule1
}, {
  start: 8239,
  length: 1,
  convRule: rule1
}, {
  start: 8287,
  length: 1,
  convRule: rule1
}, {
  start: 12288,
  length: 1,
  convRule: rule1
}];
var gencatZP = 67108864;
var rule162 = /* @__PURE__ */ function() {
  return {
    category: gencatZP,
    unicodeCat: NUMCAT_ZP.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatZL = 33554432;
var rule161 = /* @__PURE__ */ function() {
  return {
    category: gencatZL,
    unicodeCat: NUMCAT_ZL.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatSO = 8192;
var rule13 = /* @__PURE__ */ function() {
  return {
    category: gencatSO,
    unicodeCat: NUMCAT_SO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule170 = /* @__PURE__ */ function() {
  return {
    category: gencatSO,
    unicodeCat: NUMCAT_SO.value,
    possible: 1,
    updist: 0,
    lowdist: 26,
    titledist: 0
  };
}();
var rule171 = /* @__PURE__ */ function() {
  return {
    category: gencatSO,
    unicodeCat: NUMCAT_SO.value,
    possible: 1,
    updist: -26 | 0,
    lowdist: 0,
    titledist: -26 | 0
  };
}();
var gencatSM = 64;
var rule6 = /* @__PURE__ */ function() {
  return {
    category: gencatSM,
    unicodeCat: NUMCAT_SM.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatSK = 1024;
var rule10 = /* @__PURE__ */ function() {
  return {
    category: gencatSK,
    unicodeCat: NUMCAT_SK.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatSC = 8;
var rule3 = /* @__PURE__ */ function() {
  return {
    category: gencatSC,
    unicodeCat: NUMCAT_SC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPS = 16;
var rule4 = /* @__PURE__ */ function() {
  return {
    category: gencatPS,
    unicodeCat: NUMCAT_PS.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPO = 4;
var rule2 = /* @__PURE__ */ function() {
  return {
    category: gencatPO,
    unicodeCat: NUMCAT_PO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPI = 32768;
var rule15 = /* @__PURE__ */ function() {
  return {
    category: gencatPI,
    unicodeCat: NUMCAT_PI.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPF = 262144;
var rule19 = /* @__PURE__ */ function() {
  return {
    category: gencatPF,
    unicodeCat: NUMCAT_PF.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPE = 32;
var rule5 = /* @__PURE__ */ function() {
  return {
    category: gencatPE,
    unicodeCat: NUMCAT_PE.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPD = 128;
var rule7 = /* @__PURE__ */ function() {
  return {
    category: gencatPD,
    unicodeCat: NUMCAT_PD.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPC = 2048;
var rule11 = /* @__PURE__ */ function() {
  return {
    category: gencatPC,
    unicodeCat: NUMCAT_PC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatNO = 131072;
var rule17 = /* @__PURE__ */ function() {
  return {
    category: gencatNO,
    unicodeCat: NUMCAT_NO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatNL = 16777216;
var rule128 = /* @__PURE__ */ function() {
  return {
    category: gencatNL,
    unicodeCat: NUMCAT_NL.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule168 = /* @__PURE__ */ function() {
  return {
    category: gencatNL,
    unicodeCat: NUMCAT_NL.value,
    possible: 1,
    updist: 0,
    lowdist: 16,
    titledist: 0
  };
}();
var rule169 = /* @__PURE__ */ function() {
  return {
    category: gencatNL,
    unicodeCat: NUMCAT_NL.value,
    possible: 1,
    updist: -16 | 0,
    lowdist: 0,
    titledist: -16 | 0
  };
}();
var gencatND = 256;
var rule8 = /* @__PURE__ */ function() {
  return {
    category: gencatND,
    unicodeCat: NUMCAT_ND.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatMN = 2097152;
var rule92 = /* @__PURE__ */ function() {
  return {
    category: gencatMN,
    unicodeCat: NUMCAT_MN.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule93 = /* @__PURE__ */ function() {
  return {
    category: gencatMN,
    unicodeCat: NUMCAT_MN.value,
    possible: 1,
    updist: 84,
    lowdist: 0,
    titledist: 84
  };
}();
var gencatME = 4194304;
var rule119 = /* @__PURE__ */ function() {
  return {
    category: gencatME,
    unicodeCat: NUMCAT_ME.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatMC = 8388608;
var rule124 = /* @__PURE__ */ function() {
  return {
    category: gencatMC,
    unicodeCat: NUMCAT_MC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatLU = 512;
var nullrule = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_CN.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule104 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 8,
    titledist: 0
  };
}();
var rule107 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule115 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -60 | 0,
    titledist: 0
  };
}();
var rule117 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -7 | 0,
    titledist: 0
  };
}();
var rule118 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 80,
    titledist: 0
  };
}();
var rule120 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 15,
    titledist: 0
  };
}();
var rule122 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 48,
    titledist: 0
  };
}();
var rule125 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 7264,
    titledist: 0
  };
}();
var rule127 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 38864,
    titledist: 0
  };
}();
var rule137 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -3008 | 0,
    titledist: 0
  };
}();
var rule142 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -7615 | 0,
    titledist: 0
  };
}();
var rule144 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -8 | 0,
    titledist: 0
  };
}();
var rule153 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -74 | 0,
    titledist: 0
  };
}();
var rule156 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -86 | 0,
    titledist: 0
  };
}();
var rule157 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -100 | 0,
    titledist: 0
  };
}();
var rule158 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -112 | 0,
    titledist: 0
  };
}();
var rule159 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -128 | 0,
    titledist: 0
  };
}();
var rule160 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -126 | 0,
    titledist: 0
  };
}();
var rule163 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -7517 | 0,
    titledist: 0
  };
}();
var rule164 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -8383 | 0,
    titledist: 0
  };
}();
var rule165 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -8262 | 0,
    titledist: 0
  };
}();
var rule166 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 28,
    titledist: 0
  };
}();
var rule172 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10743 | 0,
    titledist: 0
  };
}();
var rule173 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -3814 | 0,
    titledist: 0
  };
}();
var rule174 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10727 | 0,
    titledist: 0
  };
}();
var rule177 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10780 | 0,
    titledist: 0
  };
}();
var rule178 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10749 | 0,
    titledist: 0
  };
}();
var rule179 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10783 | 0,
    titledist: 0
  };
}();
var rule180 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10782 | 0,
    titledist: 0
  };
}();
var rule181 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10815 | 0,
    titledist: 0
  };
}();
var rule183 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -35332 | 0,
    titledist: 0
  };
}();
var rule184 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42280 | 0,
    titledist: 0
  };
}();
var rule186 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42308 | 0,
    titledist: 0
  };
}();
var rule187 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42319 | 0,
    titledist: 0
  };
}();
var rule188 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42315 | 0,
    titledist: 0
  };
}();
var rule189 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42305 | 0,
    titledist: 0
  };
}();
var rule190 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42258 | 0,
    titledist: 0
  };
}();
var rule191 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42282 | 0,
    titledist: 0
  };
}();
var rule192 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42261 | 0,
    titledist: 0
  };
}();
var rule193 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 928,
    titledist: 0
  };
}();
var rule194 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -48 | 0,
    titledist: 0
  };
}();
var rule195 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42307 | 0,
    titledist: 0
  };
}();
var rule196 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -35384 | 0,
    titledist: 0
  };
}();
var rule201 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 40,
    titledist: 0
  };
}();
var rule203 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 34,
    titledist: 0
  };
}();
var rule22 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 1,
    titledist: 0
  };
}();
var rule24 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -199 | 0,
    titledist: 0
  };
}();
var rule26 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -121 | 0,
    titledist: 0
  };
}();
var rule29 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 210,
    titledist: 0
  };
}();
var rule30 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 206,
    titledist: 0
  };
}();
var rule31 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 205,
    titledist: 0
  };
}();
var rule32 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 79,
    titledist: 0
  };
}();
var rule33 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 202,
    titledist: 0
  };
}();
var rule34 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 203,
    titledist: 0
  };
}();
var rule35 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 207,
    titledist: 0
  };
}();
var rule37 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 211,
    titledist: 0
  };
}();
var rule38 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 209,
    titledist: 0
  };
}();
var rule40 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 213,
    titledist: 0
  };
}();
var rule42 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 214,
    titledist: 0
  };
}();
var rule43 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 218,
    titledist: 0
  };
}();
var rule44 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 217,
    titledist: 0
  };
}();
var rule45 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 219,
    titledist: 0
  };
}();
var rule47 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 2,
    titledist: 1
  };
}();
var rule51 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -97 | 0,
    titledist: 0
  };
}();
var rule52 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -56 | 0,
    titledist: 0
  };
}();
var rule53 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -130 | 0,
    titledist: 0
  };
}();
var rule54 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 10795,
    titledist: 0
  };
}();
var rule55 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -163 | 0,
    titledist: 0
  };
}();
var rule56 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 10792,
    titledist: 0
  };
}();
var rule58 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -195 | 0,
    titledist: 0
  };
}();
var rule59 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 69,
    titledist: 0
  };
}();
var rule60 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 71,
    titledist: 0
  };
}();
var rule9 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 32,
    titledist: 0
  };
}();
var rule94 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 116,
    titledist: 0
  };
}();
var rule95 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 38,
    titledist: 0
  };
}();
var rule96 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 37,
    titledist: 0
  };
}();
var rule97 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 64,
    titledist: 0
  };
}();
var rule98 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 63,
    titledist: 0
  };
}();
var gencatLT = 524288;
var rule151 = /* @__PURE__ */ function() {
  return {
    category: gencatLT,
    unicodeCat: NUMCAT_LT.value,
    possible: 1,
    updist: 0,
    lowdist: -8 | 0,
    titledist: 0
  };
}();
var rule154 = /* @__PURE__ */ function() {
  return {
    category: gencatLT,
    unicodeCat: NUMCAT_LT.value,
    possible: 1,
    updist: 0,
    lowdist: -9 | 0,
    titledist: 0
  };
}();
var rule48 = /* @__PURE__ */ function() {
  return {
    category: gencatLT,
    unicodeCat: NUMCAT_LT.value,
    possible: 1,
    updist: -1 | 0,
    lowdist: 1,
    titledist: 0
  };
}();
var gencatLO = 16384;
var rule14 = /* @__PURE__ */ function() {
  return {
    category: gencatLO,
    unicodeCat: NUMCAT_LO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatLM = 1048576;
var rule91 = /* @__PURE__ */ function() {
  return {
    category: gencatLM,
    unicodeCat: NUMCAT_LM.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatLL = 4096;
var rule100 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -37 | 0,
    lowdist: 0,
    titledist: -37 | 0
  };
}();
var rule101 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -31 | 0,
    lowdist: 0,
    titledist: -31 | 0
  };
}();
var rule102 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -64 | 0,
    lowdist: 0,
    titledist: -64 | 0
  };
}();
var rule103 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -63 | 0,
    lowdist: 0,
    titledist: -63 | 0
  };
}();
var rule105 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -62 | 0,
    lowdist: 0,
    titledist: -62 | 0
  };
}();
var rule106 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -57 | 0,
    lowdist: 0,
    titledist: -57 | 0
  };
}();
var rule108 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -47 | 0,
    lowdist: 0,
    titledist: -47 | 0
  };
}();
var rule109 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -54 | 0,
    lowdist: 0,
    titledist: -54 | 0
  };
}();
var rule110 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -8 | 0,
    lowdist: 0,
    titledist: -8 | 0
  };
}();
var rule111 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -86 | 0,
    lowdist: 0,
    titledist: -86 | 0
  };
}();
var rule112 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -80 | 0,
    lowdist: 0,
    titledist: -80 | 0
  };
}();
var rule113 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 7,
    lowdist: 0,
    titledist: 7
  };
}();
var rule114 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -116 | 0,
    lowdist: 0,
    titledist: -116 | 0
  };
}();
var rule116 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -96 | 0,
    lowdist: 0,
    titledist: -96 | 0
  };
}();
var rule12 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -32 | 0,
    lowdist: 0,
    titledist: -32 | 0
  };
}();
var rule121 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -15 | 0,
    lowdist: 0,
    titledist: -15 | 0
  };
}();
var rule123 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -48 | 0,
    lowdist: 0,
    titledist: -48 | 0
  };
}();
var rule126 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 3008,
    lowdist: 0,
    titledist: 0
  };
}();
var rule129 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6254 | 0,
    lowdist: 0,
    titledist: -6254 | 0
  };
}();
var rule130 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6253 | 0,
    lowdist: 0,
    titledist: -6253 | 0
  };
}();
var rule131 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6244 | 0,
    lowdist: 0,
    titledist: -6244 | 0
  };
}();
var rule132 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6242 | 0,
    lowdist: 0,
    titledist: -6242 | 0
  };
}();
var rule133 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6243 | 0,
    lowdist: 0,
    titledist: -6243 | 0
  };
}();
var rule134 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6236 | 0,
    lowdist: 0,
    titledist: -6236 | 0
  };
}();
var rule135 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6181 | 0,
    lowdist: 0,
    titledist: -6181 | 0
  };
}();
var rule136 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 35266,
    lowdist: 0,
    titledist: 35266
  };
}();
var rule138 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 35332,
    lowdist: 0,
    titledist: 35332
  };
}();
var rule139 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 3814,
    lowdist: 0,
    titledist: 3814
  };
}();
var rule140 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 35384,
    lowdist: 0,
    titledist: 35384
  };
}();
var rule141 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -59 | 0,
    lowdist: 0,
    titledist: -59 | 0
  };
}();
var rule143 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 8,
    lowdist: 0,
    titledist: 8
  };
}();
var rule145 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 74,
    lowdist: 0,
    titledist: 74
  };
}();
var rule146 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 86,
    lowdist: 0,
    titledist: 86
  };
}();
var rule147 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 100,
    lowdist: 0,
    titledist: 100
  };
}();
var rule148 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 128,
    lowdist: 0,
    titledist: 128
  };
}();
var rule149 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 112,
    lowdist: 0,
    titledist: 112
  };
}();
var rule150 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 126,
    lowdist: 0,
    titledist: 126
  };
}();
var rule152 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 9,
    lowdist: 0,
    titledist: 9
  };
}();
var rule155 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -7205 | 0,
    lowdist: 0,
    titledist: -7205 | 0
  };
}();
var rule167 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -28 | 0,
    lowdist: 0,
    titledist: -28 | 0
  };
}();
var rule175 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -10795 | 0,
    lowdist: 0,
    titledist: -10795 | 0
  };
}();
var rule176 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -10792 | 0,
    lowdist: 0,
    titledist: -10792 | 0
  };
}();
var rule18 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 743,
    lowdist: 0,
    titledist: 743
  };
}();
var rule182 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -7264 | 0,
    lowdist: 0,
    titledist: -7264 | 0
  };
}();
var rule185 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 48,
    lowdist: 0,
    titledist: 48
  };
}();
var rule197 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -928 | 0,
    lowdist: 0,
    titledist: -928 | 0
  };
}();
var rule198 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -38864 | 0,
    lowdist: 0,
    titledist: -38864 | 0
  };
}();
var rule20 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule202 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -40 | 0,
    lowdist: 0,
    titledist: -40 | 0
  };
}();
var rule204 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -34 | 0,
    lowdist: 0,
    titledist: -34 | 0
  };
}();
var rule21 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 121,
    lowdist: 0,
    titledist: 121
  };
}();
var rule23 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -1 | 0,
    lowdist: 0,
    titledist: -1 | 0
  };
}();
var rule25 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -232 | 0,
    lowdist: 0,
    titledist: -232 | 0
  };
}();
var rule27 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -300 | 0,
    lowdist: 0,
    titledist: -300 | 0
  };
}();
var rule28 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 195,
    lowdist: 0,
    titledist: 195
  };
}();
var rule36 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 97,
    lowdist: 0,
    titledist: 97
  };
}();
var rule39 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 163,
    lowdist: 0,
    titledist: 163
  };
}();
var rule41 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 130,
    lowdist: 0,
    titledist: 130
  };
}();
var rule46 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 56,
    lowdist: 0,
    titledist: 56
  };
}();
var rule49 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -2 | 0,
    lowdist: 0,
    titledist: -1 | 0
  };
}();
var rule50 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -79 | 0,
    lowdist: 0,
    titledist: -79 | 0
  };
}();
var rule57 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10815,
    lowdist: 0,
    titledist: 10815
  };
}();
var rule61 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10783,
    lowdist: 0,
    titledist: 10783
  };
}();
var rule62 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10780,
    lowdist: 0,
    titledist: 10780
  };
}();
var rule63 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10782,
    lowdist: 0,
    titledist: 10782
  };
}();
var rule64 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -210 | 0,
    lowdist: 0,
    titledist: -210 | 0
  };
}();
var rule65 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -206 | 0,
    lowdist: 0,
    titledist: -206 | 0
  };
}();
var rule66 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -205 | 0,
    lowdist: 0,
    titledist: -205 | 0
  };
}();
var rule67 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -202 | 0,
    lowdist: 0,
    titledist: -202 | 0
  };
}();
var rule68 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -203 | 0,
    lowdist: 0,
    titledist: -203 | 0
  };
}();
var rule69 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42319,
    lowdist: 0,
    titledist: 42319
  };
}();
var rule70 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42315,
    lowdist: 0,
    titledist: 42315
  };
}();
var rule71 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -207 | 0,
    lowdist: 0,
    titledist: -207 | 0
  };
}();
var rule72 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42280,
    lowdist: 0,
    titledist: 42280
  };
}();
var rule73 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42308,
    lowdist: 0,
    titledist: 42308
  };
}();
var rule74 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -209 | 0,
    lowdist: 0,
    titledist: -209 | 0
  };
}();
var rule75 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -211 | 0,
    lowdist: 0,
    titledist: -211 | 0
  };
}();
var rule76 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10743,
    lowdist: 0,
    titledist: 10743
  };
}();
var rule77 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42305,
    lowdist: 0,
    titledist: 42305
  };
}();
var rule78 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10749,
    lowdist: 0,
    titledist: 10749
  };
}();
var rule79 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -213 | 0,
    lowdist: 0,
    titledist: -213 | 0
  };
}();
var rule80 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -214 | 0,
    lowdist: 0,
    titledist: -214 | 0
  };
}();
var rule81 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10727,
    lowdist: 0,
    titledist: 10727
  };
}();
var rule82 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -218 | 0,
    lowdist: 0,
    titledist: -218 | 0
  };
}();
var rule83 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42307,
    lowdist: 0,
    titledist: 42307
  };
}();
var rule84 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42282,
    lowdist: 0,
    titledist: 42282
  };
}();
var rule85 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -69 | 0,
    lowdist: 0,
    titledist: -69 | 0
  };
}();
var rule86 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -217 | 0,
    lowdist: 0,
    titledist: -217 | 0
  };
}();
var rule87 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -71 | 0,
    lowdist: 0,
    titledist: -71 | 0
  };
}();
var rule88 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -219 | 0,
    lowdist: 0,
    titledist: -219 | 0
  };
}();
var rule89 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42261,
    lowdist: 0,
    titledist: 42261
  };
}();
var rule90 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42258,
    lowdist: 0,
    titledist: 42258
  };
}();
var rule99 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -38 | 0,
    lowdist: 0,
    titledist: -38 | 0
  };
}();
var gencatCS = 134217728;
var rule199 = /* @__PURE__ */ function() {
  return {
    category: gencatCS,
    unicodeCat: NUMCAT_CS.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatCO = 268435456;
var rule200 = /* @__PURE__ */ function() {
  return {
    category: gencatCO,
    unicodeCat: NUMCAT_CO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatCF = 65536;
var rule16 = /* @__PURE__ */ function() {
  return {
    category: gencatCF,
    unicodeCat: NUMCAT_CF.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatCC = 1;
var rule0 = /* @__PURE__ */ function() {
  return {
    category: gencatCC,
    unicodeCat: NUMCAT_CC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var convchars = [{
  start: 65,
  length: 26,
  convRule: rule9
}, {
  start: 97,
  length: 26,
  convRule: rule12
}, {
  start: 181,
  length: 1,
  convRule: rule18
}, {
  start: 192,
  length: 23,
  convRule: rule9
}, {
  start: 216,
  length: 7,
  convRule: rule9
}, {
  start: 224,
  length: 23,
  convRule: rule12
}, {
  start: 248,
  length: 7,
  convRule: rule12
}, {
  start: 255,
  length: 1,
  convRule: rule21
}, {
  start: 256,
  length: 1,
  convRule: rule22
}, {
  start: 257,
  length: 1,
  convRule: rule23
}, {
  start: 258,
  length: 1,
  convRule: rule22
}, {
  start: 259,
  length: 1,
  convRule: rule23
}, {
  start: 260,
  length: 1,
  convRule: rule22
}, {
  start: 261,
  length: 1,
  convRule: rule23
}, {
  start: 262,
  length: 1,
  convRule: rule22
}, {
  start: 263,
  length: 1,
  convRule: rule23
}, {
  start: 264,
  length: 1,
  convRule: rule22
}, {
  start: 265,
  length: 1,
  convRule: rule23
}, {
  start: 266,
  length: 1,
  convRule: rule22
}, {
  start: 267,
  length: 1,
  convRule: rule23
}, {
  start: 268,
  length: 1,
  convRule: rule22
}, {
  start: 269,
  length: 1,
  convRule: rule23
}, {
  start: 270,
  length: 1,
  convRule: rule22
}, {
  start: 271,
  length: 1,
  convRule: rule23
}, {
  start: 272,
  length: 1,
  convRule: rule22
}, {
  start: 273,
  length: 1,
  convRule: rule23
}, {
  start: 274,
  length: 1,
  convRule: rule22
}, {
  start: 275,
  length: 1,
  convRule: rule23
}, {
  start: 276,
  length: 1,
  convRule: rule22
}, {
  start: 277,
  length: 1,
  convRule: rule23
}, {
  start: 278,
  length: 1,
  convRule: rule22
}, {
  start: 279,
  length: 1,
  convRule: rule23
}, {
  start: 280,
  length: 1,
  convRule: rule22
}, {
  start: 281,
  length: 1,
  convRule: rule23
}, {
  start: 282,
  length: 1,
  convRule: rule22
}, {
  start: 283,
  length: 1,
  convRule: rule23
}, {
  start: 284,
  length: 1,
  convRule: rule22
}, {
  start: 285,
  length: 1,
  convRule: rule23
}, {
  start: 286,
  length: 1,
  convRule: rule22
}, {
  start: 287,
  length: 1,
  convRule: rule23
}, {
  start: 288,
  length: 1,
  convRule: rule22
}, {
  start: 289,
  length: 1,
  convRule: rule23
}, {
  start: 290,
  length: 1,
  convRule: rule22
}, {
  start: 291,
  length: 1,
  convRule: rule23
}, {
  start: 292,
  length: 1,
  convRule: rule22
}, {
  start: 293,
  length: 1,
  convRule: rule23
}, {
  start: 294,
  length: 1,
  convRule: rule22
}, {
  start: 295,
  length: 1,
  convRule: rule23
}, {
  start: 296,
  length: 1,
  convRule: rule22
}, {
  start: 297,
  length: 1,
  convRule: rule23
}, {
  start: 298,
  length: 1,
  convRule: rule22
}, {
  start: 299,
  length: 1,
  convRule: rule23
}, {
  start: 300,
  length: 1,
  convRule: rule22
}, {
  start: 301,
  length: 1,
  convRule: rule23
}, {
  start: 302,
  length: 1,
  convRule: rule22
}, {
  start: 303,
  length: 1,
  convRule: rule23
}, {
  start: 304,
  length: 1,
  convRule: rule24
}, {
  start: 305,
  length: 1,
  convRule: rule25
}, {
  start: 306,
  length: 1,
  convRule: rule22
}, {
  start: 307,
  length: 1,
  convRule: rule23
}, {
  start: 308,
  length: 1,
  convRule: rule22
}, {
  start: 309,
  length: 1,
  convRule: rule23
}, {
  start: 310,
  length: 1,
  convRule: rule22
}, {
  start: 311,
  length: 1,
  convRule: rule23
}, {
  start: 313,
  length: 1,
  convRule: rule22
}, {
  start: 314,
  length: 1,
  convRule: rule23
}, {
  start: 315,
  length: 1,
  convRule: rule22
}, {
  start: 316,
  length: 1,
  convRule: rule23
}, {
  start: 317,
  length: 1,
  convRule: rule22
}, {
  start: 318,
  length: 1,
  convRule: rule23
}, {
  start: 319,
  length: 1,
  convRule: rule22
}, {
  start: 320,
  length: 1,
  convRule: rule23
}, {
  start: 321,
  length: 1,
  convRule: rule22
}, {
  start: 322,
  length: 1,
  convRule: rule23
}, {
  start: 323,
  length: 1,
  convRule: rule22
}, {
  start: 324,
  length: 1,
  convRule: rule23
}, {
  start: 325,
  length: 1,
  convRule: rule22
}, {
  start: 326,
  length: 1,
  convRule: rule23
}, {
  start: 327,
  length: 1,
  convRule: rule22
}, {
  start: 328,
  length: 1,
  convRule: rule23
}, {
  start: 330,
  length: 1,
  convRule: rule22
}, {
  start: 331,
  length: 1,
  convRule: rule23
}, {
  start: 332,
  length: 1,
  convRule: rule22
}, {
  start: 333,
  length: 1,
  convRule: rule23
}, {
  start: 334,
  length: 1,
  convRule: rule22
}, {
  start: 335,
  length: 1,
  convRule: rule23
}, {
  start: 336,
  length: 1,
  convRule: rule22
}, {
  start: 337,
  length: 1,
  convRule: rule23
}, {
  start: 338,
  length: 1,
  convRule: rule22
}, {
  start: 339,
  length: 1,
  convRule: rule23
}, {
  start: 340,
  length: 1,
  convRule: rule22
}, {
  start: 341,
  length: 1,
  convRule: rule23
}, {
  start: 342,
  length: 1,
  convRule: rule22
}, {
  start: 343,
  length: 1,
  convRule: rule23
}, {
  start: 344,
  length: 1,
  convRule: rule22
}, {
  start: 345,
  length: 1,
  convRule: rule23
}, {
  start: 346,
  length: 1,
  convRule: rule22
}, {
  start: 347,
  length: 1,
  convRule: rule23
}, {
  start: 348,
  length: 1,
  convRule: rule22
}, {
  start: 349,
  length: 1,
  convRule: rule23
}, {
  start: 350,
  length: 1,
  convRule: rule22
}, {
  start: 351,
  length: 1,
  convRule: rule23
}, {
  start: 352,
  length: 1,
  convRule: rule22
}, {
  start: 353,
  length: 1,
  convRule: rule23
}, {
  start: 354,
  length: 1,
  convRule: rule22
}, {
  start: 355,
  length: 1,
  convRule: rule23
}, {
  start: 356,
  length: 1,
  convRule: rule22
}, {
  start: 357,
  length: 1,
  convRule: rule23
}, {
  start: 358,
  length: 1,
  convRule: rule22
}, {
  start: 359,
  length: 1,
  convRule: rule23
}, {
  start: 360,
  length: 1,
  convRule: rule22
}, {
  start: 361,
  length: 1,
  convRule: rule23
}, {
  start: 362,
  length: 1,
  convRule: rule22
}, {
  start: 363,
  length: 1,
  convRule: rule23
}, {
  start: 364,
  length: 1,
  convRule: rule22
}, {
  start: 365,
  length: 1,
  convRule: rule23
}, {
  start: 366,
  length: 1,
  convRule: rule22
}, {
  start: 367,
  length: 1,
  convRule: rule23
}, {
  start: 368,
  length: 1,
  convRule: rule22
}, {
  start: 369,
  length: 1,
  convRule: rule23
}, {
  start: 370,
  length: 1,
  convRule: rule22
}, {
  start: 371,
  length: 1,
  convRule: rule23
}, {
  start: 372,
  length: 1,
  convRule: rule22
}, {
  start: 373,
  length: 1,
  convRule: rule23
}, {
  start: 374,
  length: 1,
  convRule: rule22
}, {
  start: 375,
  length: 1,
  convRule: rule23
}, {
  start: 376,
  length: 1,
  convRule: rule26
}, {
  start: 377,
  length: 1,
  convRule: rule22
}, {
  start: 378,
  length: 1,
  convRule: rule23
}, {
  start: 379,
  length: 1,
  convRule: rule22
}, {
  start: 380,
  length: 1,
  convRule: rule23
}, {
  start: 381,
  length: 1,
  convRule: rule22
}, {
  start: 382,
  length: 1,
  convRule: rule23
}, {
  start: 383,
  length: 1,
  convRule: rule27
}, {
  start: 384,
  length: 1,
  convRule: rule28
}, {
  start: 385,
  length: 1,
  convRule: rule29
}, {
  start: 386,
  length: 1,
  convRule: rule22
}, {
  start: 387,
  length: 1,
  convRule: rule23
}, {
  start: 388,
  length: 1,
  convRule: rule22
}, {
  start: 389,
  length: 1,
  convRule: rule23
}, {
  start: 390,
  length: 1,
  convRule: rule30
}, {
  start: 391,
  length: 1,
  convRule: rule22
}, {
  start: 392,
  length: 1,
  convRule: rule23
}, {
  start: 393,
  length: 2,
  convRule: rule31
}, {
  start: 395,
  length: 1,
  convRule: rule22
}, {
  start: 396,
  length: 1,
  convRule: rule23
}, {
  start: 398,
  length: 1,
  convRule: rule32
}, {
  start: 399,
  length: 1,
  convRule: rule33
}, {
  start: 400,
  length: 1,
  convRule: rule34
}, {
  start: 401,
  length: 1,
  convRule: rule22
}, {
  start: 402,
  length: 1,
  convRule: rule23
}, {
  start: 403,
  length: 1,
  convRule: rule31
}, {
  start: 404,
  length: 1,
  convRule: rule35
}, {
  start: 405,
  length: 1,
  convRule: rule36
}, {
  start: 406,
  length: 1,
  convRule: rule37
}, {
  start: 407,
  length: 1,
  convRule: rule38
}, {
  start: 408,
  length: 1,
  convRule: rule22
}, {
  start: 409,
  length: 1,
  convRule: rule23
}, {
  start: 410,
  length: 1,
  convRule: rule39
}, {
  start: 412,
  length: 1,
  convRule: rule37
}, {
  start: 413,
  length: 1,
  convRule: rule40
}, {
  start: 414,
  length: 1,
  convRule: rule41
}, {
  start: 415,
  length: 1,
  convRule: rule42
}, {
  start: 416,
  length: 1,
  convRule: rule22
}, {
  start: 417,
  length: 1,
  convRule: rule23
}, {
  start: 418,
  length: 1,
  convRule: rule22
}, {
  start: 419,
  length: 1,
  convRule: rule23
}, {
  start: 420,
  length: 1,
  convRule: rule22
}, {
  start: 421,
  length: 1,
  convRule: rule23
}, {
  start: 422,
  length: 1,
  convRule: rule43
}, {
  start: 423,
  length: 1,
  convRule: rule22
}, {
  start: 424,
  length: 1,
  convRule: rule23
}, {
  start: 425,
  length: 1,
  convRule: rule43
}, {
  start: 428,
  length: 1,
  convRule: rule22
}, {
  start: 429,
  length: 1,
  convRule: rule23
}, {
  start: 430,
  length: 1,
  convRule: rule43
}, {
  start: 431,
  length: 1,
  convRule: rule22
}, {
  start: 432,
  length: 1,
  convRule: rule23
}, {
  start: 433,
  length: 2,
  convRule: rule44
}, {
  start: 435,
  length: 1,
  convRule: rule22
}, {
  start: 436,
  length: 1,
  convRule: rule23
}, {
  start: 437,
  length: 1,
  convRule: rule22
}, {
  start: 438,
  length: 1,
  convRule: rule23
}, {
  start: 439,
  length: 1,
  convRule: rule45
}, {
  start: 440,
  length: 1,
  convRule: rule22
}, {
  start: 441,
  length: 1,
  convRule: rule23
}, {
  start: 444,
  length: 1,
  convRule: rule22
}, {
  start: 445,
  length: 1,
  convRule: rule23
}, {
  start: 447,
  length: 1,
  convRule: rule46
}, {
  start: 452,
  length: 1,
  convRule: rule47
}, {
  start: 453,
  length: 1,
  convRule: rule48
}, {
  start: 454,
  length: 1,
  convRule: rule49
}, {
  start: 455,
  length: 1,
  convRule: rule47
}, {
  start: 456,
  length: 1,
  convRule: rule48
}, {
  start: 457,
  length: 1,
  convRule: rule49
}, {
  start: 458,
  length: 1,
  convRule: rule47
}, {
  start: 459,
  length: 1,
  convRule: rule48
}, {
  start: 460,
  length: 1,
  convRule: rule49
}, {
  start: 461,
  length: 1,
  convRule: rule22
}, {
  start: 462,
  length: 1,
  convRule: rule23
}, {
  start: 463,
  length: 1,
  convRule: rule22
}, {
  start: 464,
  length: 1,
  convRule: rule23
}, {
  start: 465,
  length: 1,
  convRule: rule22
}, {
  start: 466,
  length: 1,
  convRule: rule23
}, {
  start: 467,
  length: 1,
  convRule: rule22
}, {
  start: 468,
  length: 1,
  convRule: rule23
}, {
  start: 469,
  length: 1,
  convRule: rule22
}, {
  start: 470,
  length: 1,
  convRule: rule23
}, {
  start: 471,
  length: 1,
  convRule: rule22
}, {
  start: 472,
  length: 1,
  convRule: rule23
}, {
  start: 473,
  length: 1,
  convRule: rule22
}, {
  start: 474,
  length: 1,
  convRule: rule23
}, {
  start: 475,
  length: 1,
  convRule: rule22
}, {
  start: 476,
  length: 1,
  convRule: rule23
}, {
  start: 477,
  length: 1,
  convRule: rule50
}, {
  start: 478,
  length: 1,
  convRule: rule22
}, {
  start: 479,
  length: 1,
  convRule: rule23
}, {
  start: 480,
  length: 1,
  convRule: rule22
}, {
  start: 481,
  length: 1,
  convRule: rule23
}, {
  start: 482,
  length: 1,
  convRule: rule22
}, {
  start: 483,
  length: 1,
  convRule: rule23
}, {
  start: 484,
  length: 1,
  convRule: rule22
}, {
  start: 485,
  length: 1,
  convRule: rule23
}, {
  start: 486,
  length: 1,
  convRule: rule22
}, {
  start: 487,
  length: 1,
  convRule: rule23
}, {
  start: 488,
  length: 1,
  convRule: rule22
}, {
  start: 489,
  length: 1,
  convRule: rule23
}, {
  start: 490,
  length: 1,
  convRule: rule22
}, {
  start: 491,
  length: 1,
  convRule: rule23
}, {
  start: 492,
  length: 1,
  convRule: rule22
}, {
  start: 493,
  length: 1,
  convRule: rule23
}, {
  start: 494,
  length: 1,
  convRule: rule22
}, {
  start: 495,
  length: 1,
  convRule: rule23
}, {
  start: 497,
  length: 1,
  convRule: rule47
}, {
  start: 498,
  length: 1,
  convRule: rule48
}, {
  start: 499,
  length: 1,
  convRule: rule49
}, {
  start: 500,
  length: 1,
  convRule: rule22
}, {
  start: 501,
  length: 1,
  convRule: rule23
}, {
  start: 502,
  length: 1,
  convRule: rule51
}, {
  start: 503,
  length: 1,
  convRule: rule52
}, {
  start: 504,
  length: 1,
  convRule: rule22
}, {
  start: 505,
  length: 1,
  convRule: rule23
}, {
  start: 506,
  length: 1,
  convRule: rule22
}, {
  start: 507,
  length: 1,
  convRule: rule23
}, {
  start: 508,
  length: 1,
  convRule: rule22
}, {
  start: 509,
  length: 1,
  convRule: rule23
}, {
  start: 510,
  length: 1,
  convRule: rule22
}, {
  start: 511,
  length: 1,
  convRule: rule23
}, {
  start: 512,
  length: 1,
  convRule: rule22
}, {
  start: 513,
  length: 1,
  convRule: rule23
}, {
  start: 514,
  length: 1,
  convRule: rule22
}, {
  start: 515,
  length: 1,
  convRule: rule23
}, {
  start: 516,
  length: 1,
  convRule: rule22
}, {
  start: 517,
  length: 1,
  convRule: rule23
}, {
  start: 518,
  length: 1,
  convRule: rule22
}, {
  start: 519,
  length: 1,
  convRule: rule23
}, {
  start: 520,
  length: 1,
  convRule: rule22
}, {
  start: 521,
  length: 1,
  convRule: rule23
}, {
  start: 522,
  length: 1,
  convRule: rule22
}, {
  start: 523,
  length: 1,
  convRule: rule23
}, {
  start: 524,
  length: 1,
  convRule: rule22
}, {
  start: 525,
  length: 1,
  convRule: rule23
}, {
  start: 526,
  length: 1,
  convRule: rule22
}, {
  start: 527,
  length: 1,
  convRule: rule23
}, {
  start: 528,
  length: 1,
  convRule: rule22
}, {
  start: 529,
  length: 1,
  convRule: rule23
}, {
  start: 530,
  length: 1,
  convRule: rule22
}, {
  start: 531,
  length: 1,
  convRule: rule23
}, {
  start: 532,
  length: 1,
  convRule: rule22
}, {
  start: 533,
  length: 1,
  convRule: rule23
}, {
  start: 534,
  length: 1,
  convRule: rule22
}, {
  start: 535,
  length: 1,
  convRule: rule23
}, {
  start: 536,
  length: 1,
  convRule: rule22
}, {
  start: 537,
  length: 1,
  convRule: rule23
}, {
  start: 538,
  length: 1,
  convRule: rule22
}, {
  start: 539,
  length: 1,
  convRule: rule23
}, {
  start: 540,
  length: 1,
  convRule: rule22
}, {
  start: 541,
  length: 1,
  convRule: rule23
}, {
  start: 542,
  length: 1,
  convRule: rule22
}, {
  start: 543,
  length: 1,
  convRule: rule23
}, {
  start: 544,
  length: 1,
  convRule: rule53
}, {
  start: 546,
  length: 1,
  convRule: rule22
}, {
  start: 547,
  length: 1,
  convRule: rule23
}, {
  start: 548,
  length: 1,
  convRule: rule22
}, {
  start: 549,
  length: 1,
  convRule: rule23
}, {
  start: 550,
  length: 1,
  convRule: rule22
}, {
  start: 551,
  length: 1,
  convRule: rule23
}, {
  start: 552,
  length: 1,
  convRule: rule22
}, {
  start: 553,
  length: 1,
  convRule: rule23
}, {
  start: 554,
  length: 1,
  convRule: rule22
}, {
  start: 555,
  length: 1,
  convRule: rule23
}, {
  start: 556,
  length: 1,
  convRule: rule22
}, {
  start: 557,
  length: 1,
  convRule: rule23
}, {
  start: 558,
  length: 1,
  convRule: rule22
}, {
  start: 559,
  length: 1,
  convRule: rule23
}, {
  start: 560,
  length: 1,
  convRule: rule22
}, {
  start: 561,
  length: 1,
  convRule: rule23
}, {
  start: 562,
  length: 1,
  convRule: rule22
}, {
  start: 563,
  length: 1,
  convRule: rule23
}, {
  start: 570,
  length: 1,
  convRule: rule54
}, {
  start: 571,
  length: 1,
  convRule: rule22
}, {
  start: 572,
  length: 1,
  convRule: rule23
}, {
  start: 573,
  length: 1,
  convRule: rule55
}, {
  start: 574,
  length: 1,
  convRule: rule56
}, {
  start: 575,
  length: 2,
  convRule: rule57
}, {
  start: 577,
  length: 1,
  convRule: rule22
}, {
  start: 578,
  length: 1,
  convRule: rule23
}, {
  start: 579,
  length: 1,
  convRule: rule58
}, {
  start: 580,
  length: 1,
  convRule: rule59
}, {
  start: 581,
  length: 1,
  convRule: rule60
}, {
  start: 582,
  length: 1,
  convRule: rule22
}, {
  start: 583,
  length: 1,
  convRule: rule23
}, {
  start: 584,
  length: 1,
  convRule: rule22
}, {
  start: 585,
  length: 1,
  convRule: rule23
}, {
  start: 586,
  length: 1,
  convRule: rule22
}, {
  start: 587,
  length: 1,
  convRule: rule23
}, {
  start: 588,
  length: 1,
  convRule: rule22
}, {
  start: 589,
  length: 1,
  convRule: rule23
}, {
  start: 590,
  length: 1,
  convRule: rule22
}, {
  start: 591,
  length: 1,
  convRule: rule23
}, {
  start: 592,
  length: 1,
  convRule: rule61
}, {
  start: 593,
  length: 1,
  convRule: rule62
}, {
  start: 594,
  length: 1,
  convRule: rule63
}, {
  start: 595,
  length: 1,
  convRule: rule64
}, {
  start: 596,
  length: 1,
  convRule: rule65
}, {
  start: 598,
  length: 2,
  convRule: rule66
}, {
  start: 601,
  length: 1,
  convRule: rule67
}, {
  start: 603,
  length: 1,
  convRule: rule68
}, {
  start: 604,
  length: 1,
  convRule: rule69
}, {
  start: 608,
  length: 1,
  convRule: rule66
}, {
  start: 609,
  length: 1,
  convRule: rule70
}, {
  start: 611,
  length: 1,
  convRule: rule71
}, {
  start: 613,
  length: 1,
  convRule: rule72
}, {
  start: 614,
  length: 1,
  convRule: rule73
}, {
  start: 616,
  length: 1,
  convRule: rule74
}, {
  start: 617,
  length: 1,
  convRule: rule75
}, {
  start: 618,
  length: 1,
  convRule: rule73
}, {
  start: 619,
  length: 1,
  convRule: rule76
}, {
  start: 620,
  length: 1,
  convRule: rule77
}, {
  start: 623,
  length: 1,
  convRule: rule75
}, {
  start: 625,
  length: 1,
  convRule: rule78
}, {
  start: 626,
  length: 1,
  convRule: rule79
}, {
  start: 629,
  length: 1,
  convRule: rule80
}, {
  start: 637,
  length: 1,
  convRule: rule81
}, {
  start: 640,
  length: 1,
  convRule: rule82
}, {
  start: 642,
  length: 1,
  convRule: rule83
}, {
  start: 643,
  length: 1,
  convRule: rule82
}, {
  start: 647,
  length: 1,
  convRule: rule84
}, {
  start: 648,
  length: 1,
  convRule: rule82
}, {
  start: 649,
  length: 1,
  convRule: rule85
}, {
  start: 650,
  length: 2,
  convRule: rule86
}, {
  start: 652,
  length: 1,
  convRule: rule87
}, {
  start: 658,
  length: 1,
  convRule: rule88
}, {
  start: 669,
  length: 1,
  convRule: rule89
}, {
  start: 670,
  length: 1,
  convRule: rule90
}, {
  start: 837,
  length: 1,
  convRule: rule93
}, {
  start: 880,
  length: 1,
  convRule: rule22
}, {
  start: 881,
  length: 1,
  convRule: rule23
}, {
  start: 882,
  length: 1,
  convRule: rule22
}, {
  start: 883,
  length: 1,
  convRule: rule23
}, {
  start: 886,
  length: 1,
  convRule: rule22
}, {
  start: 887,
  length: 1,
  convRule: rule23
}, {
  start: 891,
  length: 3,
  convRule: rule41
}, {
  start: 895,
  length: 1,
  convRule: rule94
}, {
  start: 902,
  length: 1,
  convRule: rule95
}, {
  start: 904,
  length: 3,
  convRule: rule96
}, {
  start: 908,
  length: 1,
  convRule: rule97
}, {
  start: 910,
  length: 2,
  convRule: rule98
}, {
  start: 913,
  length: 17,
  convRule: rule9
}, {
  start: 931,
  length: 9,
  convRule: rule9
}, {
  start: 940,
  length: 1,
  convRule: rule99
}, {
  start: 941,
  length: 3,
  convRule: rule100
}, {
  start: 945,
  length: 17,
  convRule: rule12
}, {
  start: 962,
  length: 1,
  convRule: rule101
}, {
  start: 963,
  length: 9,
  convRule: rule12
}, {
  start: 972,
  length: 1,
  convRule: rule102
}, {
  start: 973,
  length: 2,
  convRule: rule103
}, {
  start: 975,
  length: 1,
  convRule: rule104
}, {
  start: 976,
  length: 1,
  convRule: rule105
}, {
  start: 977,
  length: 1,
  convRule: rule106
}, {
  start: 981,
  length: 1,
  convRule: rule108
}, {
  start: 982,
  length: 1,
  convRule: rule109
}, {
  start: 983,
  length: 1,
  convRule: rule110
}, {
  start: 984,
  length: 1,
  convRule: rule22
}, {
  start: 985,
  length: 1,
  convRule: rule23
}, {
  start: 986,
  length: 1,
  convRule: rule22
}, {
  start: 987,
  length: 1,
  convRule: rule23
}, {
  start: 988,
  length: 1,
  convRule: rule22
}, {
  start: 989,
  length: 1,
  convRule: rule23
}, {
  start: 990,
  length: 1,
  convRule: rule22
}, {
  start: 991,
  length: 1,
  convRule: rule23
}, {
  start: 992,
  length: 1,
  convRule: rule22
}, {
  start: 993,
  length: 1,
  convRule: rule23
}, {
  start: 994,
  length: 1,
  convRule: rule22
}, {
  start: 995,
  length: 1,
  convRule: rule23
}, {
  start: 996,
  length: 1,
  convRule: rule22
}, {
  start: 997,
  length: 1,
  convRule: rule23
}, {
  start: 998,
  length: 1,
  convRule: rule22
}, {
  start: 999,
  length: 1,
  convRule: rule23
}, {
  start: 1e3,
  length: 1,
  convRule: rule22
}, {
  start: 1001,
  length: 1,
  convRule: rule23
}, {
  start: 1002,
  length: 1,
  convRule: rule22
}, {
  start: 1003,
  length: 1,
  convRule: rule23
}, {
  start: 1004,
  length: 1,
  convRule: rule22
}, {
  start: 1005,
  length: 1,
  convRule: rule23
}, {
  start: 1006,
  length: 1,
  convRule: rule22
}, {
  start: 1007,
  length: 1,
  convRule: rule23
}, {
  start: 1008,
  length: 1,
  convRule: rule111
}, {
  start: 1009,
  length: 1,
  convRule: rule112
}, {
  start: 1010,
  length: 1,
  convRule: rule113
}, {
  start: 1011,
  length: 1,
  convRule: rule114
}, {
  start: 1012,
  length: 1,
  convRule: rule115
}, {
  start: 1013,
  length: 1,
  convRule: rule116
}, {
  start: 1015,
  length: 1,
  convRule: rule22
}, {
  start: 1016,
  length: 1,
  convRule: rule23
}, {
  start: 1017,
  length: 1,
  convRule: rule117
}, {
  start: 1018,
  length: 1,
  convRule: rule22
}, {
  start: 1019,
  length: 1,
  convRule: rule23
}, {
  start: 1021,
  length: 3,
  convRule: rule53
}, {
  start: 1024,
  length: 16,
  convRule: rule118
}, {
  start: 1040,
  length: 32,
  convRule: rule9
}, {
  start: 1072,
  length: 32,
  convRule: rule12
}, {
  start: 1104,
  length: 16,
  convRule: rule112
}, {
  start: 1120,
  length: 1,
  convRule: rule22
}, {
  start: 1121,
  length: 1,
  convRule: rule23
}, {
  start: 1122,
  length: 1,
  convRule: rule22
}, {
  start: 1123,
  length: 1,
  convRule: rule23
}, {
  start: 1124,
  length: 1,
  convRule: rule22
}, {
  start: 1125,
  length: 1,
  convRule: rule23
}, {
  start: 1126,
  length: 1,
  convRule: rule22
}, {
  start: 1127,
  length: 1,
  convRule: rule23
}, {
  start: 1128,
  length: 1,
  convRule: rule22
}, {
  start: 1129,
  length: 1,
  convRule: rule23
}, {
  start: 1130,
  length: 1,
  convRule: rule22
}, {
  start: 1131,
  length: 1,
  convRule: rule23
}, {
  start: 1132,
  length: 1,
  convRule: rule22
}, {
  start: 1133,
  length: 1,
  convRule: rule23
}, {
  start: 1134,
  length: 1,
  convRule: rule22
}, {
  start: 1135,
  length: 1,
  convRule: rule23
}, {
  start: 1136,
  length: 1,
  convRule: rule22
}, {
  start: 1137,
  length: 1,
  convRule: rule23
}, {
  start: 1138,
  length: 1,
  convRule: rule22
}, {
  start: 1139,
  length: 1,
  convRule: rule23
}, {
  start: 1140,
  length: 1,
  convRule: rule22
}, {
  start: 1141,
  length: 1,
  convRule: rule23
}, {
  start: 1142,
  length: 1,
  convRule: rule22
}, {
  start: 1143,
  length: 1,
  convRule: rule23
}, {
  start: 1144,
  length: 1,
  convRule: rule22
}, {
  start: 1145,
  length: 1,
  convRule: rule23
}, {
  start: 1146,
  length: 1,
  convRule: rule22
}, {
  start: 1147,
  length: 1,
  convRule: rule23
}, {
  start: 1148,
  length: 1,
  convRule: rule22
}, {
  start: 1149,
  length: 1,
  convRule: rule23
}, {
  start: 1150,
  length: 1,
  convRule: rule22
}, {
  start: 1151,
  length: 1,
  convRule: rule23
}, {
  start: 1152,
  length: 1,
  convRule: rule22
}, {
  start: 1153,
  length: 1,
  convRule: rule23
}, {
  start: 1162,
  length: 1,
  convRule: rule22
}, {
  start: 1163,
  length: 1,
  convRule: rule23
}, {
  start: 1164,
  length: 1,
  convRule: rule22
}, {
  start: 1165,
  length: 1,
  convRule: rule23
}, {
  start: 1166,
  length: 1,
  convRule: rule22
}, {
  start: 1167,
  length: 1,
  convRule: rule23
}, {
  start: 1168,
  length: 1,
  convRule: rule22
}, {
  start: 1169,
  length: 1,
  convRule: rule23
}, {
  start: 1170,
  length: 1,
  convRule: rule22
}, {
  start: 1171,
  length: 1,
  convRule: rule23
}, {
  start: 1172,
  length: 1,
  convRule: rule22
}, {
  start: 1173,
  length: 1,
  convRule: rule23
}, {
  start: 1174,
  length: 1,
  convRule: rule22
}, {
  start: 1175,
  length: 1,
  convRule: rule23
}, {
  start: 1176,
  length: 1,
  convRule: rule22
}, {
  start: 1177,
  length: 1,
  convRule: rule23
}, {
  start: 1178,
  length: 1,
  convRule: rule22
}, {
  start: 1179,
  length: 1,
  convRule: rule23
}, {
  start: 1180,
  length: 1,
  convRule: rule22
}, {
  start: 1181,
  length: 1,
  convRule: rule23
}, {
  start: 1182,
  length: 1,
  convRule: rule22
}, {
  start: 1183,
  length: 1,
  convRule: rule23
}, {
  start: 1184,
  length: 1,
  convRule: rule22
}, {
  start: 1185,
  length: 1,
  convRule: rule23
}, {
  start: 1186,
  length: 1,
  convRule: rule22
}, {
  start: 1187,
  length: 1,
  convRule: rule23
}, {
  start: 1188,
  length: 1,
  convRule: rule22
}, {
  start: 1189,
  length: 1,
  convRule: rule23
}, {
  start: 1190,
  length: 1,
  convRule: rule22
}, {
  start: 1191,
  length: 1,
  convRule: rule23
}, {
  start: 1192,
  length: 1,
  convRule: rule22
}, {
  start: 1193,
  length: 1,
  convRule: rule23
}, {
  start: 1194,
  length: 1,
  convRule: rule22
}, {
  start: 1195,
  length: 1,
  convRule: rule23
}, {
  start: 1196,
  length: 1,
  convRule: rule22
}, {
  start: 1197,
  length: 1,
  convRule: rule23
}, {
  start: 1198,
  length: 1,
  convRule: rule22
}, {
  start: 1199,
  length: 1,
  convRule: rule23
}, {
  start: 1200,
  length: 1,
  convRule: rule22
}, {
  start: 1201,
  length: 1,
  convRule: rule23
}, {
  start: 1202,
  length: 1,
  convRule: rule22
}, {
  start: 1203,
  length: 1,
  convRule: rule23
}, {
  start: 1204,
  length: 1,
  convRule: rule22
}, {
  start: 1205,
  length: 1,
  convRule: rule23
}, {
  start: 1206,
  length: 1,
  convRule: rule22
}, {
  start: 1207,
  length: 1,
  convRule: rule23
}, {
  start: 1208,
  length: 1,
  convRule: rule22
}, {
  start: 1209,
  length: 1,
  convRule: rule23
}, {
  start: 1210,
  length: 1,
  convRule: rule22
}, {
  start: 1211,
  length: 1,
  convRule: rule23
}, {
  start: 1212,
  length: 1,
  convRule: rule22
}, {
  start: 1213,
  length: 1,
  convRule: rule23
}, {
  start: 1214,
  length: 1,
  convRule: rule22
}, {
  start: 1215,
  length: 1,
  convRule: rule23
}, {
  start: 1216,
  length: 1,
  convRule: rule120
}, {
  start: 1217,
  length: 1,
  convRule: rule22
}, {
  start: 1218,
  length: 1,
  convRule: rule23
}, {
  start: 1219,
  length: 1,
  convRule: rule22
}, {
  start: 1220,
  length: 1,
  convRule: rule23
}, {
  start: 1221,
  length: 1,
  convRule: rule22
}, {
  start: 1222,
  length: 1,
  convRule: rule23
}, {
  start: 1223,
  length: 1,
  convRule: rule22
}, {
  start: 1224,
  length: 1,
  convRule: rule23
}, {
  start: 1225,
  length: 1,
  convRule: rule22
}, {
  start: 1226,
  length: 1,
  convRule: rule23
}, {
  start: 1227,
  length: 1,
  convRule: rule22
}, {
  start: 1228,
  length: 1,
  convRule: rule23
}, {
  start: 1229,
  length: 1,
  convRule: rule22
}, {
  start: 1230,
  length: 1,
  convRule: rule23
}, {
  start: 1231,
  length: 1,
  convRule: rule121
}, {
  start: 1232,
  length: 1,
  convRule: rule22
}, {
  start: 1233,
  length: 1,
  convRule: rule23
}, {
  start: 1234,
  length: 1,
  convRule: rule22
}, {
  start: 1235,
  length: 1,
  convRule: rule23
}, {
  start: 1236,
  length: 1,
  convRule: rule22
}, {
  start: 1237,
  length: 1,
  convRule: rule23
}, {
  start: 1238,
  length: 1,
  convRule: rule22
}, {
  start: 1239,
  length: 1,
  convRule: rule23
}, {
  start: 1240,
  length: 1,
  convRule: rule22
}, {
  start: 1241,
  length: 1,
  convRule: rule23
}, {
  start: 1242,
  length: 1,
  convRule: rule22
}, {
  start: 1243,
  length: 1,
  convRule: rule23
}, {
  start: 1244,
  length: 1,
  convRule: rule22
}, {
  start: 1245,
  length: 1,
  convRule: rule23
}, {
  start: 1246,
  length: 1,
  convRule: rule22
}, {
  start: 1247,
  length: 1,
  convRule: rule23
}, {
  start: 1248,
  length: 1,
  convRule: rule22
}, {
  start: 1249,
  length: 1,
  convRule: rule23
}, {
  start: 1250,
  length: 1,
  convRule: rule22
}, {
  start: 1251,
  length: 1,
  convRule: rule23
}, {
  start: 1252,
  length: 1,
  convRule: rule22
}, {
  start: 1253,
  length: 1,
  convRule: rule23
}, {
  start: 1254,
  length: 1,
  convRule: rule22
}, {
  start: 1255,
  length: 1,
  convRule: rule23
}, {
  start: 1256,
  length: 1,
  convRule: rule22
}, {
  start: 1257,
  length: 1,
  convRule: rule23
}, {
  start: 1258,
  length: 1,
  convRule: rule22
}, {
  start: 1259,
  length: 1,
  convRule: rule23
}, {
  start: 1260,
  length: 1,
  convRule: rule22
}, {
  start: 1261,
  length: 1,
  convRule: rule23
}, {
  start: 1262,
  length: 1,
  convRule: rule22
}, {
  start: 1263,
  length: 1,
  convRule: rule23
}, {
  start: 1264,
  length: 1,
  convRule: rule22
}, {
  start: 1265,
  length: 1,
  convRule: rule23
}, {
  start: 1266,
  length: 1,
  convRule: rule22
}, {
  start: 1267,
  length: 1,
  convRule: rule23
}, {
  start: 1268,
  length: 1,
  convRule: rule22
}, {
  start: 1269,
  length: 1,
  convRule: rule23
}, {
  start: 1270,
  length: 1,
  convRule: rule22
}, {
  start: 1271,
  length: 1,
  convRule: rule23
}, {
  start: 1272,
  length: 1,
  convRule: rule22
}, {
  start: 1273,
  length: 1,
  convRule: rule23
}, {
  start: 1274,
  length: 1,
  convRule: rule22
}, {
  start: 1275,
  length: 1,
  convRule: rule23
}, {
  start: 1276,
  length: 1,
  convRule: rule22
}, {
  start: 1277,
  length: 1,
  convRule: rule23
}, {
  start: 1278,
  length: 1,
  convRule: rule22
}, {
  start: 1279,
  length: 1,
  convRule: rule23
}, {
  start: 1280,
  length: 1,
  convRule: rule22
}, {
  start: 1281,
  length: 1,
  convRule: rule23
}, {
  start: 1282,
  length: 1,
  convRule: rule22
}, {
  start: 1283,
  length: 1,
  convRule: rule23
}, {
  start: 1284,
  length: 1,
  convRule: rule22
}, {
  start: 1285,
  length: 1,
  convRule: rule23
}, {
  start: 1286,
  length: 1,
  convRule: rule22
}, {
  start: 1287,
  length: 1,
  convRule: rule23
}, {
  start: 1288,
  length: 1,
  convRule: rule22
}, {
  start: 1289,
  length: 1,
  convRule: rule23
}, {
  start: 1290,
  length: 1,
  convRule: rule22
}, {
  start: 1291,
  length: 1,
  convRule: rule23
}, {
  start: 1292,
  length: 1,
  convRule: rule22
}, {
  start: 1293,
  length: 1,
  convRule: rule23
}, {
  start: 1294,
  length: 1,
  convRule: rule22
}, {
  start: 1295,
  length: 1,
  convRule: rule23
}, {
  start: 1296,
  length: 1,
  convRule: rule22
}, {
  start: 1297,
  length: 1,
  convRule: rule23
}, {
  start: 1298,
  length: 1,
  convRule: rule22
}, {
  start: 1299,
  length: 1,
  convRule: rule23
}, {
  start: 1300,
  length: 1,
  convRule: rule22
}, {
  start: 1301,
  length: 1,
  convRule: rule23
}, {
  start: 1302,
  length: 1,
  convRule: rule22
}, {
  start: 1303,
  length: 1,
  convRule: rule23
}, {
  start: 1304,
  length: 1,
  convRule: rule22
}, {
  start: 1305,
  length: 1,
  convRule: rule23
}, {
  start: 1306,
  length: 1,
  convRule: rule22
}, {
  start: 1307,
  length: 1,
  convRule: rule23
}, {
  start: 1308,
  length: 1,
  convRule: rule22
}, {
  start: 1309,
  length: 1,
  convRule: rule23
}, {
  start: 1310,
  length: 1,
  convRule: rule22
}, {
  start: 1311,
  length: 1,
  convRule: rule23
}, {
  start: 1312,
  length: 1,
  convRule: rule22
}, {
  start: 1313,
  length: 1,
  convRule: rule23
}, {
  start: 1314,
  length: 1,
  convRule: rule22
}, {
  start: 1315,
  length: 1,
  convRule: rule23
}, {
  start: 1316,
  length: 1,
  convRule: rule22
}, {
  start: 1317,
  length: 1,
  convRule: rule23
}, {
  start: 1318,
  length: 1,
  convRule: rule22
}, {
  start: 1319,
  length: 1,
  convRule: rule23
}, {
  start: 1320,
  length: 1,
  convRule: rule22
}, {
  start: 1321,
  length: 1,
  convRule: rule23
}, {
  start: 1322,
  length: 1,
  convRule: rule22
}, {
  start: 1323,
  length: 1,
  convRule: rule23
}, {
  start: 1324,
  length: 1,
  convRule: rule22
}, {
  start: 1325,
  length: 1,
  convRule: rule23
}, {
  start: 1326,
  length: 1,
  convRule: rule22
}, {
  start: 1327,
  length: 1,
  convRule: rule23
}, {
  start: 1329,
  length: 38,
  convRule: rule122
}, {
  start: 1377,
  length: 38,
  convRule: rule123
}, {
  start: 4256,
  length: 38,
  convRule: rule125
}, {
  start: 4295,
  length: 1,
  convRule: rule125
}, {
  start: 4301,
  length: 1,
  convRule: rule125
}, {
  start: 4304,
  length: 43,
  convRule: rule126
}, {
  start: 4349,
  length: 3,
  convRule: rule126
}, {
  start: 5024,
  length: 80,
  convRule: rule127
}, {
  start: 5104,
  length: 6,
  convRule: rule104
}, {
  start: 5112,
  length: 6,
  convRule: rule110
}, {
  start: 7296,
  length: 1,
  convRule: rule129
}, {
  start: 7297,
  length: 1,
  convRule: rule130
}, {
  start: 7298,
  length: 1,
  convRule: rule131
}, {
  start: 7299,
  length: 2,
  convRule: rule132
}, {
  start: 7301,
  length: 1,
  convRule: rule133
}, {
  start: 7302,
  length: 1,
  convRule: rule134
}, {
  start: 7303,
  length: 1,
  convRule: rule135
}, {
  start: 7304,
  length: 1,
  convRule: rule136
}, {
  start: 7312,
  length: 43,
  convRule: rule137
}, {
  start: 7357,
  length: 3,
  convRule: rule137
}, {
  start: 7545,
  length: 1,
  convRule: rule138
}, {
  start: 7549,
  length: 1,
  convRule: rule139
}, {
  start: 7566,
  length: 1,
  convRule: rule140
}, {
  start: 7680,
  length: 1,
  convRule: rule22
}, {
  start: 7681,
  length: 1,
  convRule: rule23
}, {
  start: 7682,
  length: 1,
  convRule: rule22
}, {
  start: 7683,
  length: 1,
  convRule: rule23
}, {
  start: 7684,
  length: 1,
  convRule: rule22
}, {
  start: 7685,
  length: 1,
  convRule: rule23
}, {
  start: 7686,
  length: 1,
  convRule: rule22
}, {
  start: 7687,
  length: 1,
  convRule: rule23
}, {
  start: 7688,
  length: 1,
  convRule: rule22
}, {
  start: 7689,
  length: 1,
  convRule: rule23
}, {
  start: 7690,
  length: 1,
  convRule: rule22
}, {
  start: 7691,
  length: 1,
  convRule: rule23
}, {
  start: 7692,
  length: 1,
  convRule: rule22
}, {
  start: 7693,
  length: 1,
  convRule: rule23
}, {
  start: 7694,
  length: 1,
  convRule: rule22
}, {
  start: 7695,
  length: 1,
  convRule: rule23
}, {
  start: 7696,
  length: 1,
  convRule: rule22
}, {
  start: 7697,
  length: 1,
  convRule: rule23
}, {
  start: 7698,
  length: 1,
  convRule: rule22
}, {
  start: 7699,
  length: 1,
  convRule: rule23
}, {
  start: 7700,
  length: 1,
  convRule: rule22
}, {
  start: 7701,
  length: 1,
  convRule: rule23
}, {
  start: 7702,
  length: 1,
  convRule: rule22
}, {
  start: 7703,
  length: 1,
  convRule: rule23
}, {
  start: 7704,
  length: 1,
  convRule: rule22
}, {
  start: 7705,
  length: 1,
  convRule: rule23
}, {
  start: 7706,
  length: 1,
  convRule: rule22
}, {
  start: 7707,
  length: 1,
  convRule: rule23
}, {
  start: 7708,
  length: 1,
  convRule: rule22
}, {
  start: 7709,
  length: 1,
  convRule: rule23
}, {
  start: 7710,
  length: 1,
  convRule: rule22
}, {
  start: 7711,
  length: 1,
  convRule: rule23
}, {
  start: 7712,
  length: 1,
  convRule: rule22
}, {
  start: 7713,
  length: 1,
  convRule: rule23
}, {
  start: 7714,
  length: 1,
  convRule: rule22
}, {
  start: 7715,
  length: 1,
  convRule: rule23
}, {
  start: 7716,
  length: 1,
  convRule: rule22
}, {
  start: 7717,
  length: 1,
  convRule: rule23
}, {
  start: 7718,
  length: 1,
  convRule: rule22
}, {
  start: 7719,
  length: 1,
  convRule: rule23
}, {
  start: 7720,
  length: 1,
  convRule: rule22
}, {
  start: 7721,
  length: 1,
  convRule: rule23
}, {
  start: 7722,
  length: 1,
  convRule: rule22
}, {
  start: 7723,
  length: 1,
  convRule: rule23
}, {
  start: 7724,
  length: 1,
  convRule: rule22
}, {
  start: 7725,
  length: 1,
  convRule: rule23
}, {
  start: 7726,
  length: 1,
  convRule: rule22
}, {
  start: 7727,
  length: 1,
  convRule: rule23
}, {
  start: 7728,
  length: 1,
  convRule: rule22
}, {
  start: 7729,
  length: 1,
  convRule: rule23
}, {
  start: 7730,
  length: 1,
  convRule: rule22
}, {
  start: 7731,
  length: 1,
  convRule: rule23
}, {
  start: 7732,
  length: 1,
  convRule: rule22
}, {
  start: 7733,
  length: 1,
  convRule: rule23
}, {
  start: 7734,
  length: 1,
  convRule: rule22
}, {
  start: 7735,
  length: 1,
  convRule: rule23
}, {
  start: 7736,
  length: 1,
  convRule: rule22
}, {
  start: 7737,
  length: 1,
  convRule: rule23
}, {
  start: 7738,
  length: 1,
  convRule: rule22
}, {
  start: 7739,
  length: 1,
  convRule: rule23
}, {
  start: 7740,
  length: 1,
  convRule: rule22
}, {
  start: 7741,
  length: 1,
  convRule: rule23
}, {
  start: 7742,
  length: 1,
  convRule: rule22
}, {
  start: 7743,
  length: 1,
  convRule: rule23
}, {
  start: 7744,
  length: 1,
  convRule: rule22
}, {
  start: 7745,
  length: 1,
  convRule: rule23
}, {
  start: 7746,
  length: 1,
  convRule: rule22
}, {
  start: 7747,
  length: 1,
  convRule: rule23
}, {
  start: 7748,
  length: 1,
  convRule: rule22
}, {
  start: 7749,
  length: 1,
  convRule: rule23
}, {
  start: 7750,
  length: 1,
  convRule: rule22
}, {
  start: 7751,
  length: 1,
  convRule: rule23
}, {
  start: 7752,
  length: 1,
  convRule: rule22
}, {
  start: 7753,
  length: 1,
  convRule: rule23
}, {
  start: 7754,
  length: 1,
  convRule: rule22
}, {
  start: 7755,
  length: 1,
  convRule: rule23
}, {
  start: 7756,
  length: 1,
  convRule: rule22
}, {
  start: 7757,
  length: 1,
  convRule: rule23
}, {
  start: 7758,
  length: 1,
  convRule: rule22
}, {
  start: 7759,
  length: 1,
  convRule: rule23
}, {
  start: 7760,
  length: 1,
  convRule: rule22
}, {
  start: 7761,
  length: 1,
  convRule: rule23
}, {
  start: 7762,
  length: 1,
  convRule: rule22
}, {
  start: 7763,
  length: 1,
  convRule: rule23
}, {
  start: 7764,
  length: 1,
  convRule: rule22
}, {
  start: 7765,
  length: 1,
  convRule: rule23
}, {
  start: 7766,
  length: 1,
  convRule: rule22
}, {
  start: 7767,
  length: 1,
  convRule: rule23
}, {
  start: 7768,
  length: 1,
  convRule: rule22
}, {
  start: 7769,
  length: 1,
  convRule: rule23
}, {
  start: 7770,
  length: 1,
  convRule: rule22
}, {
  start: 7771,
  length: 1,
  convRule: rule23
}, {
  start: 7772,
  length: 1,
  convRule: rule22
}, {
  start: 7773,
  length: 1,
  convRule: rule23
}, {
  start: 7774,
  length: 1,
  convRule: rule22
}, {
  start: 7775,
  length: 1,
  convRule: rule23
}, {
  start: 7776,
  length: 1,
  convRule: rule22
}, {
  start: 7777,
  length: 1,
  convRule: rule23
}, {
  start: 7778,
  length: 1,
  convRule: rule22
}, {
  start: 7779,
  length: 1,
  convRule: rule23
}, {
  start: 7780,
  length: 1,
  convRule: rule22
}, {
  start: 7781,
  length: 1,
  convRule: rule23
}, {
  start: 7782,
  length: 1,
  convRule: rule22
}, {
  start: 7783,
  length: 1,
  convRule: rule23
}, {
  start: 7784,
  length: 1,
  convRule: rule22
}, {
  start: 7785,
  length: 1,
  convRule: rule23
}, {
  start: 7786,
  length: 1,
  convRule: rule22
}, {
  start: 7787,
  length: 1,
  convRule: rule23
}, {
  start: 7788,
  length: 1,
  convRule: rule22
}, {
  start: 7789,
  length: 1,
  convRule: rule23
}, {
  start: 7790,
  length: 1,
  convRule: rule22
}, {
  start: 7791,
  length: 1,
  convRule: rule23
}, {
  start: 7792,
  length: 1,
  convRule: rule22
}, {
  start: 7793,
  length: 1,
  convRule: rule23
}, {
  start: 7794,
  length: 1,
  convRule: rule22
}, {
  start: 7795,
  length: 1,
  convRule: rule23
}, {
  start: 7796,
  length: 1,
  convRule: rule22
}, {
  start: 7797,
  length: 1,
  convRule: rule23
}, {
  start: 7798,
  length: 1,
  convRule: rule22
}, {
  start: 7799,
  length: 1,
  convRule: rule23
}, {
  start: 7800,
  length: 1,
  convRule: rule22
}, {
  start: 7801,
  length: 1,
  convRule: rule23
}, {
  start: 7802,
  length: 1,
  convRule: rule22
}, {
  start: 7803,
  length: 1,
  convRule: rule23
}, {
  start: 7804,
  length: 1,
  convRule: rule22
}, {
  start: 7805,
  length: 1,
  convRule: rule23
}, {
  start: 7806,
  length: 1,
  convRule: rule22
}, {
  start: 7807,
  length: 1,
  convRule: rule23
}, {
  start: 7808,
  length: 1,
  convRule: rule22
}, {
  start: 7809,
  length: 1,
  convRule: rule23
}, {
  start: 7810,
  length: 1,
  convRule: rule22
}, {
  start: 7811,
  length: 1,
  convRule: rule23
}, {
  start: 7812,
  length: 1,
  convRule: rule22
}, {
  start: 7813,
  length: 1,
  convRule: rule23
}, {
  start: 7814,
  length: 1,
  convRule: rule22
}, {
  start: 7815,
  length: 1,
  convRule: rule23
}, {
  start: 7816,
  length: 1,
  convRule: rule22
}, {
  start: 7817,
  length: 1,
  convRule: rule23
}, {
  start: 7818,
  length: 1,
  convRule: rule22
}, {
  start: 7819,
  length: 1,
  convRule: rule23
}, {
  start: 7820,
  length: 1,
  convRule: rule22
}, {
  start: 7821,
  length: 1,
  convRule: rule23
}, {
  start: 7822,
  length: 1,
  convRule: rule22
}, {
  start: 7823,
  length: 1,
  convRule: rule23
}, {
  start: 7824,
  length: 1,
  convRule: rule22
}, {
  start: 7825,
  length: 1,
  convRule: rule23
}, {
  start: 7826,
  length: 1,
  convRule: rule22
}, {
  start: 7827,
  length: 1,
  convRule: rule23
}, {
  start: 7828,
  length: 1,
  convRule: rule22
}, {
  start: 7829,
  length: 1,
  convRule: rule23
}, {
  start: 7835,
  length: 1,
  convRule: rule141
}, {
  start: 7838,
  length: 1,
  convRule: rule142
}, {
  start: 7840,
  length: 1,
  convRule: rule22
}, {
  start: 7841,
  length: 1,
  convRule: rule23
}, {
  start: 7842,
  length: 1,
  convRule: rule22
}, {
  start: 7843,
  length: 1,
  convRule: rule23
}, {
  start: 7844,
  length: 1,
  convRule: rule22
}, {
  start: 7845,
  length: 1,
  convRule: rule23
}, {
  start: 7846,
  length: 1,
  convRule: rule22
}, {
  start: 7847,
  length: 1,
  convRule: rule23
}, {
  start: 7848,
  length: 1,
  convRule: rule22
}, {
  start: 7849,
  length: 1,
  convRule: rule23
}, {
  start: 7850,
  length: 1,
  convRule: rule22
}, {
  start: 7851,
  length: 1,
  convRule: rule23
}, {
  start: 7852,
  length: 1,
  convRule: rule22
}, {
  start: 7853,
  length: 1,
  convRule: rule23
}, {
  start: 7854,
  length: 1,
  convRule: rule22
}, {
  start: 7855,
  length: 1,
  convRule: rule23
}, {
  start: 7856,
  length: 1,
  convRule: rule22
}, {
  start: 7857,
  length: 1,
  convRule: rule23
}, {
  start: 7858,
  length: 1,
  convRule: rule22
}, {
  start: 7859,
  length: 1,
  convRule: rule23
}, {
  start: 7860,
  length: 1,
  convRule: rule22
}, {
  start: 7861,
  length: 1,
  convRule: rule23
}, {
  start: 7862,
  length: 1,
  convRule: rule22
}, {
  start: 7863,
  length: 1,
  convRule: rule23
}, {
  start: 7864,
  length: 1,
  convRule: rule22
}, {
  start: 7865,
  length: 1,
  convRule: rule23
}, {
  start: 7866,
  length: 1,
  convRule: rule22
}, {
  start: 7867,
  length: 1,
  convRule: rule23
}, {
  start: 7868,
  length: 1,
  convRule: rule22
}, {
  start: 7869,
  length: 1,
  convRule: rule23
}, {
  start: 7870,
  length: 1,
  convRule: rule22
}, {
  start: 7871,
  length: 1,
  convRule: rule23
}, {
  start: 7872,
  length: 1,
  convRule: rule22
}, {
  start: 7873,
  length: 1,
  convRule: rule23
}, {
  start: 7874,
  length: 1,
  convRule: rule22
}, {
  start: 7875,
  length: 1,
  convRule: rule23
}, {
  start: 7876,
  length: 1,
  convRule: rule22
}, {
  start: 7877,
  length: 1,
  convRule: rule23
}, {
  start: 7878,
  length: 1,
  convRule: rule22
}, {
  start: 7879,
  length: 1,
  convRule: rule23
}, {
  start: 7880,
  length: 1,
  convRule: rule22
}, {
  start: 7881,
  length: 1,
  convRule: rule23
}, {
  start: 7882,
  length: 1,
  convRule: rule22
}, {
  start: 7883,
  length: 1,
  convRule: rule23
}, {
  start: 7884,
  length: 1,
  convRule: rule22
}, {
  start: 7885,
  length: 1,
  convRule: rule23
}, {
  start: 7886,
  length: 1,
  convRule: rule22
}, {
  start: 7887,
  length: 1,
  convRule: rule23
}, {
  start: 7888,
  length: 1,
  convRule: rule22
}, {
  start: 7889,
  length: 1,
  convRule: rule23
}, {
  start: 7890,
  length: 1,
  convRule: rule22
}, {
  start: 7891,
  length: 1,
  convRule: rule23
}, {
  start: 7892,
  length: 1,
  convRule: rule22
}, {
  start: 7893,
  length: 1,
  convRule: rule23
}, {
  start: 7894,
  length: 1,
  convRule: rule22
}, {
  start: 7895,
  length: 1,
  convRule: rule23
}, {
  start: 7896,
  length: 1,
  convRule: rule22
}, {
  start: 7897,
  length: 1,
  convRule: rule23
}, {
  start: 7898,
  length: 1,
  convRule: rule22
}, {
  start: 7899,
  length: 1,
  convRule: rule23
}, {
  start: 7900,
  length: 1,
  convRule: rule22
}, {
  start: 7901,
  length: 1,
  convRule: rule23
}, {
  start: 7902,
  length: 1,
  convRule: rule22
}, {
  start: 7903,
  length: 1,
  convRule: rule23
}, {
  start: 7904,
  length: 1,
  convRule: rule22
}, {
  start: 7905,
  length: 1,
  convRule: rule23
}, {
  start: 7906,
  length: 1,
  convRule: rule22
}, {
  start: 7907,
  length: 1,
  convRule: rule23
}, {
  start: 7908,
  length: 1,
  convRule: rule22
}, {
  start: 7909,
  length: 1,
  convRule: rule23
}, {
  start: 7910,
  length: 1,
  convRule: rule22
}, {
  start: 7911,
  length: 1,
  convRule: rule23
}, {
  start: 7912,
  length: 1,
  convRule: rule22
}, {
  start: 7913,
  length: 1,
  convRule: rule23
}, {
  start: 7914,
  length: 1,
  convRule: rule22
}, {
  start: 7915,
  length: 1,
  convRule: rule23
}, {
  start: 7916,
  length: 1,
  convRule: rule22
}, {
  start: 7917,
  length: 1,
  convRule: rule23
}, {
  start: 7918,
  length: 1,
  convRule: rule22
}, {
  start: 7919,
  length: 1,
  convRule: rule23
}, {
  start: 7920,
  length: 1,
  convRule: rule22
}, {
  start: 7921,
  length: 1,
  convRule: rule23
}, {
  start: 7922,
  length: 1,
  convRule: rule22
}, {
  start: 7923,
  length: 1,
  convRule: rule23
}, {
  start: 7924,
  length: 1,
  convRule: rule22
}, {
  start: 7925,
  length: 1,
  convRule: rule23
}, {
  start: 7926,
  length: 1,
  convRule: rule22
}, {
  start: 7927,
  length: 1,
  convRule: rule23
}, {
  start: 7928,
  length: 1,
  convRule: rule22
}, {
  start: 7929,
  length: 1,
  convRule: rule23
}, {
  start: 7930,
  length: 1,
  convRule: rule22
}, {
  start: 7931,
  length: 1,
  convRule: rule23
}, {
  start: 7932,
  length: 1,
  convRule: rule22
}, {
  start: 7933,
  length: 1,
  convRule: rule23
}, {
  start: 7934,
  length: 1,
  convRule: rule22
}, {
  start: 7935,
  length: 1,
  convRule: rule23
}, {
  start: 7936,
  length: 8,
  convRule: rule143
}, {
  start: 7944,
  length: 8,
  convRule: rule144
}, {
  start: 7952,
  length: 6,
  convRule: rule143
}, {
  start: 7960,
  length: 6,
  convRule: rule144
}, {
  start: 7968,
  length: 8,
  convRule: rule143
}, {
  start: 7976,
  length: 8,
  convRule: rule144
}, {
  start: 7984,
  length: 8,
  convRule: rule143
}, {
  start: 7992,
  length: 8,
  convRule: rule144
}, {
  start: 8e3,
  length: 6,
  convRule: rule143
}, {
  start: 8008,
  length: 6,
  convRule: rule144
}, {
  start: 8017,
  length: 1,
  convRule: rule143
}, {
  start: 8019,
  length: 1,
  convRule: rule143
}, {
  start: 8021,
  length: 1,
  convRule: rule143
}, {
  start: 8023,
  length: 1,
  convRule: rule143
}, {
  start: 8025,
  length: 1,
  convRule: rule144
}, {
  start: 8027,
  length: 1,
  convRule: rule144
}, {
  start: 8029,
  length: 1,
  convRule: rule144
}, {
  start: 8031,
  length: 1,
  convRule: rule144
}, {
  start: 8032,
  length: 8,
  convRule: rule143
}, {
  start: 8040,
  length: 8,
  convRule: rule144
}, {
  start: 8048,
  length: 2,
  convRule: rule145
}, {
  start: 8050,
  length: 4,
  convRule: rule146
}, {
  start: 8054,
  length: 2,
  convRule: rule147
}, {
  start: 8056,
  length: 2,
  convRule: rule148
}, {
  start: 8058,
  length: 2,
  convRule: rule149
}, {
  start: 8060,
  length: 2,
  convRule: rule150
}, {
  start: 8064,
  length: 8,
  convRule: rule143
}, {
  start: 8072,
  length: 8,
  convRule: rule151
}, {
  start: 8080,
  length: 8,
  convRule: rule143
}, {
  start: 8088,
  length: 8,
  convRule: rule151
}, {
  start: 8096,
  length: 8,
  convRule: rule143
}, {
  start: 8104,
  length: 8,
  convRule: rule151
}, {
  start: 8112,
  length: 2,
  convRule: rule143
}, {
  start: 8115,
  length: 1,
  convRule: rule152
}, {
  start: 8120,
  length: 2,
  convRule: rule144
}, {
  start: 8122,
  length: 2,
  convRule: rule153
}, {
  start: 8124,
  length: 1,
  convRule: rule154
}, {
  start: 8126,
  length: 1,
  convRule: rule155
}, {
  start: 8131,
  length: 1,
  convRule: rule152
}, {
  start: 8136,
  length: 4,
  convRule: rule156
}, {
  start: 8140,
  length: 1,
  convRule: rule154
}, {
  start: 8144,
  length: 2,
  convRule: rule143
}, {
  start: 8152,
  length: 2,
  convRule: rule144
}, {
  start: 8154,
  length: 2,
  convRule: rule157
}, {
  start: 8160,
  length: 2,
  convRule: rule143
}, {
  start: 8165,
  length: 1,
  convRule: rule113
}, {
  start: 8168,
  length: 2,
  convRule: rule144
}, {
  start: 8170,
  length: 2,
  convRule: rule158
}, {
  start: 8172,
  length: 1,
  convRule: rule117
}, {
  start: 8179,
  length: 1,
  convRule: rule152
}, {
  start: 8184,
  length: 2,
  convRule: rule159
}, {
  start: 8186,
  length: 2,
  convRule: rule160
}, {
  start: 8188,
  length: 1,
  convRule: rule154
}, {
  start: 8486,
  length: 1,
  convRule: rule163
}, {
  start: 8490,
  length: 1,
  convRule: rule164
}, {
  start: 8491,
  length: 1,
  convRule: rule165
}, {
  start: 8498,
  length: 1,
  convRule: rule166
}, {
  start: 8526,
  length: 1,
  convRule: rule167
}, {
  start: 8544,
  length: 16,
  convRule: rule168
}, {
  start: 8560,
  length: 16,
  convRule: rule169
}, {
  start: 8579,
  length: 1,
  convRule: rule22
}, {
  start: 8580,
  length: 1,
  convRule: rule23
}, {
  start: 9398,
  length: 26,
  convRule: rule170
}, {
  start: 9424,
  length: 26,
  convRule: rule171
}, {
  start: 11264,
  length: 47,
  convRule: rule122
}, {
  start: 11312,
  length: 47,
  convRule: rule123
}, {
  start: 11360,
  length: 1,
  convRule: rule22
}, {
  start: 11361,
  length: 1,
  convRule: rule23
}, {
  start: 11362,
  length: 1,
  convRule: rule172
}, {
  start: 11363,
  length: 1,
  convRule: rule173
}, {
  start: 11364,
  length: 1,
  convRule: rule174
}, {
  start: 11365,
  length: 1,
  convRule: rule175
}, {
  start: 11366,
  length: 1,
  convRule: rule176
}, {
  start: 11367,
  length: 1,
  convRule: rule22
}, {
  start: 11368,
  length: 1,
  convRule: rule23
}, {
  start: 11369,
  length: 1,
  convRule: rule22
}, {
  start: 11370,
  length: 1,
  convRule: rule23
}, {
  start: 11371,
  length: 1,
  convRule: rule22
}, {
  start: 11372,
  length: 1,
  convRule: rule23
}, {
  start: 11373,
  length: 1,
  convRule: rule177
}, {
  start: 11374,
  length: 1,
  convRule: rule178
}, {
  start: 11375,
  length: 1,
  convRule: rule179
}, {
  start: 11376,
  length: 1,
  convRule: rule180
}, {
  start: 11378,
  length: 1,
  convRule: rule22
}, {
  start: 11379,
  length: 1,
  convRule: rule23
}, {
  start: 11381,
  length: 1,
  convRule: rule22
}, {
  start: 11382,
  length: 1,
  convRule: rule23
}, {
  start: 11390,
  length: 2,
  convRule: rule181
}, {
  start: 11392,
  length: 1,
  convRule: rule22
}, {
  start: 11393,
  length: 1,
  convRule: rule23
}, {
  start: 11394,
  length: 1,
  convRule: rule22
}, {
  start: 11395,
  length: 1,
  convRule: rule23
}, {
  start: 11396,
  length: 1,
  convRule: rule22
}, {
  start: 11397,
  length: 1,
  convRule: rule23
}, {
  start: 11398,
  length: 1,
  convRule: rule22
}, {
  start: 11399,
  length: 1,
  convRule: rule23
}, {
  start: 11400,
  length: 1,
  convRule: rule22
}, {
  start: 11401,
  length: 1,
  convRule: rule23
}, {
  start: 11402,
  length: 1,
  convRule: rule22
}, {
  start: 11403,
  length: 1,
  convRule: rule23
}, {
  start: 11404,
  length: 1,
  convRule: rule22
}, {
  start: 11405,
  length: 1,
  convRule: rule23
}, {
  start: 11406,
  length: 1,
  convRule: rule22
}, {
  start: 11407,
  length: 1,
  convRule: rule23
}, {
  start: 11408,
  length: 1,
  convRule: rule22
}, {
  start: 11409,
  length: 1,
  convRule: rule23
}, {
  start: 11410,
  length: 1,
  convRule: rule22
}, {
  start: 11411,
  length: 1,
  convRule: rule23
}, {
  start: 11412,
  length: 1,
  convRule: rule22
}, {
  start: 11413,
  length: 1,
  convRule: rule23
}, {
  start: 11414,
  length: 1,
  convRule: rule22
}, {
  start: 11415,
  length: 1,
  convRule: rule23
}, {
  start: 11416,
  length: 1,
  convRule: rule22
}, {
  start: 11417,
  length: 1,
  convRule: rule23
}, {
  start: 11418,
  length: 1,
  convRule: rule22
}, {
  start: 11419,
  length: 1,
  convRule: rule23
}, {
  start: 11420,
  length: 1,
  convRule: rule22
}, {
  start: 11421,
  length: 1,
  convRule: rule23
}, {
  start: 11422,
  length: 1,
  convRule: rule22
}, {
  start: 11423,
  length: 1,
  convRule: rule23
}, {
  start: 11424,
  length: 1,
  convRule: rule22
}, {
  start: 11425,
  length: 1,
  convRule: rule23
}, {
  start: 11426,
  length: 1,
  convRule: rule22
}, {
  start: 11427,
  length: 1,
  convRule: rule23
}, {
  start: 11428,
  length: 1,
  convRule: rule22
}, {
  start: 11429,
  length: 1,
  convRule: rule23
}, {
  start: 11430,
  length: 1,
  convRule: rule22
}, {
  start: 11431,
  length: 1,
  convRule: rule23
}, {
  start: 11432,
  length: 1,
  convRule: rule22
}, {
  start: 11433,
  length: 1,
  convRule: rule23
}, {
  start: 11434,
  length: 1,
  convRule: rule22
}, {
  start: 11435,
  length: 1,
  convRule: rule23
}, {
  start: 11436,
  length: 1,
  convRule: rule22
}, {
  start: 11437,
  length: 1,
  convRule: rule23
}, {
  start: 11438,
  length: 1,
  convRule: rule22
}, {
  start: 11439,
  length: 1,
  convRule: rule23
}, {
  start: 11440,
  length: 1,
  convRule: rule22
}, {
  start: 11441,
  length: 1,
  convRule: rule23
}, {
  start: 11442,
  length: 1,
  convRule: rule22
}, {
  start: 11443,
  length: 1,
  convRule: rule23
}, {
  start: 11444,
  length: 1,
  convRule: rule22
}, {
  start: 11445,
  length: 1,
  convRule: rule23
}, {
  start: 11446,
  length: 1,
  convRule: rule22
}, {
  start: 11447,
  length: 1,
  convRule: rule23
}, {
  start: 11448,
  length: 1,
  convRule: rule22
}, {
  start: 11449,
  length: 1,
  convRule: rule23
}, {
  start: 11450,
  length: 1,
  convRule: rule22
}, {
  start: 11451,
  length: 1,
  convRule: rule23
}, {
  start: 11452,
  length: 1,
  convRule: rule22
}, {
  start: 11453,
  length: 1,
  convRule: rule23
}, {
  start: 11454,
  length: 1,
  convRule: rule22
}, {
  start: 11455,
  length: 1,
  convRule: rule23
}, {
  start: 11456,
  length: 1,
  convRule: rule22
}, {
  start: 11457,
  length: 1,
  convRule: rule23
}, {
  start: 11458,
  length: 1,
  convRule: rule22
}, {
  start: 11459,
  length: 1,
  convRule: rule23
}, {
  start: 11460,
  length: 1,
  convRule: rule22
}, {
  start: 11461,
  length: 1,
  convRule: rule23
}, {
  start: 11462,
  length: 1,
  convRule: rule22
}, {
  start: 11463,
  length: 1,
  convRule: rule23
}, {
  start: 11464,
  length: 1,
  convRule: rule22
}, {
  start: 11465,
  length: 1,
  convRule: rule23
}, {
  start: 11466,
  length: 1,
  convRule: rule22
}, {
  start: 11467,
  length: 1,
  convRule: rule23
}, {
  start: 11468,
  length: 1,
  convRule: rule22
}, {
  start: 11469,
  length: 1,
  convRule: rule23
}, {
  start: 11470,
  length: 1,
  convRule: rule22
}, {
  start: 11471,
  length: 1,
  convRule: rule23
}, {
  start: 11472,
  length: 1,
  convRule: rule22
}, {
  start: 11473,
  length: 1,
  convRule: rule23
}, {
  start: 11474,
  length: 1,
  convRule: rule22
}, {
  start: 11475,
  length: 1,
  convRule: rule23
}, {
  start: 11476,
  length: 1,
  convRule: rule22
}, {
  start: 11477,
  length: 1,
  convRule: rule23
}, {
  start: 11478,
  length: 1,
  convRule: rule22
}, {
  start: 11479,
  length: 1,
  convRule: rule23
}, {
  start: 11480,
  length: 1,
  convRule: rule22
}, {
  start: 11481,
  length: 1,
  convRule: rule23
}, {
  start: 11482,
  length: 1,
  convRule: rule22
}, {
  start: 11483,
  length: 1,
  convRule: rule23
}, {
  start: 11484,
  length: 1,
  convRule: rule22
}, {
  start: 11485,
  length: 1,
  convRule: rule23
}, {
  start: 11486,
  length: 1,
  convRule: rule22
}, {
  start: 11487,
  length: 1,
  convRule: rule23
}, {
  start: 11488,
  length: 1,
  convRule: rule22
}, {
  start: 11489,
  length: 1,
  convRule: rule23
}, {
  start: 11490,
  length: 1,
  convRule: rule22
}, {
  start: 11491,
  length: 1,
  convRule: rule23
}, {
  start: 11499,
  length: 1,
  convRule: rule22
}, {
  start: 11500,
  length: 1,
  convRule: rule23
}, {
  start: 11501,
  length: 1,
  convRule: rule22
}, {
  start: 11502,
  length: 1,
  convRule: rule23
}, {
  start: 11506,
  length: 1,
  convRule: rule22
}, {
  start: 11507,
  length: 1,
  convRule: rule23
}, {
  start: 11520,
  length: 38,
  convRule: rule182
}, {
  start: 11559,
  length: 1,
  convRule: rule182
}, {
  start: 11565,
  length: 1,
  convRule: rule182
}, {
  start: 42560,
  length: 1,
  convRule: rule22
}, {
  start: 42561,
  length: 1,
  convRule: rule23
}, {
  start: 42562,
  length: 1,
  convRule: rule22
}, {
  start: 42563,
  length: 1,
  convRule: rule23
}, {
  start: 42564,
  length: 1,
  convRule: rule22
}, {
  start: 42565,
  length: 1,
  convRule: rule23
}, {
  start: 42566,
  length: 1,
  convRule: rule22
}, {
  start: 42567,
  length: 1,
  convRule: rule23
}, {
  start: 42568,
  length: 1,
  convRule: rule22
}, {
  start: 42569,
  length: 1,
  convRule: rule23
}, {
  start: 42570,
  length: 1,
  convRule: rule22
}, {
  start: 42571,
  length: 1,
  convRule: rule23
}, {
  start: 42572,
  length: 1,
  convRule: rule22
}, {
  start: 42573,
  length: 1,
  convRule: rule23
}, {
  start: 42574,
  length: 1,
  convRule: rule22
}, {
  start: 42575,
  length: 1,
  convRule: rule23
}, {
  start: 42576,
  length: 1,
  convRule: rule22
}, {
  start: 42577,
  length: 1,
  convRule: rule23
}, {
  start: 42578,
  length: 1,
  convRule: rule22
}, {
  start: 42579,
  length: 1,
  convRule: rule23
}, {
  start: 42580,
  length: 1,
  convRule: rule22
}, {
  start: 42581,
  length: 1,
  convRule: rule23
}, {
  start: 42582,
  length: 1,
  convRule: rule22
}, {
  start: 42583,
  length: 1,
  convRule: rule23
}, {
  start: 42584,
  length: 1,
  convRule: rule22
}, {
  start: 42585,
  length: 1,
  convRule: rule23
}, {
  start: 42586,
  length: 1,
  convRule: rule22
}, {
  start: 42587,
  length: 1,
  convRule: rule23
}, {
  start: 42588,
  length: 1,
  convRule: rule22
}, {
  start: 42589,
  length: 1,
  convRule: rule23
}, {
  start: 42590,
  length: 1,
  convRule: rule22
}, {
  start: 42591,
  length: 1,
  convRule: rule23
}, {
  start: 42592,
  length: 1,
  convRule: rule22
}, {
  start: 42593,
  length: 1,
  convRule: rule23
}, {
  start: 42594,
  length: 1,
  convRule: rule22
}, {
  start: 42595,
  length: 1,
  convRule: rule23
}, {
  start: 42596,
  length: 1,
  convRule: rule22
}, {
  start: 42597,
  length: 1,
  convRule: rule23
}, {
  start: 42598,
  length: 1,
  convRule: rule22
}, {
  start: 42599,
  length: 1,
  convRule: rule23
}, {
  start: 42600,
  length: 1,
  convRule: rule22
}, {
  start: 42601,
  length: 1,
  convRule: rule23
}, {
  start: 42602,
  length: 1,
  convRule: rule22
}, {
  start: 42603,
  length: 1,
  convRule: rule23
}, {
  start: 42604,
  length: 1,
  convRule: rule22
}, {
  start: 42605,
  length: 1,
  convRule: rule23
}, {
  start: 42624,
  length: 1,
  convRule: rule22
}, {
  start: 42625,
  length: 1,
  convRule: rule23
}, {
  start: 42626,
  length: 1,
  convRule: rule22
}, {
  start: 42627,
  length: 1,
  convRule: rule23
}, {
  start: 42628,
  length: 1,
  convRule: rule22
}, {
  start: 42629,
  length: 1,
  convRule: rule23
}, {
  start: 42630,
  length: 1,
  convRule: rule22
}, {
  start: 42631,
  length: 1,
  convRule: rule23
}, {
  start: 42632,
  length: 1,
  convRule: rule22
}, {
  start: 42633,
  length: 1,
  convRule: rule23
}, {
  start: 42634,
  length: 1,
  convRule: rule22
}, {
  start: 42635,
  length: 1,
  convRule: rule23
}, {
  start: 42636,
  length: 1,
  convRule: rule22
}, {
  start: 42637,
  length: 1,
  convRule: rule23
}, {
  start: 42638,
  length: 1,
  convRule: rule22
}, {
  start: 42639,
  length: 1,
  convRule: rule23
}, {
  start: 42640,
  length: 1,
  convRule: rule22
}, {
  start: 42641,
  length: 1,
  convRule: rule23
}, {
  start: 42642,
  length: 1,
  convRule: rule22
}, {
  start: 42643,
  length: 1,
  convRule: rule23
}, {
  start: 42644,
  length: 1,
  convRule: rule22
}, {
  start: 42645,
  length: 1,
  convRule: rule23
}, {
  start: 42646,
  length: 1,
  convRule: rule22
}, {
  start: 42647,
  length: 1,
  convRule: rule23
}, {
  start: 42648,
  length: 1,
  convRule: rule22
}, {
  start: 42649,
  length: 1,
  convRule: rule23
}, {
  start: 42650,
  length: 1,
  convRule: rule22
}, {
  start: 42651,
  length: 1,
  convRule: rule23
}, {
  start: 42786,
  length: 1,
  convRule: rule22
}, {
  start: 42787,
  length: 1,
  convRule: rule23
}, {
  start: 42788,
  length: 1,
  convRule: rule22
}, {
  start: 42789,
  length: 1,
  convRule: rule23
}, {
  start: 42790,
  length: 1,
  convRule: rule22
}, {
  start: 42791,
  length: 1,
  convRule: rule23
}, {
  start: 42792,
  length: 1,
  convRule: rule22
}, {
  start: 42793,
  length: 1,
  convRule: rule23
}, {
  start: 42794,
  length: 1,
  convRule: rule22
}, {
  start: 42795,
  length: 1,
  convRule: rule23
}, {
  start: 42796,
  length: 1,
  convRule: rule22
}, {
  start: 42797,
  length: 1,
  convRule: rule23
}, {
  start: 42798,
  length: 1,
  convRule: rule22
}, {
  start: 42799,
  length: 1,
  convRule: rule23
}, {
  start: 42802,
  length: 1,
  convRule: rule22
}, {
  start: 42803,
  length: 1,
  convRule: rule23
}, {
  start: 42804,
  length: 1,
  convRule: rule22
}, {
  start: 42805,
  length: 1,
  convRule: rule23
}, {
  start: 42806,
  length: 1,
  convRule: rule22
}, {
  start: 42807,
  length: 1,
  convRule: rule23
}, {
  start: 42808,
  length: 1,
  convRule: rule22
}, {
  start: 42809,
  length: 1,
  convRule: rule23
}, {
  start: 42810,
  length: 1,
  convRule: rule22
}, {
  start: 42811,
  length: 1,
  convRule: rule23
}, {
  start: 42812,
  length: 1,
  convRule: rule22
}, {
  start: 42813,
  length: 1,
  convRule: rule23
}, {
  start: 42814,
  length: 1,
  convRule: rule22
}, {
  start: 42815,
  length: 1,
  convRule: rule23
}, {
  start: 42816,
  length: 1,
  convRule: rule22
}, {
  start: 42817,
  length: 1,
  convRule: rule23
}, {
  start: 42818,
  length: 1,
  convRule: rule22
}, {
  start: 42819,
  length: 1,
  convRule: rule23
}, {
  start: 42820,
  length: 1,
  convRule: rule22
}, {
  start: 42821,
  length: 1,
  convRule: rule23
}, {
  start: 42822,
  length: 1,
  convRule: rule22
}, {
  start: 42823,
  length: 1,
  convRule: rule23
}, {
  start: 42824,
  length: 1,
  convRule: rule22
}, {
  start: 42825,
  length: 1,
  convRule: rule23
}, {
  start: 42826,
  length: 1,
  convRule: rule22
}, {
  start: 42827,
  length: 1,
  convRule: rule23
}, {
  start: 42828,
  length: 1,
  convRule: rule22
}, {
  start: 42829,
  length: 1,
  convRule: rule23
}, {
  start: 42830,
  length: 1,
  convRule: rule22
}, {
  start: 42831,
  length: 1,
  convRule: rule23
}, {
  start: 42832,
  length: 1,
  convRule: rule22
}, {
  start: 42833,
  length: 1,
  convRule: rule23
}, {
  start: 42834,
  length: 1,
  convRule: rule22
}, {
  start: 42835,
  length: 1,
  convRule: rule23
}, {
  start: 42836,
  length: 1,
  convRule: rule22
}, {
  start: 42837,
  length: 1,
  convRule: rule23
}, {
  start: 42838,
  length: 1,
  convRule: rule22
}, {
  start: 42839,
  length: 1,
  convRule: rule23
}, {
  start: 42840,
  length: 1,
  convRule: rule22
}, {
  start: 42841,
  length: 1,
  convRule: rule23
}, {
  start: 42842,
  length: 1,
  convRule: rule22
}, {
  start: 42843,
  length: 1,
  convRule: rule23
}, {
  start: 42844,
  length: 1,
  convRule: rule22
}, {
  start: 42845,
  length: 1,
  convRule: rule23
}, {
  start: 42846,
  length: 1,
  convRule: rule22
}, {
  start: 42847,
  length: 1,
  convRule: rule23
}, {
  start: 42848,
  length: 1,
  convRule: rule22
}, {
  start: 42849,
  length: 1,
  convRule: rule23
}, {
  start: 42850,
  length: 1,
  convRule: rule22
}, {
  start: 42851,
  length: 1,
  convRule: rule23
}, {
  start: 42852,
  length: 1,
  convRule: rule22
}, {
  start: 42853,
  length: 1,
  convRule: rule23
}, {
  start: 42854,
  length: 1,
  convRule: rule22
}, {
  start: 42855,
  length: 1,
  convRule: rule23
}, {
  start: 42856,
  length: 1,
  convRule: rule22
}, {
  start: 42857,
  length: 1,
  convRule: rule23
}, {
  start: 42858,
  length: 1,
  convRule: rule22
}, {
  start: 42859,
  length: 1,
  convRule: rule23
}, {
  start: 42860,
  length: 1,
  convRule: rule22
}, {
  start: 42861,
  length: 1,
  convRule: rule23
}, {
  start: 42862,
  length: 1,
  convRule: rule22
}, {
  start: 42863,
  length: 1,
  convRule: rule23
}, {
  start: 42873,
  length: 1,
  convRule: rule22
}, {
  start: 42874,
  length: 1,
  convRule: rule23
}, {
  start: 42875,
  length: 1,
  convRule: rule22
}, {
  start: 42876,
  length: 1,
  convRule: rule23
}, {
  start: 42877,
  length: 1,
  convRule: rule183
}, {
  start: 42878,
  length: 1,
  convRule: rule22
}, {
  start: 42879,
  length: 1,
  convRule: rule23
}, {
  start: 42880,
  length: 1,
  convRule: rule22
}, {
  start: 42881,
  length: 1,
  convRule: rule23
}, {
  start: 42882,
  length: 1,
  convRule: rule22
}, {
  start: 42883,
  length: 1,
  convRule: rule23
}, {
  start: 42884,
  length: 1,
  convRule: rule22
}, {
  start: 42885,
  length: 1,
  convRule: rule23
}, {
  start: 42886,
  length: 1,
  convRule: rule22
}, {
  start: 42887,
  length: 1,
  convRule: rule23
}, {
  start: 42891,
  length: 1,
  convRule: rule22
}, {
  start: 42892,
  length: 1,
  convRule: rule23
}, {
  start: 42893,
  length: 1,
  convRule: rule184
}, {
  start: 42896,
  length: 1,
  convRule: rule22
}, {
  start: 42897,
  length: 1,
  convRule: rule23
}, {
  start: 42898,
  length: 1,
  convRule: rule22
}, {
  start: 42899,
  length: 1,
  convRule: rule23
}, {
  start: 42900,
  length: 1,
  convRule: rule185
}, {
  start: 42902,
  length: 1,
  convRule: rule22
}, {
  start: 42903,
  length: 1,
  convRule: rule23
}, {
  start: 42904,
  length: 1,
  convRule: rule22
}, {
  start: 42905,
  length: 1,
  convRule: rule23
}, {
  start: 42906,
  length: 1,
  convRule: rule22
}, {
  start: 42907,
  length: 1,
  convRule: rule23
}, {
  start: 42908,
  length: 1,
  convRule: rule22
}, {
  start: 42909,
  length: 1,
  convRule: rule23
}, {
  start: 42910,
  length: 1,
  convRule: rule22
}, {
  start: 42911,
  length: 1,
  convRule: rule23
}, {
  start: 42912,
  length: 1,
  convRule: rule22
}, {
  start: 42913,
  length: 1,
  convRule: rule23
}, {
  start: 42914,
  length: 1,
  convRule: rule22
}, {
  start: 42915,
  length: 1,
  convRule: rule23
}, {
  start: 42916,
  length: 1,
  convRule: rule22
}, {
  start: 42917,
  length: 1,
  convRule: rule23
}, {
  start: 42918,
  length: 1,
  convRule: rule22
}, {
  start: 42919,
  length: 1,
  convRule: rule23
}, {
  start: 42920,
  length: 1,
  convRule: rule22
}, {
  start: 42921,
  length: 1,
  convRule: rule23
}, {
  start: 42922,
  length: 1,
  convRule: rule186
}, {
  start: 42923,
  length: 1,
  convRule: rule187
}, {
  start: 42924,
  length: 1,
  convRule: rule188
}, {
  start: 42925,
  length: 1,
  convRule: rule189
}, {
  start: 42926,
  length: 1,
  convRule: rule186
}, {
  start: 42928,
  length: 1,
  convRule: rule190
}, {
  start: 42929,
  length: 1,
  convRule: rule191
}, {
  start: 42930,
  length: 1,
  convRule: rule192
}, {
  start: 42931,
  length: 1,
  convRule: rule193
}, {
  start: 42932,
  length: 1,
  convRule: rule22
}, {
  start: 42933,
  length: 1,
  convRule: rule23
}, {
  start: 42934,
  length: 1,
  convRule: rule22
}, {
  start: 42935,
  length: 1,
  convRule: rule23
}, {
  start: 42936,
  length: 1,
  convRule: rule22
}, {
  start: 42937,
  length: 1,
  convRule: rule23
}, {
  start: 42938,
  length: 1,
  convRule: rule22
}, {
  start: 42939,
  length: 1,
  convRule: rule23
}, {
  start: 42940,
  length: 1,
  convRule: rule22
}, {
  start: 42941,
  length: 1,
  convRule: rule23
}, {
  start: 42942,
  length: 1,
  convRule: rule22
}, {
  start: 42943,
  length: 1,
  convRule: rule23
}, {
  start: 42946,
  length: 1,
  convRule: rule22
}, {
  start: 42947,
  length: 1,
  convRule: rule23
}, {
  start: 42948,
  length: 1,
  convRule: rule194
}, {
  start: 42949,
  length: 1,
  convRule: rule195
}, {
  start: 42950,
  length: 1,
  convRule: rule196
}, {
  start: 42951,
  length: 1,
  convRule: rule22
}, {
  start: 42952,
  length: 1,
  convRule: rule23
}, {
  start: 42953,
  length: 1,
  convRule: rule22
}, {
  start: 42954,
  length: 1,
  convRule: rule23
}, {
  start: 42997,
  length: 1,
  convRule: rule22
}, {
  start: 42998,
  length: 1,
  convRule: rule23
}, {
  start: 43859,
  length: 1,
  convRule: rule197
}, {
  start: 43888,
  length: 80,
  convRule: rule198
}, {
  start: 65313,
  length: 26,
  convRule: rule9
}, {
  start: 65345,
  length: 26,
  convRule: rule12
}, {
  start: 66560,
  length: 40,
  convRule: rule201
}, {
  start: 66600,
  length: 40,
  convRule: rule202
}, {
  start: 66736,
  length: 36,
  convRule: rule201
}, {
  start: 66776,
  length: 36,
  convRule: rule202
}, {
  start: 68736,
  length: 51,
  convRule: rule97
}, {
  start: 68800,
  length: 51,
  convRule: rule102
}, {
  start: 71840,
  length: 32,
  convRule: rule9
}, {
  start: 71872,
  length: 32,
  convRule: rule12
}, {
  start: 93760,
  length: 32,
  convRule: rule9
}, {
  start: 93792,
  length: 32,
  convRule: rule12
}, {
  start: 125184,
  length: 34,
  convRule: rule203
}, {
  start: 125218,
  length: 34,
  convRule: rule204
}];
var bsearch = function(a) {
  return function(array) {
    return function(size2) {
      return function(compare4) {
        var go = function($copy_i) {
          return function($copy_k) {
            var $tco_var_i = $copy_i;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(i, k) {
              if (i > k || i >= length(array)) {
                $tco_done = true;
                return Nothing.value;
              }
              ;
              if (otherwise) {
                var j = floor2(toNumber(i + k | 0) / 2);
                var b = unsafeIndex2(array)(j);
                var v = compare4(a)(b);
                if (v instanceof EQ) {
                  $tco_done = true;
                  return new Just(b);
                }
                ;
                if (v instanceof GT) {
                  $tco_var_i = j + 1 | 0;
                  $copy_k = k;
                  return;
                }
                ;
                $tco_var_i = i;
                $copy_k = j - 1 | 0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i.constructor.name, k.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_i, $copy_k);
            }
            ;
            return $tco_result;
          };
        };
        return go(0)(size2);
      };
    };
  };
};
var blkCmp = function(v) {
  return function(v1) {
    if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
      return EQ.value;
    }
    ;
    if (v.start > v1.start) {
      return GT.value;
    }
    ;
    if (otherwise) {
      return LT.value;
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
  };
};
var getRule = function(blocks) {
  return function(unichar) {
    return function(size2) {
      var key = {
        start: unichar,
        length: 1,
        convRule: nullrule
      };
      var maybeCharBlock = bsearch(key)(blocks)(size2)(blkCmp);
      if (maybeCharBlock instanceof Nothing) {
        return Nothing.value;
      }
      ;
      if (maybeCharBlock instanceof Just) {
        return new Just(maybeCharBlock.value0.convRule);
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
    };
  };
};
var caseConv = function(f) {
  return function($$char2) {
    var maybeConversionRule = getRule(convchars)($$char2)(numConvBlocks);
    if (maybeConversionRule instanceof Nothing) {
      return $$char2;
    }
    ;
    if (maybeConversionRule instanceof Just) {
      return $$char2 + f(maybeConversionRule.value0) | 0;
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5727, column 5 - line 5729, column 53): " + [maybeConversionRule.constructor.name]);
  };
};
var uTowlower = /* @__PURE__ */ caseConv(function(v) {
  return v.lowdist;
});
var uTowupper = /* @__PURE__ */ caseConv(function(v) {
  return v.updist;
});
var checkAttrS = function(categories) {
  return function($$char2) {
    var maybeConversionRule = getRule(spacechars)($$char2)(numSpaceBlocks);
    if (maybeConversionRule instanceof Nothing) {
      return false;
    }
    ;
    if (maybeConversionRule instanceof Just) {
      return isJust(elemIndex3(maybeConversionRule.value0.category)(categories));
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5654, column 5 - line 5656, column 86): " + [maybeConversionRule.constructor.name]);
  };
};
var uIswspace = /* @__PURE__ */ checkAttrS([gencatZS]);
var allchars = [{
  start: 0,
  length: 32,
  convRule: rule0
}, {
  start: 32,
  length: 1,
  convRule: rule1
}, {
  start: 33,
  length: 3,
  convRule: rule2
}, {
  start: 36,
  length: 1,
  convRule: rule3
}, {
  start: 37,
  length: 3,
  convRule: rule2
}, {
  start: 40,
  length: 1,
  convRule: rule4
}, {
  start: 41,
  length: 1,
  convRule: rule5
}, {
  start: 42,
  length: 1,
  convRule: rule2
}, {
  start: 43,
  length: 1,
  convRule: rule6
}, {
  start: 44,
  length: 1,
  convRule: rule2
}, {
  start: 45,
  length: 1,
  convRule: rule7
}, {
  start: 46,
  length: 2,
  convRule: rule2
}, {
  start: 48,
  length: 10,
  convRule: rule8
}, {
  start: 58,
  length: 2,
  convRule: rule2
}, {
  start: 60,
  length: 3,
  convRule: rule6
}, {
  start: 63,
  length: 2,
  convRule: rule2
}, {
  start: 65,
  length: 26,
  convRule: rule9
}, {
  start: 91,
  length: 1,
  convRule: rule4
}, {
  start: 92,
  length: 1,
  convRule: rule2
}, {
  start: 93,
  length: 1,
  convRule: rule5
}, {
  start: 94,
  length: 1,
  convRule: rule10
}, {
  start: 95,
  length: 1,
  convRule: rule11
}, {
  start: 96,
  length: 1,
  convRule: rule10
}, {
  start: 97,
  length: 26,
  convRule: rule12
}, {
  start: 123,
  length: 1,
  convRule: rule4
}, {
  start: 124,
  length: 1,
  convRule: rule6
}, {
  start: 125,
  length: 1,
  convRule: rule5
}, {
  start: 126,
  length: 1,
  convRule: rule6
}, {
  start: 127,
  length: 33,
  convRule: rule0
}, {
  start: 160,
  length: 1,
  convRule: rule1
}, {
  start: 161,
  length: 1,
  convRule: rule2
}, {
  start: 162,
  length: 4,
  convRule: rule3
}, {
  start: 166,
  length: 1,
  convRule: rule13
}, {
  start: 167,
  length: 1,
  convRule: rule2
}, {
  start: 168,
  length: 1,
  convRule: rule10
}, {
  start: 169,
  length: 1,
  convRule: rule13
}, {
  start: 170,
  length: 1,
  convRule: rule14
}, {
  start: 171,
  length: 1,
  convRule: rule15
}, {
  start: 172,
  length: 1,
  convRule: rule6
}, {
  start: 173,
  length: 1,
  convRule: rule16
}, {
  start: 174,
  length: 1,
  convRule: rule13
}, {
  start: 175,
  length: 1,
  convRule: rule10
}, {
  start: 176,
  length: 1,
  convRule: rule13
}, {
  start: 177,
  length: 1,
  convRule: rule6
}, {
  start: 178,
  length: 2,
  convRule: rule17
}, {
  start: 180,
  length: 1,
  convRule: rule10
}, {
  start: 181,
  length: 1,
  convRule: rule18
}, {
  start: 182,
  length: 2,
  convRule: rule2
}, {
  start: 184,
  length: 1,
  convRule: rule10
}, {
  start: 185,
  length: 1,
  convRule: rule17
}, {
  start: 186,
  length: 1,
  convRule: rule14
}, {
  start: 187,
  length: 1,
  convRule: rule19
}, {
  start: 188,
  length: 3,
  convRule: rule17
}, {
  start: 191,
  length: 1,
  convRule: rule2
}, {
  start: 192,
  length: 23,
  convRule: rule9
}, {
  start: 215,
  length: 1,
  convRule: rule6
}, {
  start: 216,
  length: 7,
  convRule: rule9
}, {
  start: 223,
  length: 1,
  convRule: rule20
}, {
  start: 224,
  length: 23,
  convRule: rule12
}, {
  start: 247,
  length: 1,
  convRule: rule6
}, {
  start: 248,
  length: 7,
  convRule: rule12
}, {
  start: 255,
  length: 1,
  convRule: rule21
}, {
  start: 256,
  length: 1,
  convRule: rule22
}, {
  start: 257,
  length: 1,
  convRule: rule23
}, {
  start: 258,
  length: 1,
  convRule: rule22
}, {
  start: 259,
  length: 1,
  convRule: rule23
}, {
  start: 260,
  length: 1,
  convRule: rule22
}, {
  start: 261,
  length: 1,
  convRule: rule23
}, {
  start: 262,
  length: 1,
  convRule: rule22
}, {
  start: 263,
  length: 1,
  convRule: rule23
}, {
  start: 264,
  length: 1,
  convRule: rule22
}, {
  start: 265,
  length: 1,
  convRule: rule23
}, {
  start: 266,
  length: 1,
  convRule: rule22
}, {
  start: 267,
  length: 1,
  convRule: rule23
}, {
  start: 268,
  length: 1,
  convRule: rule22
}, {
  start: 269,
  length: 1,
  convRule: rule23
}, {
  start: 270,
  length: 1,
  convRule: rule22
}, {
  start: 271,
  length: 1,
  convRule: rule23
}, {
  start: 272,
  length: 1,
  convRule: rule22
}, {
  start: 273,
  length: 1,
  convRule: rule23
}, {
  start: 274,
  length: 1,
  convRule: rule22
}, {
  start: 275,
  length: 1,
  convRule: rule23
}, {
  start: 276,
  length: 1,
  convRule: rule22
}, {
  start: 277,
  length: 1,
  convRule: rule23
}, {
  start: 278,
  length: 1,
  convRule: rule22
}, {
  start: 279,
  length: 1,
  convRule: rule23
}, {
  start: 280,
  length: 1,
  convRule: rule22
}, {
  start: 281,
  length: 1,
  convRule: rule23
}, {
  start: 282,
  length: 1,
  convRule: rule22
}, {
  start: 283,
  length: 1,
  convRule: rule23
}, {
  start: 284,
  length: 1,
  convRule: rule22
}, {
  start: 285,
  length: 1,
  convRule: rule23
}, {
  start: 286,
  length: 1,
  convRule: rule22
}, {
  start: 287,
  length: 1,
  convRule: rule23
}, {
  start: 288,
  length: 1,
  convRule: rule22
}, {
  start: 289,
  length: 1,
  convRule: rule23
}, {
  start: 290,
  length: 1,
  convRule: rule22
}, {
  start: 291,
  length: 1,
  convRule: rule23
}, {
  start: 292,
  length: 1,
  convRule: rule22
}, {
  start: 293,
  length: 1,
  convRule: rule23
}, {
  start: 294,
  length: 1,
  convRule: rule22
}, {
  start: 295,
  length: 1,
  convRule: rule23
}, {
  start: 296,
  length: 1,
  convRule: rule22
}, {
  start: 297,
  length: 1,
  convRule: rule23
}, {
  start: 298,
  length: 1,
  convRule: rule22
}, {
  start: 299,
  length: 1,
  convRule: rule23
}, {
  start: 300,
  length: 1,
  convRule: rule22
}, {
  start: 301,
  length: 1,
  convRule: rule23
}, {
  start: 302,
  length: 1,
  convRule: rule22
}, {
  start: 303,
  length: 1,
  convRule: rule23
}, {
  start: 304,
  length: 1,
  convRule: rule24
}, {
  start: 305,
  length: 1,
  convRule: rule25
}, {
  start: 306,
  length: 1,
  convRule: rule22
}, {
  start: 307,
  length: 1,
  convRule: rule23
}, {
  start: 308,
  length: 1,
  convRule: rule22
}, {
  start: 309,
  length: 1,
  convRule: rule23
}, {
  start: 310,
  length: 1,
  convRule: rule22
}, {
  start: 311,
  length: 1,
  convRule: rule23
}, {
  start: 312,
  length: 1,
  convRule: rule20
}, {
  start: 313,
  length: 1,
  convRule: rule22
}, {
  start: 314,
  length: 1,
  convRule: rule23
}, {
  start: 315,
  length: 1,
  convRule: rule22
}, {
  start: 316,
  length: 1,
  convRule: rule23
}, {
  start: 317,
  length: 1,
  convRule: rule22
}, {
  start: 318,
  length: 1,
  convRule: rule23
}, {
  start: 319,
  length: 1,
  convRule: rule22
}, {
  start: 320,
  length: 1,
  convRule: rule23
}, {
  start: 321,
  length: 1,
  convRule: rule22
}, {
  start: 322,
  length: 1,
  convRule: rule23
}, {
  start: 323,
  length: 1,
  convRule: rule22
}, {
  start: 324,
  length: 1,
  convRule: rule23
}, {
  start: 325,
  length: 1,
  convRule: rule22
}, {
  start: 326,
  length: 1,
  convRule: rule23
}, {
  start: 327,
  length: 1,
  convRule: rule22
}, {
  start: 328,
  length: 1,
  convRule: rule23
}, {
  start: 329,
  length: 1,
  convRule: rule20
}, {
  start: 330,
  length: 1,
  convRule: rule22
}, {
  start: 331,
  length: 1,
  convRule: rule23
}, {
  start: 332,
  length: 1,
  convRule: rule22
}, {
  start: 333,
  length: 1,
  convRule: rule23
}, {
  start: 334,
  length: 1,
  convRule: rule22
}, {
  start: 335,
  length: 1,
  convRule: rule23
}, {
  start: 336,
  length: 1,
  convRule: rule22
}, {
  start: 337,
  length: 1,
  convRule: rule23
}, {
  start: 338,
  length: 1,
  convRule: rule22
}, {
  start: 339,
  length: 1,
  convRule: rule23
}, {
  start: 340,
  length: 1,
  convRule: rule22
}, {
  start: 341,
  length: 1,
  convRule: rule23
}, {
  start: 342,
  length: 1,
  convRule: rule22
}, {
  start: 343,
  length: 1,
  convRule: rule23
}, {
  start: 344,
  length: 1,
  convRule: rule22
}, {
  start: 345,
  length: 1,
  convRule: rule23
}, {
  start: 346,
  length: 1,
  convRule: rule22
}, {
  start: 347,
  length: 1,
  convRule: rule23
}, {
  start: 348,
  length: 1,
  convRule: rule22
}, {
  start: 349,
  length: 1,
  convRule: rule23
}, {
  start: 350,
  length: 1,
  convRule: rule22
}, {
  start: 351,
  length: 1,
  convRule: rule23
}, {
  start: 352,
  length: 1,
  convRule: rule22
}, {
  start: 353,
  length: 1,
  convRule: rule23
}, {
  start: 354,
  length: 1,
  convRule: rule22
}, {
  start: 355,
  length: 1,
  convRule: rule23
}, {
  start: 356,
  length: 1,
  convRule: rule22
}, {
  start: 357,
  length: 1,
  convRule: rule23
}, {
  start: 358,
  length: 1,
  convRule: rule22
}, {
  start: 359,
  length: 1,
  convRule: rule23
}, {
  start: 360,
  length: 1,
  convRule: rule22
}, {
  start: 361,
  length: 1,
  convRule: rule23
}, {
  start: 362,
  length: 1,
  convRule: rule22
}, {
  start: 363,
  length: 1,
  convRule: rule23
}, {
  start: 364,
  length: 1,
  convRule: rule22
}, {
  start: 365,
  length: 1,
  convRule: rule23
}, {
  start: 366,
  length: 1,
  convRule: rule22
}, {
  start: 367,
  length: 1,
  convRule: rule23
}, {
  start: 368,
  length: 1,
  convRule: rule22
}, {
  start: 369,
  length: 1,
  convRule: rule23
}, {
  start: 370,
  length: 1,
  convRule: rule22
}, {
  start: 371,
  length: 1,
  convRule: rule23
}, {
  start: 372,
  length: 1,
  convRule: rule22
}, {
  start: 373,
  length: 1,
  convRule: rule23
}, {
  start: 374,
  length: 1,
  convRule: rule22
}, {
  start: 375,
  length: 1,
  convRule: rule23
}, {
  start: 376,
  length: 1,
  convRule: rule26
}, {
  start: 377,
  length: 1,
  convRule: rule22
}, {
  start: 378,
  length: 1,
  convRule: rule23
}, {
  start: 379,
  length: 1,
  convRule: rule22
}, {
  start: 380,
  length: 1,
  convRule: rule23
}, {
  start: 381,
  length: 1,
  convRule: rule22
}, {
  start: 382,
  length: 1,
  convRule: rule23
}, {
  start: 383,
  length: 1,
  convRule: rule27
}, {
  start: 384,
  length: 1,
  convRule: rule28
}, {
  start: 385,
  length: 1,
  convRule: rule29
}, {
  start: 386,
  length: 1,
  convRule: rule22
}, {
  start: 387,
  length: 1,
  convRule: rule23
}, {
  start: 388,
  length: 1,
  convRule: rule22
}, {
  start: 389,
  length: 1,
  convRule: rule23
}, {
  start: 390,
  length: 1,
  convRule: rule30
}, {
  start: 391,
  length: 1,
  convRule: rule22
}, {
  start: 392,
  length: 1,
  convRule: rule23
}, {
  start: 393,
  length: 2,
  convRule: rule31
}, {
  start: 395,
  length: 1,
  convRule: rule22
}, {
  start: 396,
  length: 1,
  convRule: rule23
}, {
  start: 397,
  length: 1,
  convRule: rule20
}, {
  start: 398,
  length: 1,
  convRule: rule32
}, {
  start: 399,
  length: 1,
  convRule: rule33
}, {
  start: 400,
  length: 1,
  convRule: rule34
}, {
  start: 401,
  length: 1,
  convRule: rule22
}, {
  start: 402,
  length: 1,
  convRule: rule23
}, {
  start: 403,
  length: 1,
  convRule: rule31
}, {
  start: 404,
  length: 1,
  convRule: rule35
}, {
  start: 405,
  length: 1,
  convRule: rule36
}, {
  start: 406,
  length: 1,
  convRule: rule37
}, {
  start: 407,
  length: 1,
  convRule: rule38
}, {
  start: 408,
  length: 1,
  convRule: rule22
}, {
  start: 409,
  length: 1,
  convRule: rule23
}, {
  start: 410,
  length: 1,
  convRule: rule39
}, {
  start: 411,
  length: 1,
  convRule: rule20
}, {
  start: 412,
  length: 1,
  convRule: rule37
}, {
  start: 413,
  length: 1,
  convRule: rule40
}, {
  start: 414,
  length: 1,
  convRule: rule41
}, {
  start: 415,
  length: 1,
  convRule: rule42
}, {
  start: 416,
  length: 1,
  convRule: rule22
}, {
  start: 417,
  length: 1,
  convRule: rule23
}, {
  start: 418,
  length: 1,
  convRule: rule22
}, {
  start: 419,
  length: 1,
  convRule: rule23
}, {
  start: 420,
  length: 1,
  convRule: rule22
}, {
  start: 421,
  length: 1,
  convRule: rule23
}, {
  start: 422,
  length: 1,
  convRule: rule43
}, {
  start: 423,
  length: 1,
  convRule: rule22
}, {
  start: 424,
  length: 1,
  convRule: rule23
}, {
  start: 425,
  length: 1,
  convRule: rule43
}, {
  start: 426,
  length: 2,
  convRule: rule20
}, {
  start: 428,
  length: 1,
  convRule: rule22
}, {
  start: 429,
  length: 1,
  convRule: rule23
}, {
  start: 430,
  length: 1,
  convRule: rule43
}, {
  start: 431,
  length: 1,
  convRule: rule22
}, {
  start: 432,
  length: 1,
  convRule: rule23
}, {
  start: 433,
  length: 2,
  convRule: rule44
}, {
  start: 435,
  length: 1,
  convRule: rule22
}, {
  start: 436,
  length: 1,
  convRule: rule23
}, {
  start: 437,
  length: 1,
  convRule: rule22
}, {
  start: 438,
  length: 1,
  convRule: rule23
}, {
  start: 439,
  length: 1,
  convRule: rule45
}, {
  start: 440,
  length: 1,
  convRule: rule22
}, {
  start: 441,
  length: 1,
  convRule: rule23
}, {
  start: 442,
  length: 1,
  convRule: rule20
}, {
  start: 443,
  length: 1,
  convRule: rule14
}, {
  start: 444,
  length: 1,
  convRule: rule22
}, {
  start: 445,
  length: 1,
  convRule: rule23
}, {
  start: 446,
  length: 1,
  convRule: rule20
}, {
  start: 447,
  length: 1,
  convRule: rule46
}, {
  start: 448,
  length: 4,
  convRule: rule14
}, {
  start: 452,
  length: 1,
  convRule: rule47
}, {
  start: 453,
  length: 1,
  convRule: rule48
}, {
  start: 454,
  length: 1,
  convRule: rule49
}, {
  start: 455,
  length: 1,
  convRule: rule47
}, {
  start: 456,
  length: 1,
  convRule: rule48
}, {
  start: 457,
  length: 1,
  convRule: rule49
}, {
  start: 458,
  length: 1,
  convRule: rule47
}, {
  start: 459,
  length: 1,
  convRule: rule48
}, {
  start: 460,
  length: 1,
  convRule: rule49
}, {
  start: 461,
  length: 1,
  convRule: rule22
}, {
  start: 462,
  length: 1,
  convRule: rule23
}, {
  start: 463,
  length: 1,
  convRule: rule22
}, {
  start: 464,
  length: 1,
  convRule: rule23
}, {
  start: 465,
  length: 1,
  convRule: rule22
}, {
  start: 466,
  length: 1,
  convRule: rule23
}, {
  start: 467,
  length: 1,
  convRule: rule22
}, {
  start: 468,
  length: 1,
  convRule: rule23
}, {
  start: 469,
  length: 1,
  convRule: rule22
}, {
  start: 470,
  length: 1,
  convRule: rule23
}, {
  start: 471,
  length: 1,
  convRule: rule22
}, {
  start: 472,
  length: 1,
  convRule: rule23
}, {
  start: 473,
  length: 1,
  convRule: rule22
}, {
  start: 474,
  length: 1,
  convRule: rule23
}, {
  start: 475,
  length: 1,
  convRule: rule22
}, {
  start: 476,
  length: 1,
  convRule: rule23
}, {
  start: 477,
  length: 1,
  convRule: rule50
}, {
  start: 478,
  length: 1,
  convRule: rule22
}, {
  start: 479,
  length: 1,
  convRule: rule23
}, {
  start: 480,
  length: 1,
  convRule: rule22
}, {
  start: 481,
  length: 1,
  convRule: rule23
}, {
  start: 482,
  length: 1,
  convRule: rule22
}, {
  start: 483,
  length: 1,
  convRule: rule23
}, {
  start: 484,
  length: 1,
  convRule: rule22
}, {
  start: 485,
  length: 1,
  convRule: rule23
}, {
  start: 486,
  length: 1,
  convRule: rule22
}, {
  start: 487,
  length: 1,
  convRule: rule23
}, {
  start: 488,
  length: 1,
  convRule: rule22
}, {
  start: 489,
  length: 1,
  convRule: rule23
}, {
  start: 490,
  length: 1,
  convRule: rule22
}, {
  start: 491,
  length: 1,
  convRule: rule23
}, {
  start: 492,
  length: 1,
  convRule: rule22
}, {
  start: 493,
  length: 1,
  convRule: rule23
}, {
  start: 494,
  length: 1,
  convRule: rule22
}, {
  start: 495,
  length: 1,
  convRule: rule23
}, {
  start: 496,
  length: 1,
  convRule: rule20
}, {
  start: 497,
  length: 1,
  convRule: rule47
}, {
  start: 498,
  length: 1,
  convRule: rule48
}, {
  start: 499,
  length: 1,
  convRule: rule49
}, {
  start: 500,
  length: 1,
  convRule: rule22
}, {
  start: 501,
  length: 1,
  convRule: rule23
}, {
  start: 502,
  length: 1,
  convRule: rule51
}, {
  start: 503,
  length: 1,
  convRule: rule52
}, {
  start: 504,
  length: 1,
  convRule: rule22
}, {
  start: 505,
  length: 1,
  convRule: rule23
}, {
  start: 506,
  length: 1,
  convRule: rule22
}, {
  start: 507,
  length: 1,
  convRule: rule23
}, {
  start: 508,
  length: 1,
  convRule: rule22
}, {
  start: 509,
  length: 1,
  convRule: rule23
}, {
  start: 510,
  length: 1,
  convRule: rule22
}, {
  start: 511,
  length: 1,
  convRule: rule23
}, {
  start: 512,
  length: 1,
  convRule: rule22
}, {
  start: 513,
  length: 1,
  convRule: rule23
}, {
  start: 514,
  length: 1,
  convRule: rule22
}, {
  start: 515,
  length: 1,
  convRule: rule23
}, {
  start: 516,
  length: 1,
  convRule: rule22
}, {
  start: 517,
  length: 1,
  convRule: rule23
}, {
  start: 518,
  length: 1,
  convRule: rule22
}, {
  start: 519,
  length: 1,
  convRule: rule23
}, {
  start: 520,
  length: 1,
  convRule: rule22
}, {
  start: 521,
  length: 1,
  convRule: rule23
}, {
  start: 522,
  length: 1,
  convRule: rule22
}, {
  start: 523,
  length: 1,
  convRule: rule23
}, {
  start: 524,
  length: 1,
  convRule: rule22
}, {
  start: 525,
  length: 1,
  convRule: rule23
}, {
  start: 526,
  length: 1,
  convRule: rule22
}, {
  start: 527,
  length: 1,
  convRule: rule23
}, {
  start: 528,
  length: 1,
  convRule: rule22
}, {
  start: 529,
  length: 1,
  convRule: rule23
}, {
  start: 530,
  length: 1,
  convRule: rule22
}, {
  start: 531,
  length: 1,
  convRule: rule23
}, {
  start: 532,
  length: 1,
  convRule: rule22
}, {
  start: 533,
  length: 1,
  convRule: rule23
}, {
  start: 534,
  length: 1,
  convRule: rule22
}, {
  start: 535,
  length: 1,
  convRule: rule23
}, {
  start: 536,
  length: 1,
  convRule: rule22
}, {
  start: 537,
  length: 1,
  convRule: rule23
}, {
  start: 538,
  length: 1,
  convRule: rule22
}, {
  start: 539,
  length: 1,
  convRule: rule23
}, {
  start: 540,
  length: 1,
  convRule: rule22
}, {
  start: 541,
  length: 1,
  convRule: rule23
}, {
  start: 542,
  length: 1,
  convRule: rule22
}, {
  start: 543,
  length: 1,
  convRule: rule23
}, {
  start: 544,
  length: 1,
  convRule: rule53
}, {
  start: 545,
  length: 1,
  convRule: rule20
}, {
  start: 546,
  length: 1,
  convRule: rule22
}, {
  start: 547,
  length: 1,
  convRule: rule23
}, {
  start: 548,
  length: 1,
  convRule: rule22
}, {
  start: 549,
  length: 1,
  convRule: rule23
}, {
  start: 550,
  length: 1,
  convRule: rule22
}, {
  start: 551,
  length: 1,
  convRule: rule23
}, {
  start: 552,
  length: 1,
  convRule: rule22
}, {
  start: 553,
  length: 1,
  convRule: rule23
}, {
  start: 554,
  length: 1,
  convRule: rule22
}, {
  start: 555,
  length: 1,
  convRule: rule23
}, {
  start: 556,
  length: 1,
  convRule: rule22
}, {
  start: 557,
  length: 1,
  convRule: rule23
}, {
  start: 558,
  length: 1,
  convRule: rule22
}, {
  start: 559,
  length: 1,
  convRule: rule23
}, {
  start: 560,
  length: 1,
  convRule: rule22
}, {
  start: 561,
  length: 1,
  convRule: rule23
}, {
  start: 562,
  length: 1,
  convRule: rule22
}, {
  start: 563,
  length: 1,
  convRule: rule23
}, {
  start: 564,
  length: 6,
  convRule: rule20
}, {
  start: 570,
  length: 1,
  convRule: rule54
}, {
  start: 571,
  length: 1,
  convRule: rule22
}, {
  start: 572,
  length: 1,
  convRule: rule23
}, {
  start: 573,
  length: 1,
  convRule: rule55
}, {
  start: 574,
  length: 1,
  convRule: rule56
}, {
  start: 575,
  length: 2,
  convRule: rule57
}, {
  start: 577,
  length: 1,
  convRule: rule22
}, {
  start: 578,
  length: 1,
  convRule: rule23
}, {
  start: 579,
  length: 1,
  convRule: rule58
}, {
  start: 580,
  length: 1,
  convRule: rule59
}, {
  start: 581,
  length: 1,
  convRule: rule60
}, {
  start: 582,
  length: 1,
  convRule: rule22
}, {
  start: 583,
  length: 1,
  convRule: rule23
}, {
  start: 584,
  length: 1,
  convRule: rule22
}, {
  start: 585,
  length: 1,
  convRule: rule23
}, {
  start: 586,
  length: 1,
  convRule: rule22
}, {
  start: 587,
  length: 1,
  convRule: rule23
}, {
  start: 588,
  length: 1,
  convRule: rule22
}, {
  start: 589,
  length: 1,
  convRule: rule23
}, {
  start: 590,
  length: 1,
  convRule: rule22
}, {
  start: 591,
  length: 1,
  convRule: rule23
}, {
  start: 592,
  length: 1,
  convRule: rule61
}, {
  start: 593,
  length: 1,
  convRule: rule62
}, {
  start: 594,
  length: 1,
  convRule: rule63
}, {
  start: 595,
  length: 1,
  convRule: rule64
}, {
  start: 596,
  length: 1,
  convRule: rule65
}, {
  start: 597,
  length: 1,
  convRule: rule20
}, {
  start: 598,
  length: 2,
  convRule: rule66
}, {
  start: 600,
  length: 1,
  convRule: rule20
}, {
  start: 601,
  length: 1,
  convRule: rule67
}, {
  start: 602,
  length: 1,
  convRule: rule20
}, {
  start: 603,
  length: 1,
  convRule: rule68
}, {
  start: 604,
  length: 1,
  convRule: rule69
}, {
  start: 605,
  length: 3,
  convRule: rule20
}, {
  start: 608,
  length: 1,
  convRule: rule66
}, {
  start: 609,
  length: 1,
  convRule: rule70
}, {
  start: 610,
  length: 1,
  convRule: rule20
}, {
  start: 611,
  length: 1,
  convRule: rule71
}, {
  start: 612,
  length: 1,
  convRule: rule20
}, {
  start: 613,
  length: 1,
  convRule: rule72
}, {
  start: 614,
  length: 1,
  convRule: rule73
}, {
  start: 615,
  length: 1,
  convRule: rule20
}, {
  start: 616,
  length: 1,
  convRule: rule74
}, {
  start: 617,
  length: 1,
  convRule: rule75
}, {
  start: 618,
  length: 1,
  convRule: rule73
}, {
  start: 619,
  length: 1,
  convRule: rule76
}, {
  start: 620,
  length: 1,
  convRule: rule77
}, {
  start: 621,
  length: 2,
  convRule: rule20
}, {
  start: 623,
  length: 1,
  convRule: rule75
}, {
  start: 624,
  length: 1,
  convRule: rule20
}, {
  start: 625,
  length: 1,
  convRule: rule78
}, {
  start: 626,
  length: 1,
  convRule: rule79
}, {
  start: 627,
  length: 2,
  convRule: rule20
}, {
  start: 629,
  length: 1,
  convRule: rule80
}, {
  start: 630,
  length: 7,
  convRule: rule20
}, {
  start: 637,
  length: 1,
  convRule: rule81
}, {
  start: 638,
  length: 2,
  convRule: rule20
}, {
  start: 640,
  length: 1,
  convRule: rule82
}, {
  start: 641,
  length: 1,
  convRule: rule20
}, {
  start: 642,
  length: 1,
  convRule: rule83
}, {
  start: 643,
  length: 1,
  convRule: rule82
}, {
  start: 644,
  length: 3,
  convRule: rule20
}, {
  start: 647,
  length: 1,
  convRule: rule84
}, {
  start: 648,
  length: 1,
  convRule: rule82
}, {
  start: 649,
  length: 1,
  convRule: rule85
}, {
  start: 650,
  length: 2,
  convRule: rule86
}, {
  start: 652,
  length: 1,
  convRule: rule87
}, {
  start: 653,
  length: 5,
  convRule: rule20
}, {
  start: 658,
  length: 1,
  convRule: rule88
}, {
  start: 659,
  length: 1,
  convRule: rule20
}, {
  start: 660,
  length: 1,
  convRule: rule14
}, {
  start: 661,
  length: 8,
  convRule: rule20
}, {
  start: 669,
  length: 1,
  convRule: rule89
}, {
  start: 670,
  length: 1,
  convRule: rule90
}, {
  start: 671,
  length: 17,
  convRule: rule20
}, {
  start: 688,
  length: 18,
  convRule: rule91
}, {
  start: 706,
  length: 4,
  convRule: rule10
}, {
  start: 710,
  length: 12,
  convRule: rule91
}, {
  start: 722,
  length: 14,
  convRule: rule10
}, {
  start: 736,
  length: 5,
  convRule: rule91
}, {
  start: 741,
  length: 7,
  convRule: rule10
}, {
  start: 748,
  length: 1,
  convRule: rule91
}, {
  start: 749,
  length: 1,
  convRule: rule10
}, {
  start: 750,
  length: 1,
  convRule: rule91
}, {
  start: 751,
  length: 17,
  convRule: rule10
}, {
  start: 768,
  length: 69,
  convRule: rule92
}, {
  start: 837,
  length: 1,
  convRule: rule93
}, {
  start: 838,
  length: 42,
  convRule: rule92
}, {
  start: 880,
  length: 1,
  convRule: rule22
}, {
  start: 881,
  length: 1,
  convRule: rule23
}, {
  start: 882,
  length: 1,
  convRule: rule22
}, {
  start: 883,
  length: 1,
  convRule: rule23
}, {
  start: 884,
  length: 1,
  convRule: rule91
}, {
  start: 885,
  length: 1,
  convRule: rule10
}, {
  start: 886,
  length: 1,
  convRule: rule22
}, {
  start: 887,
  length: 1,
  convRule: rule23
}, {
  start: 890,
  length: 1,
  convRule: rule91
}, {
  start: 891,
  length: 3,
  convRule: rule41
}, {
  start: 894,
  length: 1,
  convRule: rule2
}, {
  start: 895,
  length: 1,
  convRule: rule94
}, {
  start: 900,
  length: 2,
  convRule: rule10
}, {
  start: 902,
  length: 1,
  convRule: rule95
}, {
  start: 903,
  length: 1,
  convRule: rule2
}, {
  start: 904,
  length: 3,
  convRule: rule96
}, {
  start: 908,
  length: 1,
  convRule: rule97
}, {
  start: 910,
  length: 2,
  convRule: rule98
}, {
  start: 912,
  length: 1,
  convRule: rule20
}, {
  start: 913,
  length: 17,
  convRule: rule9
}, {
  start: 931,
  length: 9,
  convRule: rule9
}, {
  start: 940,
  length: 1,
  convRule: rule99
}, {
  start: 941,
  length: 3,
  convRule: rule100
}, {
  start: 944,
  length: 1,
  convRule: rule20
}, {
  start: 945,
  length: 17,
  convRule: rule12
}, {
  start: 962,
  length: 1,
  convRule: rule101
}, {
  start: 963,
  length: 9,
  convRule: rule12
}, {
  start: 972,
  length: 1,
  convRule: rule102
}, {
  start: 973,
  length: 2,
  convRule: rule103
}, {
  start: 975,
  length: 1,
  convRule: rule104
}, {
  start: 976,
  length: 1,
  convRule: rule105
}, {
  start: 977,
  length: 1,
  convRule: rule106
}, {
  start: 978,
  length: 3,
  convRule: rule107
}, {
  start: 981,
  length: 1,
  convRule: rule108
}, {
  start: 982,
  length: 1,
  convRule: rule109
}, {
  start: 983,
  length: 1,
  convRule: rule110
}, {
  start: 984,
  length: 1,
  convRule: rule22
}, {
  start: 985,
  length: 1,
  convRule: rule23
}, {
  start: 986,
  length: 1,
  convRule: rule22
}, {
  start: 987,
  length: 1,
  convRule: rule23
}, {
  start: 988,
  length: 1,
  convRule: rule22
}, {
  start: 989,
  length: 1,
  convRule: rule23
}, {
  start: 990,
  length: 1,
  convRule: rule22
}, {
  start: 991,
  length: 1,
  convRule: rule23
}, {
  start: 992,
  length: 1,
  convRule: rule22
}, {
  start: 993,
  length: 1,
  convRule: rule23
}, {
  start: 994,
  length: 1,
  convRule: rule22
}, {
  start: 995,
  length: 1,
  convRule: rule23
}, {
  start: 996,
  length: 1,
  convRule: rule22
}, {
  start: 997,
  length: 1,
  convRule: rule23
}, {
  start: 998,
  length: 1,
  convRule: rule22
}, {
  start: 999,
  length: 1,
  convRule: rule23
}, {
  start: 1e3,
  length: 1,
  convRule: rule22
}, {
  start: 1001,
  length: 1,
  convRule: rule23
}, {
  start: 1002,
  length: 1,
  convRule: rule22
}, {
  start: 1003,
  length: 1,
  convRule: rule23
}, {
  start: 1004,
  length: 1,
  convRule: rule22
}, {
  start: 1005,
  length: 1,
  convRule: rule23
}, {
  start: 1006,
  length: 1,
  convRule: rule22
}, {
  start: 1007,
  length: 1,
  convRule: rule23
}, {
  start: 1008,
  length: 1,
  convRule: rule111
}, {
  start: 1009,
  length: 1,
  convRule: rule112
}, {
  start: 1010,
  length: 1,
  convRule: rule113
}, {
  start: 1011,
  length: 1,
  convRule: rule114
}, {
  start: 1012,
  length: 1,
  convRule: rule115
}, {
  start: 1013,
  length: 1,
  convRule: rule116
}, {
  start: 1014,
  length: 1,
  convRule: rule6
}, {
  start: 1015,
  length: 1,
  convRule: rule22
}, {
  start: 1016,
  length: 1,
  convRule: rule23
}, {
  start: 1017,
  length: 1,
  convRule: rule117
}, {
  start: 1018,
  length: 1,
  convRule: rule22
}, {
  start: 1019,
  length: 1,
  convRule: rule23
}, {
  start: 1020,
  length: 1,
  convRule: rule20
}, {
  start: 1021,
  length: 3,
  convRule: rule53
}, {
  start: 1024,
  length: 16,
  convRule: rule118
}, {
  start: 1040,
  length: 32,
  convRule: rule9
}, {
  start: 1072,
  length: 32,
  convRule: rule12
}, {
  start: 1104,
  length: 16,
  convRule: rule112
}, {
  start: 1120,
  length: 1,
  convRule: rule22
}, {
  start: 1121,
  length: 1,
  convRule: rule23
}, {
  start: 1122,
  length: 1,
  convRule: rule22
}, {
  start: 1123,
  length: 1,
  convRule: rule23
}, {
  start: 1124,
  length: 1,
  convRule: rule22
}, {
  start: 1125,
  length: 1,
  convRule: rule23
}, {
  start: 1126,
  length: 1,
  convRule: rule22
}, {
  start: 1127,
  length: 1,
  convRule: rule23
}, {
  start: 1128,
  length: 1,
  convRule: rule22
}, {
  start: 1129,
  length: 1,
  convRule: rule23
}, {
  start: 1130,
  length: 1,
  convRule: rule22
}, {
  start: 1131,
  length: 1,
  convRule: rule23
}, {
  start: 1132,
  length: 1,
  convRule: rule22
}, {
  start: 1133,
  length: 1,
  convRule: rule23
}, {
  start: 1134,
  length: 1,
  convRule: rule22
}, {
  start: 1135,
  length: 1,
  convRule: rule23
}, {
  start: 1136,
  length: 1,
  convRule: rule22
}, {
  start: 1137,
  length: 1,
  convRule: rule23
}, {
  start: 1138,
  length: 1,
  convRule: rule22
}, {
  start: 1139,
  length: 1,
  convRule: rule23
}, {
  start: 1140,
  length: 1,
  convRule: rule22
}, {
  start: 1141,
  length: 1,
  convRule: rule23
}, {
  start: 1142,
  length: 1,
  convRule: rule22
}, {
  start: 1143,
  length: 1,
  convRule: rule23
}, {
  start: 1144,
  length: 1,
  convRule: rule22
}, {
  start: 1145,
  length: 1,
  convRule: rule23
}, {
  start: 1146,
  length: 1,
  convRule: rule22
}, {
  start: 1147,
  length: 1,
  convRule: rule23
}, {
  start: 1148,
  length: 1,
  convRule: rule22
}, {
  start: 1149,
  length: 1,
  convRule: rule23
}, {
  start: 1150,
  length: 1,
  convRule: rule22
}, {
  start: 1151,
  length: 1,
  convRule: rule23
}, {
  start: 1152,
  length: 1,
  convRule: rule22
}, {
  start: 1153,
  length: 1,
  convRule: rule23
}, {
  start: 1154,
  length: 1,
  convRule: rule13
}, {
  start: 1155,
  length: 5,
  convRule: rule92
}, {
  start: 1160,
  length: 2,
  convRule: rule119
}, {
  start: 1162,
  length: 1,
  convRule: rule22
}, {
  start: 1163,
  length: 1,
  convRule: rule23
}, {
  start: 1164,
  length: 1,
  convRule: rule22
}, {
  start: 1165,
  length: 1,
  convRule: rule23
}, {
  start: 1166,
  length: 1,
  convRule: rule22
}, {
  start: 1167,
  length: 1,
  convRule: rule23
}, {
  start: 1168,
  length: 1,
  convRule: rule22
}, {
  start: 1169,
  length: 1,
  convRule: rule23
}, {
  start: 1170,
  length: 1,
  convRule: rule22
}, {
  start: 1171,
  length: 1,
  convRule: rule23
}, {
  start: 1172,
  length: 1,
  convRule: rule22
}, {
  start: 1173,
  length: 1,
  convRule: rule23
}, {
  start: 1174,
  length: 1,
  convRule: rule22
}, {
  start: 1175,
  length: 1,
  convRule: rule23
}, {
  start: 1176,
  length: 1,
  convRule: rule22
}, {
  start: 1177,
  length: 1,
  convRule: rule23
}, {
  start: 1178,
  length: 1,
  convRule: rule22
}, {
  start: 1179,
  length: 1,
  convRule: rule23
}, {
  start: 1180,
  length: 1,
  convRule: rule22
}, {
  start: 1181,
  length: 1,
  convRule: rule23
}, {
  start: 1182,
  length: 1,
  convRule: rule22
}, {
  start: 1183,
  length: 1,
  convRule: rule23
}, {
  start: 1184,
  length: 1,
  convRule: rule22
}, {
  start: 1185,
  length: 1,
  convRule: rule23
}, {
  start: 1186,
  length: 1,
  convRule: rule22
}, {
  start: 1187,
  length: 1,
  convRule: rule23
}, {
  start: 1188,
  length: 1,
  convRule: rule22
}, {
  start: 1189,
  length: 1,
  convRule: rule23
}, {
  start: 1190,
  length: 1,
  convRule: rule22
}, {
  start: 1191,
  length: 1,
  convRule: rule23
}, {
  start: 1192,
  length: 1,
  convRule: rule22
}, {
  start: 1193,
  length: 1,
  convRule: rule23
}, {
  start: 1194,
  length: 1,
  convRule: rule22
}, {
  start: 1195,
  length: 1,
  convRule: rule23
}, {
  start: 1196,
  length: 1,
  convRule: rule22
}, {
  start: 1197,
  length: 1,
  convRule: rule23
}, {
  start: 1198,
  length: 1,
  convRule: rule22
}, {
  start: 1199,
  length: 1,
  convRule: rule23
}, {
  start: 1200,
  length: 1,
  convRule: rule22
}, {
  start: 1201,
  length: 1,
  convRule: rule23
}, {
  start: 1202,
  length: 1,
  convRule: rule22
}, {
  start: 1203,
  length: 1,
  convRule: rule23
}, {
  start: 1204,
  length: 1,
  convRule: rule22
}, {
  start: 1205,
  length: 1,
  convRule: rule23
}, {
  start: 1206,
  length: 1,
  convRule: rule22
}, {
  start: 1207,
  length: 1,
  convRule: rule23
}, {
  start: 1208,
  length: 1,
  convRule: rule22
}, {
  start: 1209,
  length: 1,
  convRule: rule23
}, {
  start: 1210,
  length: 1,
  convRule: rule22
}, {
  start: 1211,
  length: 1,
  convRule: rule23
}, {
  start: 1212,
  length: 1,
  convRule: rule22
}, {
  start: 1213,
  length: 1,
  convRule: rule23
}, {
  start: 1214,
  length: 1,
  convRule: rule22
}, {
  start: 1215,
  length: 1,
  convRule: rule23
}, {
  start: 1216,
  length: 1,
  convRule: rule120
}, {
  start: 1217,
  length: 1,
  convRule: rule22
}, {
  start: 1218,
  length: 1,
  convRule: rule23
}, {
  start: 1219,
  length: 1,
  convRule: rule22
}, {
  start: 1220,
  length: 1,
  convRule: rule23
}, {
  start: 1221,
  length: 1,
  convRule: rule22
}, {
  start: 1222,
  length: 1,
  convRule: rule23
}, {
  start: 1223,
  length: 1,
  convRule: rule22
}, {
  start: 1224,
  length: 1,
  convRule: rule23
}, {
  start: 1225,
  length: 1,
  convRule: rule22
}, {
  start: 1226,
  length: 1,
  convRule: rule23
}, {
  start: 1227,
  length: 1,
  convRule: rule22
}, {
  start: 1228,
  length: 1,
  convRule: rule23
}, {
  start: 1229,
  length: 1,
  convRule: rule22
}, {
  start: 1230,
  length: 1,
  convRule: rule23
}, {
  start: 1231,
  length: 1,
  convRule: rule121
}, {
  start: 1232,
  length: 1,
  convRule: rule22
}, {
  start: 1233,
  length: 1,
  convRule: rule23
}, {
  start: 1234,
  length: 1,
  convRule: rule22
}, {
  start: 1235,
  length: 1,
  convRule: rule23
}, {
  start: 1236,
  length: 1,
  convRule: rule22
}, {
  start: 1237,
  length: 1,
  convRule: rule23
}, {
  start: 1238,
  length: 1,
  convRule: rule22
}, {
  start: 1239,
  length: 1,
  convRule: rule23
}, {
  start: 1240,
  length: 1,
  convRule: rule22
}, {
  start: 1241,
  length: 1,
  convRule: rule23
}, {
  start: 1242,
  length: 1,
  convRule: rule22
}, {
  start: 1243,
  length: 1,
  convRule: rule23
}, {
  start: 1244,
  length: 1,
  convRule: rule22
}, {
  start: 1245,
  length: 1,
  convRule: rule23
}, {
  start: 1246,
  length: 1,
  convRule: rule22
}, {
  start: 1247,
  length: 1,
  convRule: rule23
}, {
  start: 1248,
  length: 1,
  convRule: rule22
}, {
  start: 1249,
  length: 1,
  convRule: rule23
}, {
  start: 1250,
  length: 1,
  convRule: rule22
}, {
  start: 1251,
  length: 1,
  convRule: rule23
}, {
  start: 1252,
  length: 1,
  convRule: rule22
}, {
  start: 1253,
  length: 1,
  convRule: rule23
}, {
  start: 1254,
  length: 1,
  convRule: rule22
}, {
  start: 1255,
  length: 1,
  convRule: rule23
}, {
  start: 1256,
  length: 1,
  convRule: rule22
}, {
  start: 1257,
  length: 1,
  convRule: rule23
}, {
  start: 1258,
  length: 1,
  convRule: rule22
}, {
  start: 1259,
  length: 1,
  convRule: rule23
}, {
  start: 1260,
  length: 1,
  convRule: rule22
}, {
  start: 1261,
  length: 1,
  convRule: rule23
}, {
  start: 1262,
  length: 1,
  convRule: rule22
}, {
  start: 1263,
  length: 1,
  convRule: rule23
}, {
  start: 1264,
  length: 1,
  convRule: rule22
}, {
  start: 1265,
  length: 1,
  convRule: rule23
}, {
  start: 1266,
  length: 1,
  convRule: rule22
}, {
  start: 1267,
  length: 1,
  convRule: rule23
}, {
  start: 1268,
  length: 1,
  convRule: rule22
}, {
  start: 1269,
  length: 1,
  convRule: rule23
}, {
  start: 1270,
  length: 1,
  convRule: rule22
}, {
  start: 1271,
  length: 1,
  convRule: rule23
}, {
  start: 1272,
  length: 1,
  convRule: rule22
}, {
  start: 1273,
  length: 1,
  convRule: rule23
}, {
  start: 1274,
  length: 1,
  convRule: rule22
}, {
  start: 1275,
  length: 1,
  convRule: rule23
}, {
  start: 1276,
  length: 1,
  convRule: rule22
}, {
  start: 1277,
  length: 1,
  convRule: rule23
}, {
  start: 1278,
  length: 1,
  convRule: rule22
}, {
  start: 1279,
  length: 1,
  convRule: rule23
}, {
  start: 1280,
  length: 1,
  convRule: rule22
}, {
  start: 1281,
  length: 1,
  convRule: rule23
}, {
  start: 1282,
  length: 1,
  convRule: rule22
}, {
  start: 1283,
  length: 1,
  convRule: rule23
}, {
  start: 1284,
  length: 1,
  convRule: rule22
}, {
  start: 1285,
  length: 1,
  convRule: rule23
}, {
  start: 1286,
  length: 1,
  convRule: rule22
}, {
  start: 1287,
  length: 1,
  convRule: rule23
}, {
  start: 1288,
  length: 1,
  convRule: rule22
}, {
  start: 1289,
  length: 1,
  convRule: rule23
}, {
  start: 1290,
  length: 1,
  convRule: rule22
}, {
  start: 1291,
  length: 1,
  convRule: rule23
}, {
  start: 1292,
  length: 1,
  convRule: rule22
}, {
  start: 1293,
  length: 1,
  convRule: rule23
}, {
  start: 1294,
  length: 1,
  convRule: rule22
}, {
  start: 1295,
  length: 1,
  convRule: rule23
}, {
  start: 1296,
  length: 1,
  convRule: rule22
}, {
  start: 1297,
  length: 1,
  convRule: rule23
}, {
  start: 1298,
  length: 1,
  convRule: rule22
}, {
  start: 1299,
  length: 1,
  convRule: rule23
}, {
  start: 1300,
  length: 1,
  convRule: rule22
}, {
  start: 1301,
  length: 1,
  convRule: rule23
}, {
  start: 1302,
  length: 1,
  convRule: rule22
}, {
  start: 1303,
  length: 1,
  convRule: rule23
}, {
  start: 1304,
  length: 1,
  convRule: rule22
}, {
  start: 1305,
  length: 1,
  convRule: rule23
}, {
  start: 1306,
  length: 1,
  convRule: rule22
}, {
  start: 1307,
  length: 1,
  convRule: rule23
}, {
  start: 1308,
  length: 1,
  convRule: rule22
}, {
  start: 1309,
  length: 1,
  convRule: rule23
}, {
  start: 1310,
  length: 1,
  convRule: rule22
}, {
  start: 1311,
  length: 1,
  convRule: rule23
}, {
  start: 1312,
  length: 1,
  convRule: rule22
}, {
  start: 1313,
  length: 1,
  convRule: rule23
}, {
  start: 1314,
  length: 1,
  convRule: rule22
}, {
  start: 1315,
  length: 1,
  convRule: rule23
}, {
  start: 1316,
  length: 1,
  convRule: rule22
}, {
  start: 1317,
  length: 1,
  convRule: rule23
}, {
  start: 1318,
  length: 1,
  convRule: rule22
}, {
  start: 1319,
  length: 1,
  convRule: rule23
}, {
  start: 1320,
  length: 1,
  convRule: rule22
}, {
  start: 1321,
  length: 1,
  convRule: rule23
}, {
  start: 1322,
  length: 1,
  convRule: rule22
}, {
  start: 1323,
  length: 1,
  convRule: rule23
}, {
  start: 1324,
  length: 1,
  convRule: rule22
}, {
  start: 1325,
  length: 1,
  convRule: rule23
}, {
  start: 1326,
  length: 1,
  convRule: rule22
}, {
  start: 1327,
  length: 1,
  convRule: rule23
}, {
  start: 1329,
  length: 38,
  convRule: rule122
}, {
  start: 1369,
  length: 1,
  convRule: rule91
}, {
  start: 1370,
  length: 6,
  convRule: rule2
}, {
  start: 1376,
  length: 1,
  convRule: rule20
}, {
  start: 1377,
  length: 38,
  convRule: rule123
}, {
  start: 1415,
  length: 2,
  convRule: rule20
}, {
  start: 1417,
  length: 1,
  convRule: rule2
}, {
  start: 1418,
  length: 1,
  convRule: rule7
}, {
  start: 1421,
  length: 2,
  convRule: rule13
}, {
  start: 1423,
  length: 1,
  convRule: rule3
}, {
  start: 1425,
  length: 45,
  convRule: rule92
}, {
  start: 1470,
  length: 1,
  convRule: rule7
}, {
  start: 1471,
  length: 1,
  convRule: rule92
}, {
  start: 1472,
  length: 1,
  convRule: rule2
}, {
  start: 1473,
  length: 2,
  convRule: rule92
}, {
  start: 1475,
  length: 1,
  convRule: rule2
}, {
  start: 1476,
  length: 2,
  convRule: rule92
}, {
  start: 1478,
  length: 1,
  convRule: rule2
}, {
  start: 1479,
  length: 1,
  convRule: rule92
}, {
  start: 1488,
  length: 27,
  convRule: rule14
}, {
  start: 1519,
  length: 4,
  convRule: rule14
}, {
  start: 1523,
  length: 2,
  convRule: rule2
}, {
  start: 1536,
  length: 6,
  convRule: rule16
}, {
  start: 1542,
  length: 3,
  convRule: rule6
}, {
  start: 1545,
  length: 2,
  convRule: rule2
}, {
  start: 1547,
  length: 1,
  convRule: rule3
}, {
  start: 1548,
  length: 2,
  convRule: rule2
}, {
  start: 1550,
  length: 2,
  convRule: rule13
}, {
  start: 1552,
  length: 11,
  convRule: rule92
}, {
  start: 1563,
  length: 1,
  convRule: rule2
}, {
  start: 1564,
  length: 1,
  convRule: rule16
}, {
  start: 1566,
  length: 2,
  convRule: rule2
}, {
  start: 1568,
  length: 32,
  convRule: rule14
}, {
  start: 1600,
  length: 1,
  convRule: rule91
}, {
  start: 1601,
  length: 10,
  convRule: rule14
}, {
  start: 1611,
  length: 21,
  convRule: rule92
}, {
  start: 1632,
  length: 10,
  convRule: rule8
}, {
  start: 1642,
  length: 4,
  convRule: rule2
}, {
  start: 1646,
  length: 2,
  convRule: rule14
}, {
  start: 1648,
  length: 1,
  convRule: rule92
}, {
  start: 1649,
  length: 99,
  convRule: rule14
}, {
  start: 1748,
  length: 1,
  convRule: rule2
}, {
  start: 1749,
  length: 1,
  convRule: rule14
}, {
  start: 1750,
  length: 7,
  convRule: rule92
}, {
  start: 1757,
  length: 1,
  convRule: rule16
}, {
  start: 1758,
  length: 1,
  convRule: rule13
}, {
  start: 1759,
  length: 6,
  convRule: rule92
}, {
  start: 1765,
  length: 2,
  convRule: rule91
}, {
  start: 1767,
  length: 2,
  convRule: rule92
}, {
  start: 1769,
  length: 1,
  convRule: rule13
}, {
  start: 1770,
  length: 4,
  convRule: rule92
}, {
  start: 1774,
  length: 2,
  convRule: rule14
}, {
  start: 1776,
  length: 10,
  convRule: rule8
}, {
  start: 1786,
  length: 3,
  convRule: rule14
}, {
  start: 1789,
  length: 2,
  convRule: rule13
}, {
  start: 1791,
  length: 1,
  convRule: rule14
}, {
  start: 1792,
  length: 14,
  convRule: rule2
}, {
  start: 1807,
  length: 1,
  convRule: rule16
}, {
  start: 1808,
  length: 1,
  convRule: rule14
}, {
  start: 1809,
  length: 1,
  convRule: rule92
}, {
  start: 1810,
  length: 30,
  convRule: rule14
}, {
  start: 1840,
  length: 27,
  convRule: rule92
}, {
  start: 1869,
  length: 89,
  convRule: rule14
}, {
  start: 1958,
  length: 11,
  convRule: rule92
}, {
  start: 1969,
  length: 1,
  convRule: rule14
}, {
  start: 1984,
  length: 10,
  convRule: rule8
}, {
  start: 1994,
  length: 33,
  convRule: rule14
}, {
  start: 2027,
  length: 9,
  convRule: rule92
}, {
  start: 2036,
  length: 2,
  convRule: rule91
}, {
  start: 2038,
  length: 1,
  convRule: rule13
}, {
  start: 2039,
  length: 3,
  convRule: rule2
}, {
  start: 2042,
  length: 1,
  convRule: rule91
}, {
  start: 2045,
  length: 1,
  convRule: rule92
}, {
  start: 2046,
  length: 2,
  convRule: rule3
}, {
  start: 2048,
  length: 22,
  convRule: rule14
}, {
  start: 2070,
  length: 4,
  convRule: rule92
}, {
  start: 2074,
  length: 1,
  convRule: rule91
}, {
  start: 2075,
  length: 9,
  convRule: rule92
}, {
  start: 2084,
  length: 1,
  convRule: rule91
}, {
  start: 2085,
  length: 3,
  convRule: rule92
}, {
  start: 2088,
  length: 1,
  convRule: rule91
}, {
  start: 2089,
  length: 5,
  convRule: rule92
}, {
  start: 2096,
  length: 15,
  convRule: rule2
}, {
  start: 2112,
  length: 25,
  convRule: rule14
}, {
  start: 2137,
  length: 3,
  convRule: rule92
}, {
  start: 2142,
  length: 1,
  convRule: rule2
}, {
  start: 2144,
  length: 11,
  convRule: rule14
}, {
  start: 2208,
  length: 21,
  convRule: rule14
}, {
  start: 2230,
  length: 18,
  convRule: rule14
}, {
  start: 2259,
  length: 15,
  convRule: rule92
}, {
  start: 2274,
  length: 1,
  convRule: rule16
}, {
  start: 2275,
  length: 32,
  convRule: rule92
}, {
  start: 2307,
  length: 1,
  convRule: rule124
}, {
  start: 2308,
  length: 54,
  convRule: rule14
}, {
  start: 2362,
  length: 1,
  convRule: rule92
}, {
  start: 2363,
  length: 1,
  convRule: rule124
}, {
  start: 2364,
  length: 1,
  convRule: rule92
}, {
  start: 2365,
  length: 1,
  convRule: rule14
}, {
  start: 2366,
  length: 3,
  convRule: rule124
}, {
  start: 2369,
  length: 8,
  convRule: rule92
}, {
  start: 2377,
  length: 4,
  convRule: rule124
}, {
  start: 2381,
  length: 1,
  convRule: rule92
}, {
  start: 2382,
  length: 2,
  convRule: rule124
}, {
  start: 2384,
  length: 1,
  convRule: rule14
}, {
  start: 2385,
  length: 7,
  convRule: rule92
}, {
  start: 2392,
  length: 10,
  convRule: rule14
}, {
  start: 2402,
  length: 2,
  convRule: rule92
}, {
  start: 2404,
  length: 2,
  convRule: rule2
}, {
  start: 2406,
  length: 10,
  convRule: rule8
}, {
  start: 2416,
  length: 1,
  convRule: rule2
}, {
  start: 2417,
  length: 1,
  convRule: rule91
}, {
  start: 2418,
  length: 15,
  convRule: rule14
}, {
  start: 2433,
  length: 1,
  convRule: rule92
}, {
  start: 2434,
  length: 2,
  convRule: rule124
}, {
  start: 2437,
  length: 8,
  convRule: rule14
}, {
  start: 2447,
  length: 2,
  convRule: rule14
}, {
  start: 2451,
  length: 22,
  convRule: rule14
}, {
  start: 2474,
  length: 7,
  convRule: rule14
}, {
  start: 2482,
  length: 1,
  convRule: rule14
}, {
  start: 2486,
  length: 4,
  convRule: rule14
}, {
  start: 2492,
  length: 1,
  convRule: rule92
}, {
  start: 2493,
  length: 1,
  convRule: rule14
}, {
  start: 2494,
  length: 3,
  convRule: rule124
}, {
  start: 2497,
  length: 4,
  convRule: rule92
}, {
  start: 2503,
  length: 2,
  convRule: rule124
}, {
  start: 2507,
  length: 2,
  convRule: rule124
}, {
  start: 2509,
  length: 1,
  convRule: rule92
}, {
  start: 2510,
  length: 1,
  convRule: rule14
}, {
  start: 2519,
  length: 1,
  convRule: rule124
}, {
  start: 2524,
  length: 2,
  convRule: rule14
}, {
  start: 2527,
  length: 3,
  convRule: rule14
}, {
  start: 2530,
  length: 2,
  convRule: rule92
}, {
  start: 2534,
  length: 10,
  convRule: rule8
}, {
  start: 2544,
  length: 2,
  convRule: rule14
}, {
  start: 2546,
  length: 2,
  convRule: rule3
}, {
  start: 2548,
  length: 6,
  convRule: rule17
}, {
  start: 2554,
  length: 1,
  convRule: rule13
}, {
  start: 2555,
  length: 1,
  convRule: rule3
}, {
  start: 2556,
  length: 1,
  convRule: rule14
}, {
  start: 2557,
  length: 1,
  convRule: rule2
}, {
  start: 2558,
  length: 1,
  convRule: rule92
}, {
  start: 2561,
  length: 2,
  convRule: rule92
}, {
  start: 2563,
  length: 1,
  convRule: rule124
}, {
  start: 2565,
  length: 6,
  convRule: rule14
}, {
  start: 2575,
  length: 2,
  convRule: rule14
}, {
  start: 2579,
  length: 22,
  convRule: rule14
}, {
  start: 2602,
  length: 7,
  convRule: rule14
}, {
  start: 2610,
  length: 2,
  convRule: rule14
}, {
  start: 2613,
  length: 2,
  convRule: rule14
}, {
  start: 2616,
  length: 2,
  convRule: rule14
}, {
  start: 2620,
  length: 1,
  convRule: rule92
}, {
  start: 2622,
  length: 3,
  convRule: rule124
}, {
  start: 2625,
  length: 2,
  convRule: rule92
}, {
  start: 2631,
  length: 2,
  convRule: rule92
}, {
  start: 2635,
  length: 3,
  convRule: rule92
}, {
  start: 2641,
  length: 1,
  convRule: rule92
}, {
  start: 2649,
  length: 4,
  convRule: rule14
}, {
  start: 2654,
  length: 1,
  convRule: rule14
}, {
  start: 2662,
  length: 10,
  convRule: rule8
}, {
  start: 2672,
  length: 2,
  convRule: rule92
}, {
  start: 2674,
  length: 3,
  convRule: rule14
}, {
  start: 2677,
  length: 1,
  convRule: rule92
}, {
  start: 2678,
  length: 1,
  convRule: rule2
}, {
  start: 2689,
  length: 2,
  convRule: rule92
}, {
  start: 2691,
  length: 1,
  convRule: rule124
}, {
  start: 2693,
  length: 9,
  convRule: rule14
}, {
  start: 2703,
  length: 3,
  convRule: rule14
}, {
  start: 2707,
  length: 22,
  convRule: rule14
}, {
  start: 2730,
  length: 7,
  convRule: rule14
}, {
  start: 2738,
  length: 2,
  convRule: rule14
}, {
  start: 2741,
  length: 5,
  convRule: rule14
}, {
  start: 2748,
  length: 1,
  convRule: rule92
}, {
  start: 2749,
  length: 1,
  convRule: rule14
}, {
  start: 2750,
  length: 3,
  convRule: rule124
}, {
  start: 2753,
  length: 5,
  convRule: rule92
}, {
  start: 2759,
  length: 2,
  convRule: rule92
}, {
  start: 2761,
  length: 1,
  convRule: rule124
}, {
  start: 2763,
  length: 2,
  convRule: rule124
}, {
  start: 2765,
  length: 1,
  convRule: rule92
}, {
  start: 2768,
  length: 1,
  convRule: rule14
}, {
  start: 2784,
  length: 2,
  convRule: rule14
}, {
  start: 2786,
  length: 2,
  convRule: rule92
}, {
  start: 2790,
  length: 10,
  convRule: rule8
}, {
  start: 2800,
  length: 1,
  convRule: rule2
}, {
  start: 2801,
  length: 1,
  convRule: rule3
}, {
  start: 2809,
  length: 1,
  convRule: rule14
}, {
  start: 2810,
  length: 6,
  convRule: rule92
}, {
  start: 2817,
  length: 1,
  convRule: rule92
}, {
  start: 2818,
  length: 2,
  convRule: rule124
}, {
  start: 2821,
  length: 8,
  convRule: rule14
}, {
  start: 2831,
  length: 2,
  convRule: rule14
}, {
  start: 2835,
  length: 22,
  convRule: rule14
}, {
  start: 2858,
  length: 7,
  convRule: rule14
}, {
  start: 2866,
  length: 2,
  convRule: rule14
}, {
  start: 2869,
  length: 5,
  convRule: rule14
}, {
  start: 2876,
  length: 1,
  convRule: rule92
}, {
  start: 2877,
  length: 1,
  convRule: rule14
}, {
  start: 2878,
  length: 1,
  convRule: rule124
}, {
  start: 2879,
  length: 1,
  convRule: rule92
}, {
  start: 2880,
  length: 1,
  convRule: rule124
}, {
  start: 2881,
  length: 4,
  convRule: rule92
}, {
  start: 2887,
  length: 2,
  convRule: rule124
}, {
  start: 2891,
  length: 2,
  convRule: rule124
}, {
  start: 2893,
  length: 1,
  convRule: rule92
}, {
  start: 2901,
  length: 2,
  convRule: rule92
}, {
  start: 2903,
  length: 1,
  convRule: rule124
}, {
  start: 2908,
  length: 2,
  convRule: rule14
}, {
  start: 2911,
  length: 3,
  convRule: rule14
}, {
  start: 2914,
  length: 2,
  convRule: rule92
}, {
  start: 2918,
  length: 10,
  convRule: rule8
}, {
  start: 2928,
  length: 1,
  convRule: rule13
}, {
  start: 2929,
  length: 1,
  convRule: rule14
}, {
  start: 2930,
  length: 6,
  convRule: rule17
}, {
  start: 2946,
  length: 1,
  convRule: rule92
}, {
  start: 2947,
  length: 1,
  convRule: rule14
}, {
  start: 2949,
  length: 6,
  convRule: rule14
}, {
  start: 2958,
  length: 3,
  convRule: rule14
}, {
  start: 2962,
  length: 4,
  convRule: rule14
}, {
  start: 2969,
  length: 2,
  convRule: rule14
}, {
  start: 2972,
  length: 1,
  convRule: rule14
}, {
  start: 2974,
  length: 2,
  convRule: rule14
}, {
  start: 2979,
  length: 2,
  convRule: rule14
}, {
  start: 2984,
  length: 3,
  convRule: rule14
}, {
  start: 2990,
  length: 12,
  convRule: rule14
}, {
  start: 3006,
  length: 2,
  convRule: rule124
}, {
  start: 3008,
  length: 1,
  convRule: rule92
}, {
  start: 3009,
  length: 2,
  convRule: rule124
}, {
  start: 3014,
  length: 3,
  convRule: rule124
}, {
  start: 3018,
  length: 3,
  convRule: rule124
}, {
  start: 3021,
  length: 1,
  convRule: rule92
}, {
  start: 3024,
  length: 1,
  convRule: rule14
}, {
  start: 3031,
  length: 1,
  convRule: rule124
}, {
  start: 3046,
  length: 10,
  convRule: rule8
}, {
  start: 3056,
  length: 3,
  convRule: rule17
}, {
  start: 3059,
  length: 6,
  convRule: rule13
}, {
  start: 3065,
  length: 1,
  convRule: rule3
}, {
  start: 3066,
  length: 1,
  convRule: rule13
}, {
  start: 3072,
  length: 1,
  convRule: rule92
}, {
  start: 3073,
  length: 3,
  convRule: rule124
}, {
  start: 3076,
  length: 1,
  convRule: rule92
}, {
  start: 3077,
  length: 8,
  convRule: rule14
}, {
  start: 3086,
  length: 3,
  convRule: rule14
}, {
  start: 3090,
  length: 23,
  convRule: rule14
}, {
  start: 3114,
  length: 16,
  convRule: rule14
}, {
  start: 3133,
  length: 1,
  convRule: rule14
}, {
  start: 3134,
  length: 3,
  convRule: rule92
}, {
  start: 3137,
  length: 4,
  convRule: rule124
}, {
  start: 3142,
  length: 3,
  convRule: rule92
}, {
  start: 3146,
  length: 4,
  convRule: rule92
}, {
  start: 3157,
  length: 2,
  convRule: rule92
}, {
  start: 3160,
  length: 3,
  convRule: rule14
}, {
  start: 3168,
  length: 2,
  convRule: rule14
}, {
  start: 3170,
  length: 2,
  convRule: rule92
}, {
  start: 3174,
  length: 10,
  convRule: rule8
}, {
  start: 3191,
  length: 1,
  convRule: rule2
}, {
  start: 3192,
  length: 7,
  convRule: rule17
}, {
  start: 3199,
  length: 1,
  convRule: rule13
}, {
  start: 3200,
  length: 1,
  convRule: rule14
}, {
  start: 3201,
  length: 1,
  convRule: rule92
}, {
  start: 3202,
  length: 2,
  convRule: rule124
}, {
  start: 3204,
  length: 1,
  convRule: rule2
}, {
  start: 3205,
  length: 8,
  convRule: rule14
}, {
  start: 3214,
  length: 3,
  convRule: rule14
}, {
  start: 3218,
  length: 23,
  convRule: rule14
}, {
  start: 3242,
  length: 10,
  convRule: rule14
}, {
  start: 3253,
  length: 5,
  convRule: rule14
}, {
  start: 3260,
  length: 1,
  convRule: rule92
}, {
  start: 3261,
  length: 1,
  convRule: rule14
}, {
  start: 3262,
  length: 1,
  convRule: rule124
}, {
  start: 3263,
  length: 1,
  convRule: rule92
}, {
  start: 3264,
  length: 5,
  convRule: rule124
}, {
  start: 3270,
  length: 1,
  convRule: rule92
}, {
  start: 3271,
  length: 2,
  convRule: rule124
}, {
  start: 3274,
  length: 2,
  convRule: rule124
}, {
  start: 3276,
  length: 2,
  convRule: rule92
}, {
  start: 3285,
  length: 2,
  convRule: rule124
}, {
  start: 3294,
  length: 1,
  convRule: rule14
}, {
  start: 3296,
  length: 2,
  convRule: rule14
}, {
  start: 3298,
  length: 2,
  convRule: rule92
}, {
  start: 3302,
  length: 10,
  convRule: rule8
}, {
  start: 3313,
  length: 2,
  convRule: rule14
}, {
  start: 3328,
  length: 2,
  convRule: rule92
}, {
  start: 3330,
  length: 2,
  convRule: rule124
}, {
  start: 3332,
  length: 9,
  convRule: rule14
}, {
  start: 3342,
  length: 3,
  convRule: rule14
}, {
  start: 3346,
  length: 41,
  convRule: rule14
}, {
  start: 3387,
  length: 2,
  convRule: rule92
}, {
  start: 3389,
  length: 1,
  convRule: rule14
}, {
  start: 3390,
  length: 3,
  convRule: rule124
}, {
  start: 3393,
  length: 4,
  convRule: rule92
}, {
  start: 3398,
  length: 3,
  convRule: rule124
}, {
  start: 3402,
  length: 3,
  convRule: rule124
}, {
  start: 3405,
  length: 1,
  convRule: rule92
}, {
  start: 3406,
  length: 1,
  convRule: rule14
}, {
  start: 3407,
  length: 1,
  convRule: rule13
}, {
  start: 3412,
  length: 3,
  convRule: rule14
}, {
  start: 3415,
  length: 1,
  convRule: rule124
}, {
  start: 3416,
  length: 7,
  convRule: rule17
}, {
  start: 3423,
  length: 3,
  convRule: rule14
}, {
  start: 3426,
  length: 2,
  convRule: rule92
}, {
  start: 3430,
  length: 10,
  convRule: rule8
}, {
  start: 3440,
  length: 9,
  convRule: rule17
}, {
  start: 3449,
  length: 1,
  convRule: rule13
}, {
  start: 3450,
  length: 6,
  convRule: rule14
}, {
  start: 3457,
  length: 1,
  convRule: rule92
}, {
  start: 3458,
  length: 2,
  convRule: rule124
}, {
  start: 3461,
  length: 18,
  convRule: rule14
}, {
  start: 3482,
  length: 24,
  convRule: rule14
}, {
  start: 3507,
  length: 9,
  convRule: rule14
}, {
  start: 3517,
  length: 1,
  convRule: rule14
}, {
  start: 3520,
  length: 7,
  convRule: rule14
}, {
  start: 3530,
  length: 1,
  convRule: rule92
}, {
  start: 3535,
  length: 3,
  convRule: rule124
}, {
  start: 3538,
  length: 3,
  convRule: rule92
}, {
  start: 3542,
  length: 1,
  convRule: rule92
}, {
  start: 3544,
  length: 8,
  convRule: rule124
}, {
  start: 3558,
  length: 10,
  convRule: rule8
}, {
  start: 3570,
  length: 2,
  convRule: rule124
}, {
  start: 3572,
  length: 1,
  convRule: rule2
}, {
  start: 3585,
  length: 48,
  convRule: rule14
}, {
  start: 3633,
  length: 1,
  convRule: rule92
}, {
  start: 3634,
  length: 2,
  convRule: rule14
}, {
  start: 3636,
  length: 7,
  convRule: rule92
}, {
  start: 3647,
  length: 1,
  convRule: rule3
}, {
  start: 3648,
  length: 6,
  convRule: rule14
}, {
  start: 3654,
  length: 1,
  convRule: rule91
}, {
  start: 3655,
  length: 8,
  convRule: rule92
}, {
  start: 3663,
  length: 1,
  convRule: rule2
}, {
  start: 3664,
  length: 10,
  convRule: rule8
}, {
  start: 3674,
  length: 2,
  convRule: rule2
}, {
  start: 3713,
  length: 2,
  convRule: rule14
}, {
  start: 3716,
  length: 1,
  convRule: rule14
}, {
  start: 3718,
  length: 5,
  convRule: rule14
}, {
  start: 3724,
  length: 24,
  convRule: rule14
}, {
  start: 3749,
  length: 1,
  convRule: rule14
}, {
  start: 3751,
  length: 10,
  convRule: rule14
}, {
  start: 3761,
  length: 1,
  convRule: rule92
}, {
  start: 3762,
  length: 2,
  convRule: rule14
}, {
  start: 3764,
  length: 9,
  convRule: rule92
}, {
  start: 3773,
  length: 1,
  convRule: rule14
}, {
  start: 3776,
  length: 5,
  convRule: rule14
}, {
  start: 3782,
  length: 1,
  convRule: rule91
}, {
  start: 3784,
  length: 6,
  convRule: rule92
}, {
  start: 3792,
  length: 10,
  convRule: rule8
}, {
  start: 3804,
  length: 4,
  convRule: rule14
}, {
  start: 3840,
  length: 1,
  convRule: rule14
}, {
  start: 3841,
  length: 3,
  convRule: rule13
}, {
  start: 3844,
  length: 15,
  convRule: rule2
}, {
  start: 3859,
  length: 1,
  convRule: rule13
}, {
  start: 3860,
  length: 1,
  convRule: rule2
}, {
  start: 3861,
  length: 3,
  convRule: rule13
}, {
  start: 3864,
  length: 2,
  convRule: rule92
}, {
  start: 3866,
  length: 6,
  convRule: rule13
}, {
  start: 3872,
  length: 10,
  convRule: rule8
}, {
  start: 3882,
  length: 10,
  convRule: rule17
}, {
  start: 3892,
  length: 1,
  convRule: rule13
}, {
  start: 3893,
  length: 1,
  convRule: rule92
}, {
  start: 3894,
  length: 1,
  convRule: rule13
}, {
  start: 3895,
  length: 1,
  convRule: rule92
}, {
  start: 3896,
  length: 1,
  convRule: rule13
}, {
  start: 3897,
  length: 1,
  convRule: rule92
}, {
  start: 3898,
  length: 1,
  convRule: rule4
}, {
  start: 3899,
  length: 1,
  convRule: rule5
}, {
  start: 3900,
  length: 1,
  convRule: rule4
}, {
  start: 3901,
  length: 1,
  convRule: rule5
}, {
  start: 3902,
  length: 2,
  convRule: rule124
}, {
  start: 3904,
  length: 8,
  convRule: rule14
}, {
  start: 3913,
  length: 36,
  convRule: rule14
}, {
  start: 3953,
  length: 14,
  convRule: rule92
}, {
  start: 3967,
  length: 1,
  convRule: rule124
}, {
  start: 3968,
  length: 5,
  convRule: rule92
}, {
  start: 3973,
  length: 1,
  convRule: rule2
}, {
  start: 3974,
  length: 2,
  convRule: rule92
}, {
  start: 3976,
  length: 5,
  convRule: rule14
}, {
  start: 3981,
  length: 11,
  convRule: rule92
}, {
  start: 3993,
  length: 36,
  convRule: rule92
}, {
  start: 4030,
  length: 8,
  convRule: rule13
}, {
  start: 4038,
  length: 1,
  convRule: rule92
}, {
  start: 4039,
  length: 6,
  convRule: rule13
}, {
  start: 4046,
  length: 2,
  convRule: rule13
}, {
  start: 4048,
  length: 5,
  convRule: rule2
}, {
  start: 4053,
  length: 4,
  convRule: rule13
}, {
  start: 4057,
  length: 2,
  convRule: rule2
}, {
  start: 4096,
  length: 43,
  convRule: rule14
}, {
  start: 4139,
  length: 2,
  convRule: rule124
}, {
  start: 4141,
  length: 4,
  convRule: rule92
}, {
  start: 4145,
  length: 1,
  convRule: rule124
}, {
  start: 4146,
  length: 6,
  convRule: rule92
}, {
  start: 4152,
  length: 1,
  convRule: rule124
}, {
  start: 4153,
  length: 2,
  convRule: rule92
}, {
  start: 4155,
  length: 2,
  convRule: rule124
}, {
  start: 4157,
  length: 2,
  convRule: rule92
}, {
  start: 4159,
  length: 1,
  convRule: rule14
}, {
  start: 4160,
  length: 10,
  convRule: rule8
}, {
  start: 4170,
  length: 6,
  convRule: rule2
}, {
  start: 4176,
  length: 6,
  convRule: rule14
}, {
  start: 4182,
  length: 2,
  convRule: rule124
}, {
  start: 4184,
  length: 2,
  convRule: rule92
}, {
  start: 4186,
  length: 4,
  convRule: rule14
}, {
  start: 4190,
  length: 3,
  convRule: rule92
}, {
  start: 4193,
  length: 1,
  convRule: rule14
}, {
  start: 4194,
  length: 3,
  convRule: rule124
}, {
  start: 4197,
  length: 2,
  convRule: rule14
}, {
  start: 4199,
  length: 7,
  convRule: rule124
}, {
  start: 4206,
  length: 3,
  convRule: rule14
}, {
  start: 4209,
  length: 4,
  convRule: rule92
}, {
  start: 4213,
  length: 13,
  convRule: rule14
}, {
  start: 4226,
  length: 1,
  convRule: rule92
}, {
  start: 4227,
  length: 2,
  convRule: rule124
}, {
  start: 4229,
  length: 2,
  convRule: rule92
}, {
  start: 4231,
  length: 6,
  convRule: rule124
}, {
  start: 4237,
  length: 1,
  convRule: rule92
}, {
  start: 4238,
  length: 1,
  convRule: rule14
}, {
  start: 4239,
  length: 1,
  convRule: rule124
}, {
  start: 4240,
  length: 10,
  convRule: rule8
}, {
  start: 4250,
  length: 3,
  convRule: rule124
}, {
  start: 4253,
  length: 1,
  convRule: rule92
}, {
  start: 4254,
  length: 2,
  convRule: rule13
}, {
  start: 4256,
  length: 38,
  convRule: rule125
}, {
  start: 4295,
  length: 1,
  convRule: rule125
}, {
  start: 4301,
  length: 1,
  convRule: rule125
}, {
  start: 4304,
  length: 43,
  convRule: rule126
}, {
  start: 4347,
  length: 1,
  convRule: rule2
}, {
  start: 4348,
  length: 1,
  convRule: rule91
}, {
  start: 4349,
  length: 3,
  convRule: rule126
}, {
  start: 4352,
  length: 329,
  convRule: rule14
}, {
  start: 4682,
  length: 4,
  convRule: rule14
}, {
  start: 4688,
  length: 7,
  convRule: rule14
}, {
  start: 4696,
  length: 1,
  convRule: rule14
}, {
  start: 4698,
  length: 4,
  convRule: rule14
}, {
  start: 4704,
  length: 41,
  convRule: rule14
}, {
  start: 4746,
  length: 4,
  convRule: rule14
}, {
  start: 4752,
  length: 33,
  convRule: rule14
}, {
  start: 4786,
  length: 4,
  convRule: rule14
}, {
  start: 4792,
  length: 7,
  convRule: rule14
}, {
  start: 4800,
  length: 1,
  convRule: rule14
}, {
  start: 4802,
  length: 4,
  convRule: rule14
}, {
  start: 4808,
  length: 15,
  convRule: rule14
}, {
  start: 4824,
  length: 57,
  convRule: rule14
}, {
  start: 4882,
  length: 4,
  convRule: rule14
}, {
  start: 4888,
  length: 67,
  convRule: rule14
}, {
  start: 4957,
  length: 3,
  convRule: rule92
}, {
  start: 4960,
  length: 9,
  convRule: rule2
}, {
  start: 4969,
  length: 20,
  convRule: rule17
}, {
  start: 4992,
  length: 16,
  convRule: rule14
}, {
  start: 5008,
  length: 10,
  convRule: rule13
}, {
  start: 5024,
  length: 80,
  convRule: rule127
}, {
  start: 5104,
  length: 6,
  convRule: rule104
}, {
  start: 5112,
  length: 6,
  convRule: rule110
}, {
  start: 5120,
  length: 1,
  convRule: rule7
}, {
  start: 5121,
  length: 620,
  convRule: rule14
}, {
  start: 5741,
  length: 1,
  convRule: rule13
}, {
  start: 5742,
  length: 1,
  convRule: rule2
}, {
  start: 5743,
  length: 17,
  convRule: rule14
}, {
  start: 5760,
  length: 1,
  convRule: rule1
}, {
  start: 5761,
  length: 26,
  convRule: rule14
}, {
  start: 5787,
  length: 1,
  convRule: rule4
}, {
  start: 5788,
  length: 1,
  convRule: rule5
}, {
  start: 5792,
  length: 75,
  convRule: rule14
}, {
  start: 5867,
  length: 3,
  convRule: rule2
}, {
  start: 5870,
  length: 3,
  convRule: rule128
}, {
  start: 5873,
  length: 8,
  convRule: rule14
}, {
  start: 5888,
  length: 13,
  convRule: rule14
}, {
  start: 5902,
  length: 4,
  convRule: rule14
}, {
  start: 5906,
  length: 3,
  convRule: rule92
}, {
  start: 5920,
  length: 18,
  convRule: rule14
}, {
  start: 5938,
  length: 3,
  convRule: rule92
}, {
  start: 5941,
  length: 2,
  convRule: rule2
}, {
  start: 5952,
  length: 18,
  convRule: rule14
}, {
  start: 5970,
  length: 2,
  convRule: rule92
}, {
  start: 5984,
  length: 13,
  convRule: rule14
}, {
  start: 5998,
  length: 3,
  convRule: rule14
}, {
  start: 6002,
  length: 2,
  convRule: rule92
}, {
  start: 6016,
  length: 52,
  convRule: rule14
}, {
  start: 6068,
  length: 2,
  convRule: rule92
}, {
  start: 6070,
  length: 1,
  convRule: rule124
}, {
  start: 6071,
  length: 7,
  convRule: rule92
}, {
  start: 6078,
  length: 8,
  convRule: rule124
}, {
  start: 6086,
  length: 1,
  convRule: rule92
}, {
  start: 6087,
  length: 2,
  convRule: rule124
}, {
  start: 6089,
  length: 11,
  convRule: rule92
}, {
  start: 6100,
  length: 3,
  convRule: rule2
}, {
  start: 6103,
  length: 1,
  convRule: rule91
}, {
  start: 6104,
  length: 3,
  convRule: rule2
}, {
  start: 6107,
  length: 1,
  convRule: rule3
}, {
  start: 6108,
  length: 1,
  convRule: rule14
}, {
  start: 6109,
  length: 1,
  convRule: rule92
}, {
  start: 6112,
  length: 10,
  convRule: rule8
}, {
  start: 6128,
  length: 10,
  convRule: rule17
}, {
  start: 6144,
  length: 6,
  convRule: rule2
}, {
  start: 6150,
  length: 1,
  convRule: rule7
}, {
  start: 6151,
  length: 4,
  convRule: rule2
}, {
  start: 6155,
  length: 3,
  convRule: rule92
}, {
  start: 6158,
  length: 1,
  convRule: rule16
}, {
  start: 6160,
  length: 10,
  convRule: rule8
}, {
  start: 6176,
  length: 35,
  convRule: rule14
}, {
  start: 6211,
  length: 1,
  convRule: rule91
}, {
  start: 6212,
  length: 53,
  convRule: rule14
}, {
  start: 6272,
  length: 5,
  convRule: rule14
}, {
  start: 6277,
  length: 2,
  convRule: rule92
}, {
  start: 6279,
  length: 34,
  convRule: rule14
}, {
  start: 6313,
  length: 1,
  convRule: rule92
}, {
  start: 6314,
  length: 1,
  convRule: rule14
}, {
  start: 6320,
  length: 70,
  convRule: rule14
}, {
  start: 6400,
  length: 31,
  convRule: rule14
}, {
  start: 6432,
  length: 3,
  convRule: rule92
}, {
  start: 6435,
  length: 4,
  convRule: rule124
}, {
  start: 6439,
  length: 2,
  convRule: rule92
}, {
  start: 6441,
  length: 3,
  convRule: rule124
}, {
  start: 6448,
  length: 2,
  convRule: rule124
}, {
  start: 6450,
  length: 1,
  convRule: rule92
}, {
  start: 6451,
  length: 6,
  convRule: rule124
}, {
  start: 6457,
  length: 3,
  convRule: rule92
}, {
  start: 6464,
  length: 1,
  convRule: rule13
}, {
  start: 6468,
  length: 2,
  convRule: rule2
}, {
  start: 6470,
  length: 10,
  convRule: rule8
}, {
  start: 6480,
  length: 30,
  convRule: rule14
}, {
  start: 6512,
  length: 5,
  convRule: rule14
}, {
  start: 6528,
  length: 44,
  convRule: rule14
}, {
  start: 6576,
  length: 26,
  convRule: rule14
}, {
  start: 6608,
  length: 10,
  convRule: rule8
}, {
  start: 6618,
  length: 1,
  convRule: rule17
}, {
  start: 6622,
  length: 34,
  convRule: rule13
}, {
  start: 6656,
  length: 23,
  convRule: rule14
}, {
  start: 6679,
  length: 2,
  convRule: rule92
}, {
  start: 6681,
  length: 2,
  convRule: rule124
}, {
  start: 6683,
  length: 1,
  convRule: rule92
}, {
  start: 6686,
  length: 2,
  convRule: rule2
}, {
  start: 6688,
  length: 53,
  convRule: rule14
}, {
  start: 6741,
  length: 1,
  convRule: rule124
}, {
  start: 6742,
  length: 1,
  convRule: rule92
}, {
  start: 6743,
  length: 1,
  convRule: rule124
}, {
  start: 6744,
  length: 7,
  convRule: rule92
}, {
  start: 6752,
  length: 1,
  convRule: rule92
}, {
  start: 6753,
  length: 1,
  convRule: rule124
}, {
  start: 6754,
  length: 1,
  convRule: rule92
}, {
  start: 6755,
  length: 2,
  convRule: rule124
}, {
  start: 6757,
  length: 8,
  convRule: rule92
}, {
  start: 6765,
  length: 6,
  convRule: rule124
}, {
  start: 6771,
  length: 10,
  convRule: rule92
}, {
  start: 6783,
  length: 1,
  convRule: rule92
}, {
  start: 6784,
  length: 10,
  convRule: rule8
}, {
  start: 6800,
  length: 10,
  convRule: rule8
}, {
  start: 6816,
  length: 7,
  convRule: rule2
}, {
  start: 6823,
  length: 1,
  convRule: rule91
}, {
  start: 6824,
  length: 6,
  convRule: rule2
}, {
  start: 6832,
  length: 14,
  convRule: rule92
}, {
  start: 6846,
  length: 1,
  convRule: rule119
}, {
  start: 6847,
  length: 2,
  convRule: rule92
}, {
  start: 6912,
  length: 4,
  convRule: rule92
}, {
  start: 6916,
  length: 1,
  convRule: rule124
}, {
  start: 6917,
  length: 47,
  convRule: rule14
}, {
  start: 6964,
  length: 1,
  convRule: rule92
}, {
  start: 6965,
  length: 1,
  convRule: rule124
}, {
  start: 6966,
  length: 5,
  convRule: rule92
}, {
  start: 6971,
  length: 1,
  convRule: rule124
}, {
  start: 6972,
  length: 1,
  convRule: rule92
}, {
  start: 6973,
  length: 5,
  convRule: rule124
}, {
  start: 6978,
  length: 1,
  convRule: rule92
}, {
  start: 6979,
  length: 2,
  convRule: rule124
}, {
  start: 6981,
  length: 7,
  convRule: rule14
}, {
  start: 6992,
  length: 10,
  convRule: rule8
}, {
  start: 7002,
  length: 7,
  convRule: rule2
}, {
  start: 7009,
  length: 10,
  convRule: rule13
}, {
  start: 7019,
  length: 9,
  convRule: rule92
}, {
  start: 7028,
  length: 9,
  convRule: rule13
}, {
  start: 7040,
  length: 2,
  convRule: rule92
}, {
  start: 7042,
  length: 1,
  convRule: rule124
}, {
  start: 7043,
  length: 30,
  convRule: rule14
}, {
  start: 7073,
  length: 1,
  convRule: rule124
}, {
  start: 7074,
  length: 4,
  convRule: rule92
}, {
  start: 7078,
  length: 2,
  convRule: rule124
}, {
  start: 7080,
  length: 2,
  convRule: rule92
}, {
  start: 7082,
  length: 1,
  convRule: rule124
}, {
  start: 7083,
  length: 3,
  convRule: rule92
}, {
  start: 7086,
  length: 2,
  convRule: rule14
}, {
  start: 7088,
  length: 10,
  convRule: rule8
}, {
  start: 7098,
  length: 44,
  convRule: rule14
}, {
  start: 7142,
  length: 1,
  convRule: rule92
}, {
  start: 7143,
  length: 1,
  convRule: rule124
}, {
  start: 7144,
  length: 2,
  convRule: rule92
}, {
  start: 7146,
  length: 3,
  convRule: rule124
}, {
  start: 7149,
  length: 1,
  convRule: rule92
}, {
  start: 7150,
  length: 1,
  convRule: rule124
}, {
  start: 7151,
  length: 3,
  convRule: rule92
}, {
  start: 7154,
  length: 2,
  convRule: rule124
}, {
  start: 7164,
  length: 4,
  convRule: rule2
}, {
  start: 7168,
  length: 36,
  convRule: rule14
}, {
  start: 7204,
  length: 8,
  convRule: rule124
}, {
  start: 7212,
  length: 8,
  convRule: rule92
}, {
  start: 7220,
  length: 2,
  convRule: rule124
}, {
  start: 7222,
  length: 2,
  convRule: rule92
}, {
  start: 7227,
  length: 5,
  convRule: rule2
}, {
  start: 7232,
  length: 10,
  convRule: rule8
}, {
  start: 7245,
  length: 3,
  convRule: rule14
}, {
  start: 7248,
  length: 10,
  convRule: rule8
}, {
  start: 7258,
  length: 30,
  convRule: rule14
}, {
  start: 7288,
  length: 6,
  convRule: rule91
}, {
  start: 7294,
  length: 2,
  convRule: rule2
}, {
  start: 7296,
  length: 1,
  convRule: rule129
}, {
  start: 7297,
  length: 1,
  convRule: rule130
}, {
  start: 7298,
  length: 1,
  convRule: rule131
}, {
  start: 7299,
  length: 2,
  convRule: rule132
}, {
  start: 7301,
  length: 1,
  convRule: rule133
}, {
  start: 7302,
  length: 1,
  convRule: rule134
}, {
  start: 7303,
  length: 1,
  convRule: rule135
}, {
  start: 7304,
  length: 1,
  convRule: rule136
}, {
  start: 7312,
  length: 43,
  convRule: rule137
}, {
  start: 7357,
  length: 3,
  convRule: rule137
}, {
  start: 7360,
  length: 8,
  convRule: rule2
}, {
  start: 7376,
  length: 3,
  convRule: rule92
}, {
  start: 7379,
  length: 1,
  convRule: rule2
}, {
  start: 7380,
  length: 13,
  convRule: rule92
}, {
  start: 7393,
  length: 1,
  convRule: rule124
}, {
  start: 7394,
  length: 7,
  convRule: rule92
}, {
  start: 7401,
  length: 4,
  convRule: rule14
}, {
  start: 7405,
  length: 1,
  convRule: rule92
}, {
  start: 7406,
  length: 6,
  convRule: rule14
}, {
  start: 7412,
  length: 1,
  convRule: rule92
}, {
  start: 7413,
  length: 2,
  convRule: rule14
}, {
  start: 7415,
  length: 1,
  convRule: rule124
}, {
  start: 7416,
  length: 2,
  convRule: rule92
}, {
  start: 7418,
  length: 1,
  convRule: rule14
}, {
  start: 7424,
  length: 44,
  convRule: rule20
}, {
  start: 7468,
  length: 63,
  convRule: rule91
}, {
  start: 7531,
  length: 13,
  convRule: rule20
}, {
  start: 7544,
  length: 1,
  convRule: rule91
}, {
  start: 7545,
  length: 1,
  convRule: rule138
}, {
  start: 7546,
  length: 3,
  convRule: rule20
}, {
  start: 7549,
  length: 1,
  convRule: rule139
}, {
  start: 7550,
  length: 16,
  convRule: rule20
}, {
  start: 7566,
  length: 1,
  convRule: rule140
}, {
  start: 7567,
  length: 12,
  convRule: rule20
}, {
  start: 7579,
  length: 37,
  convRule: rule91
}, {
  start: 7616,
  length: 58,
  convRule: rule92
}, {
  start: 7675,
  length: 5,
  convRule: rule92
}, {
  start: 7680,
  length: 1,
  convRule: rule22
}, {
  start: 7681,
  length: 1,
  convRule: rule23
}, {
  start: 7682,
  length: 1,
  convRule: rule22
}, {
  start: 7683,
  length: 1,
  convRule: rule23
}, {
  start: 7684,
  length: 1,
  convRule: rule22
}, {
  start: 7685,
  length: 1,
  convRule: rule23
}, {
  start: 7686,
  length: 1,
  convRule: rule22
}, {
  start: 7687,
  length: 1,
  convRule: rule23
}, {
  start: 7688,
  length: 1,
  convRule: rule22
}, {
  start: 7689,
  length: 1,
  convRule: rule23
}, {
  start: 7690,
  length: 1,
  convRule: rule22
}, {
  start: 7691,
  length: 1,
  convRule: rule23
}, {
  start: 7692,
  length: 1,
  convRule: rule22
}, {
  start: 7693,
  length: 1,
  convRule: rule23
}, {
  start: 7694,
  length: 1,
  convRule: rule22
}, {
  start: 7695,
  length: 1,
  convRule: rule23
}, {
  start: 7696,
  length: 1,
  convRule: rule22
}, {
  start: 7697,
  length: 1,
  convRule: rule23
}, {
  start: 7698,
  length: 1,
  convRule: rule22
}, {
  start: 7699,
  length: 1,
  convRule: rule23
}, {
  start: 7700,
  length: 1,
  convRule: rule22
}, {
  start: 7701,
  length: 1,
  convRule: rule23
}, {
  start: 7702,
  length: 1,
  convRule: rule22
}, {
  start: 7703,
  length: 1,
  convRule: rule23
}, {
  start: 7704,
  length: 1,
  convRule: rule22
}, {
  start: 7705,
  length: 1,
  convRule: rule23
}, {
  start: 7706,
  length: 1,
  convRule: rule22
}, {
  start: 7707,
  length: 1,
  convRule: rule23
}, {
  start: 7708,
  length: 1,
  convRule: rule22
}, {
  start: 7709,
  length: 1,
  convRule: rule23
}, {
  start: 7710,
  length: 1,
  convRule: rule22
}, {
  start: 7711,
  length: 1,
  convRule: rule23
}, {
  start: 7712,
  length: 1,
  convRule: rule22
}, {
  start: 7713,
  length: 1,
  convRule: rule23
}, {
  start: 7714,
  length: 1,
  convRule: rule22
}, {
  start: 7715,
  length: 1,
  convRule: rule23
}, {
  start: 7716,
  length: 1,
  convRule: rule22
}, {
  start: 7717,
  length: 1,
  convRule: rule23
}, {
  start: 7718,
  length: 1,
  convRule: rule22
}, {
  start: 7719,
  length: 1,
  convRule: rule23
}, {
  start: 7720,
  length: 1,
  convRule: rule22
}, {
  start: 7721,
  length: 1,
  convRule: rule23
}, {
  start: 7722,
  length: 1,
  convRule: rule22
}, {
  start: 7723,
  length: 1,
  convRule: rule23
}, {
  start: 7724,
  length: 1,
  convRule: rule22
}, {
  start: 7725,
  length: 1,
  convRule: rule23
}, {
  start: 7726,
  length: 1,
  convRule: rule22
}, {
  start: 7727,
  length: 1,
  convRule: rule23
}, {
  start: 7728,
  length: 1,
  convRule: rule22
}, {
  start: 7729,
  length: 1,
  convRule: rule23
}, {
  start: 7730,
  length: 1,
  convRule: rule22
}, {
  start: 7731,
  length: 1,
  convRule: rule23
}, {
  start: 7732,
  length: 1,
  convRule: rule22
}, {
  start: 7733,
  length: 1,
  convRule: rule23
}, {
  start: 7734,
  length: 1,
  convRule: rule22
}, {
  start: 7735,
  length: 1,
  convRule: rule23
}, {
  start: 7736,
  length: 1,
  convRule: rule22
}, {
  start: 7737,
  length: 1,
  convRule: rule23
}, {
  start: 7738,
  length: 1,
  convRule: rule22
}, {
  start: 7739,
  length: 1,
  convRule: rule23
}, {
  start: 7740,
  length: 1,
  convRule: rule22
}, {
  start: 7741,
  length: 1,
  convRule: rule23
}, {
  start: 7742,
  length: 1,
  convRule: rule22
}, {
  start: 7743,
  length: 1,
  convRule: rule23
}, {
  start: 7744,
  length: 1,
  convRule: rule22
}, {
  start: 7745,
  length: 1,
  convRule: rule23
}, {
  start: 7746,
  length: 1,
  convRule: rule22
}, {
  start: 7747,
  length: 1,
  convRule: rule23
}, {
  start: 7748,
  length: 1,
  convRule: rule22
}, {
  start: 7749,
  length: 1,
  convRule: rule23
}, {
  start: 7750,
  length: 1,
  convRule: rule22
}, {
  start: 7751,
  length: 1,
  convRule: rule23
}, {
  start: 7752,
  length: 1,
  convRule: rule22
}, {
  start: 7753,
  length: 1,
  convRule: rule23
}, {
  start: 7754,
  length: 1,
  convRule: rule22
}, {
  start: 7755,
  length: 1,
  convRule: rule23
}, {
  start: 7756,
  length: 1,
  convRule: rule22
}, {
  start: 7757,
  length: 1,
  convRule: rule23
}, {
  start: 7758,
  length: 1,
  convRule: rule22
}, {
  start: 7759,
  length: 1,
  convRule: rule23
}, {
  start: 7760,
  length: 1,
  convRule: rule22
}, {
  start: 7761,
  length: 1,
  convRule: rule23
}, {
  start: 7762,
  length: 1,
  convRule: rule22
}, {
  start: 7763,
  length: 1,
  convRule: rule23
}, {
  start: 7764,
  length: 1,
  convRule: rule22
}, {
  start: 7765,
  length: 1,
  convRule: rule23
}, {
  start: 7766,
  length: 1,
  convRule: rule22
}, {
  start: 7767,
  length: 1,
  convRule: rule23
}, {
  start: 7768,
  length: 1,
  convRule: rule22
}, {
  start: 7769,
  length: 1,
  convRule: rule23
}, {
  start: 7770,
  length: 1,
  convRule: rule22
}, {
  start: 7771,
  length: 1,
  convRule: rule23
}, {
  start: 7772,
  length: 1,
  convRule: rule22
}, {
  start: 7773,
  length: 1,
  convRule: rule23
}, {
  start: 7774,
  length: 1,
  convRule: rule22
}, {
  start: 7775,
  length: 1,
  convRule: rule23
}, {
  start: 7776,
  length: 1,
  convRule: rule22
}, {
  start: 7777,
  length: 1,
  convRule: rule23
}, {
  start: 7778,
  length: 1,
  convRule: rule22
}, {
  start: 7779,
  length: 1,
  convRule: rule23
}, {
  start: 7780,
  length: 1,
  convRule: rule22
}, {
  start: 7781,
  length: 1,
  convRule: rule23
}, {
  start: 7782,
  length: 1,
  convRule: rule22
}, {
  start: 7783,
  length: 1,
  convRule: rule23
}, {
  start: 7784,
  length: 1,
  convRule: rule22
}, {
  start: 7785,
  length: 1,
  convRule: rule23
}, {
  start: 7786,
  length: 1,
  convRule: rule22
}, {
  start: 7787,
  length: 1,
  convRule: rule23
}, {
  start: 7788,
  length: 1,
  convRule: rule22
}, {
  start: 7789,
  length: 1,
  convRule: rule23
}, {
  start: 7790,
  length: 1,
  convRule: rule22
}, {
  start: 7791,
  length: 1,
  convRule: rule23
}, {
  start: 7792,
  length: 1,
  convRule: rule22
}, {
  start: 7793,
  length: 1,
  convRule: rule23
}, {
  start: 7794,
  length: 1,
  convRule: rule22
}, {
  start: 7795,
  length: 1,
  convRule: rule23
}, {
  start: 7796,
  length: 1,
  convRule: rule22
}, {
  start: 7797,
  length: 1,
  convRule: rule23
}, {
  start: 7798,
  length: 1,
  convRule: rule22
}, {
  start: 7799,
  length: 1,
  convRule: rule23
}, {
  start: 7800,
  length: 1,
  convRule: rule22
}, {
  start: 7801,
  length: 1,
  convRule: rule23
}, {
  start: 7802,
  length: 1,
  convRule: rule22
}, {
  start: 7803,
  length: 1,
  convRule: rule23
}, {
  start: 7804,
  length: 1,
  convRule: rule22
}, {
  start: 7805,
  length: 1,
  convRule: rule23
}, {
  start: 7806,
  length: 1,
  convRule: rule22
}, {
  start: 7807,
  length: 1,
  convRule: rule23
}, {
  start: 7808,
  length: 1,
  convRule: rule22
}, {
  start: 7809,
  length: 1,
  convRule: rule23
}, {
  start: 7810,
  length: 1,
  convRule: rule22
}, {
  start: 7811,
  length: 1,
  convRule: rule23
}, {
  start: 7812,
  length: 1,
  convRule: rule22
}, {
  start: 7813,
  length: 1,
  convRule: rule23
}, {
  start: 7814,
  length: 1,
  convRule: rule22
}, {
  start: 7815,
  length: 1,
  convRule: rule23
}, {
  start: 7816,
  length: 1,
  convRule: rule22
}, {
  start: 7817,
  length: 1,
  convRule: rule23
}, {
  start: 7818,
  length: 1,
  convRule: rule22
}, {
  start: 7819,
  length: 1,
  convRule: rule23
}, {
  start: 7820,
  length: 1,
  convRule: rule22
}, {
  start: 7821,
  length: 1,
  convRule: rule23
}, {
  start: 7822,
  length: 1,
  convRule: rule22
}, {
  start: 7823,
  length: 1,
  convRule: rule23
}, {
  start: 7824,
  length: 1,
  convRule: rule22
}, {
  start: 7825,
  length: 1,
  convRule: rule23
}, {
  start: 7826,
  length: 1,
  convRule: rule22
}, {
  start: 7827,
  length: 1,
  convRule: rule23
}, {
  start: 7828,
  length: 1,
  convRule: rule22
}, {
  start: 7829,
  length: 1,
  convRule: rule23
}, {
  start: 7830,
  length: 5,
  convRule: rule20
}, {
  start: 7835,
  length: 1,
  convRule: rule141
}, {
  start: 7836,
  length: 2,
  convRule: rule20
}, {
  start: 7838,
  length: 1,
  convRule: rule142
}, {
  start: 7839,
  length: 1,
  convRule: rule20
}, {
  start: 7840,
  length: 1,
  convRule: rule22
}, {
  start: 7841,
  length: 1,
  convRule: rule23
}, {
  start: 7842,
  length: 1,
  convRule: rule22
}, {
  start: 7843,
  length: 1,
  convRule: rule23
}, {
  start: 7844,
  length: 1,
  convRule: rule22
}, {
  start: 7845,
  length: 1,
  convRule: rule23
}, {
  start: 7846,
  length: 1,
  convRule: rule22
}, {
  start: 7847,
  length: 1,
  convRule: rule23
}, {
  start: 7848,
  length: 1,
  convRule: rule22
}, {
  start: 7849,
  length: 1,
  convRule: rule23
}, {
  start: 7850,
  length: 1,
  convRule: rule22
}, {
  start: 7851,
  length: 1,
  convRule: rule23
}, {
  start: 7852,
  length: 1,
  convRule: rule22
}, {
  start: 7853,
  length: 1,
  convRule: rule23
}, {
  start: 7854,
  length: 1,
  convRule: rule22
}, {
  start: 7855,
  length: 1,
  convRule: rule23
}, {
  start: 7856,
  length: 1,
  convRule: rule22
}, {
  start: 7857,
  length: 1,
  convRule: rule23
}, {
  start: 7858,
  length: 1,
  convRule: rule22
}, {
  start: 7859,
  length: 1,
  convRule: rule23
}, {
  start: 7860,
  length: 1,
  convRule: rule22
}, {
  start: 7861,
  length: 1,
  convRule: rule23
}, {
  start: 7862,
  length: 1,
  convRule: rule22
}, {
  start: 7863,
  length: 1,
  convRule: rule23
}, {
  start: 7864,
  length: 1,
  convRule: rule22
}, {
  start: 7865,
  length: 1,
  convRule: rule23
}, {
  start: 7866,
  length: 1,
  convRule: rule22
}, {
  start: 7867,
  length: 1,
  convRule: rule23
}, {
  start: 7868,
  length: 1,
  convRule: rule22
}, {
  start: 7869,
  length: 1,
  convRule: rule23
}, {
  start: 7870,
  length: 1,
  convRule: rule22
}, {
  start: 7871,
  length: 1,
  convRule: rule23
}, {
  start: 7872,
  length: 1,
  convRule: rule22
}, {
  start: 7873,
  length: 1,
  convRule: rule23
}, {
  start: 7874,
  length: 1,
  convRule: rule22
}, {
  start: 7875,
  length: 1,
  convRule: rule23
}, {
  start: 7876,
  length: 1,
  convRule: rule22
}, {
  start: 7877,
  length: 1,
  convRule: rule23
}, {
  start: 7878,
  length: 1,
  convRule: rule22
}, {
  start: 7879,
  length: 1,
  convRule: rule23
}, {
  start: 7880,
  length: 1,
  convRule: rule22
}, {
  start: 7881,
  length: 1,
  convRule: rule23
}, {
  start: 7882,
  length: 1,
  convRule: rule22
}, {
  start: 7883,
  length: 1,
  convRule: rule23
}, {
  start: 7884,
  length: 1,
  convRule: rule22
}, {
  start: 7885,
  length: 1,
  convRule: rule23
}, {
  start: 7886,
  length: 1,
  convRule: rule22
}, {
  start: 7887,
  length: 1,
  convRule: rule23
}, {
  start: 7888,
  length: 1,
  convRule: rule22
}, {
  start: 7889,
  length: 1,
  convRule: rule23
}, {
  start: 7890,
  length: 1,
  convRule: rule22
}, {
  start: 7891,
  length: 1,
  convRule: rule23
}, {
  start: 7892,
  length: 1,
  convRule: rule22
}, {
  start: 7893,
  length: 1,
  convRule: rule23
}, {
  start: 7894,
  length: 1,
  convRule: rule22
}, {
  start: 7895,
  length: 1,
  convRule: rule23
}, {
  start: 7896,
  length: 1,
  convRule: rule22
}, {
  start: 7897,
  length: 1,
  convRule: rule23
}, {
  start: 7898,
  length: 1,
  convRule: rule22
}, {
  start: 7899,
  length: 1,
  convRule: rule23
}, {
  start: 7900,
  length: 1,
  convRule: rule22
}, {
  start: 7901,
  length: 1,
  convRule: rule23
}, {
  start: 7902,
  length: 1,
  convRule: rule22
}, {
  start: 7903,
  length: 1,
  convRule: rule23
}, {
  start: 7904,
  length: 1,
  convRule: rule22
}, {
  start: 7905,
  length: 1,
  convRule: rule23
}, {
  start: 7906,
  length: 1,
  convRule: rule22
}, {
  start: 7907,
  length: 1,
  convRule: rule23
}, {
  start: 7908,
  length: 1,
  convRule: rule22
}, {
  start: 7909,
  length: 1,
  convRule: rule23
}, {
  start: 7910,
  length: 1,
  convRule: rule22
}, {
  start: 7911,
  length: 1,
  convRule: rule23
}, {
  start: 7912,
  length: 1,
  convRule: rule22
}, {
  start: 7913,
  length: 1,
  convRule: rule23
}, {
  start: 7914,
  length: 1,
  convRule: rule22
}, {
  start: 7915,
  length: 1,
  convRule: rule23
}, {
  start: 7916,
  length: 1,
  convRule: rule22
}, {
  start: 7917,
  length: 1,
  convRule: rule23
}, {
  start: 7918,
  length: 1,
  convRule: rule22
}, {
  start: 7919,
  length: 1,
  convRule: rule23
}, {
  start: 7920,
  length: 1,
  convRule: rule22
}, {
  start: 7921,
  length: 1,
  convRule: rule23
}, {
  start: 7922,
  length: 1,
  convRule: rule22
}, {
  start: 7923,
  length: 1,
  convRule: rule23
}, {
  start: 7924,
  length: 1,
  convRule: rule22
}, {
  start: 7925,
  length: 1,
  convRule: rule23
}, {
  start: 7926,
  length: 1,
  convRule: rule22
}, {
  start: 7927,
  length: 1,
  convRule: rule23
}, {
  start: 7928,
  length: 1,
  convRule: rule22
}, {
  start: 7929,
  length: 1,
  convRule: rule23
}, {
  start: 7930,
  length: 1,
  convRule: rule22
}, {
  start: 7931,
  length: 1,
  convRule: rule23
}, {
  start: 7932,
  length: 1,
  convRule: rule22
}, {
  start: 7933,
  length: 1,
  convRule: rule23
}, {
  start: 7934,
  length: 1,
  convRule: rule22
}, {
  start: 7935,
  length: 1,
  convRule: rule23
}, {
  start: 7936,
  length: 8,
  convRule: rule143
}, {
  start: 7944,
  length: 8,
  convRule: rule144
}, {
  start: 7952,
  length: 6,
  convRule: rule143
}, {
  start: 7960,
  length: 6,
  convRule: rule144
}, {
  start: 7968,
  length: 8,
  convRule: rule143
}, {
  start: 7976,
  length: 8,
  convRule: rule144
}, {
  start: 7984,
  length: 8,
  convRule: rule143
}, {
  start: 7992,
  length: 8,
  convRule: rule144
}, {
  start: 8e3,
  length: 6,
  convRule: rule143
}, {
  start: 8008,
  length: 6,
  convRule: rule144
}, {
  start: 8016,
  length: 1,
  convRule: rule20
}, {
  start: 8017,
  length: 1,
  convRule: rule143
}, {
  start: 8018,
  length: 1,
  convRule: rule20
}, {
  start: 8019,
  length: 1,
  convRule: rule143
}, {
  start: 8020,
  length: 1,
  convRule: rule20
}, {
  start: 8021,
  length: 1,
  convRule: rule143
}, {
  start: 8022,
  length: 1,
  convRule: rule20
}, {
  start: 8023,
  length: 1,
  convRule: rule143
}, {
  start: 8025,
  length: 1,
  convRule: rule144
}, {
  start: 8027,
  length: 1,
  convRule: rule144
}, {
  start: 8029,
  length: 1,
  convRule: rule144
}, {
  start: 8031,
  length: 1,
  convRule: rule144
}, {
  start: 8032,
  length: 8,
  convRule: rule143
}, {
  start: 8040,
  length: 8,
  convRule: rule144
}, {
  start: 8048,
  length: 2,
  convRule: rule145
}, {
  start: 8050,
  length: 4,
  convRule: rule146
}, {
  start: 8054,
  length: 2,
  convRule: rule147
}, {
  start: 8056,
  length: 2,
  convRule: rule148
}, {
  start: 8058,
  length: 2,
  convRule: rule149
}, {
  start: 8060,
  length: 2,
  convRule: rule150
}, {
  start: 8064,
  length: 8,
  convRule: rule143
}, {
  start: 8072,
  length: 8,
  convRule: rule151
}, {
  start: 8080,
  length: 8,
  convRule: rule143
}, {
  start: 8088,
  length: 8,
  convRule: rule151
}, {
  start: 8096,
  length: 8,
  convRule: rule143
}, {
  start: 8104,
  length: 8,
  convRule: rule151
}, {
  start: 8112,
  length: 2,
  convRule: rule143
}, {
  start: 8114,
  length: 1,
  convRule: rule20
}, {
  start: 8115,
  length: 1,
  convRule: rule152
}, {
  start: 8116,
  length: 1,
  convRule: rule20
}, {
  start: 8118,
  length: 2,
  convRule: rule20
}, {
  start: 8120,
  length: 2,
  convRule: rule144
}, {
  start: 8122,
  length: 2,
  convRule: rule153
}, {
  start: 8124,
  length: 1,
  convRule: rule154
}, {
  start: 8125,
  length: 1,
  convRule: rule10
}, {
  start: 8126,
  length: 1,
  convRule: rule155
}, {
  start: 8127,
  length: 3,
  convRule: rule10
}, {
  start: 8130,
  length: 1,
  convRule: rule20
}, {
  start: 8131,
  length: 1,
  convRule: rule152
}, {
  start: 8132,
  length: 1,
  convRule: rule20
}, {
  start: 8134,
  length: 2,
  convRule: rule20
}, {
  start: 8136,
  length: 4,
  convRule: rule156
}, {
  start: 8140,
  length: 1,
  convRule: rule154
}, {
  start: 8141,
  length: 3,
  convRule: rule10
}, {
  start: 8144,
  length: 2,
  convRule: rule143
}, {
  start: 8146,
  length: 2,
  convRule: rule20
}, {
  start: 8150,
  length: 2,
  convRule: rule20
}, {
  start: 8152,
  length: 2,
  convRule: rule144
}, {
  start: 8154,
  length: 2,
  convRule: rule157
}, {
  start: 8157,
  length: 3,
  convRule: rule10
}, {
  start: 8160,
  length: 2,
  convRule: rule143
}, {
  start: 8162,
  length: 3,
  convRule: rule20
}, {
  start: 8165,
  length: 1,
  convRule: rule113
}, {
  start: 8166,
  length: 2,
  convRule: rule20
}, {
  start: 8168,
  length: 2,
  convRule: rule144
}, {
  start: 8170,
  length: 2,
  convRule: rule158
}, {
  start: 8172,
  length: 1,
  convRule: rule117
}, {
  start: 8173,
  length: 3,
  convRule: rule10
}, {
  start: 8178,
  length: 1,
  convRule: rule20
}, {
  start: 8179,
  length: 1,
  convRule: rule152
}, {
  start: 8180,
  length: 1,
  convRule: rule20
}, {
  start: 8182,
  length: 2,
  convRule: rule20
}, {
  start: 8184,
  length: 2,
  convRule: rule159
}, {
  start: 8186,
  length: 2,
  convRule: rule160
}, {
  start: 8188,
  length: 1,
  convRule: rule154
}, {
  start: 8189,
  length: 2,
  convRule: rule10
}, {
  start: 8192,
  length: 11,
  convRule: rule1
}, {
  start: 8203,
  length: 5,
  convRule: rule16
}, {
  start: 8208,
  length: 6,
  convRule: rule7
}, {
  start: 8214,
  length: 2,
  convRule: rule2
}, {
  start: 8216,
  length: 1,
  convRule: rule15
}, {
  start: 8217,
  length: 1,
  convRule: rule19
}, {
  start: 8218,
  length: 1,
  convRule: rule4
}, {
  start: 8219,
  length: 2,
  convRule: rule15
}, {
  start: 8221,
  length: 1,
  convRule: rule19
}, {
  start: 8222,
  length: 1,
  convRule: rule4
}, {
  start: 8223,
  length: 1,
  convRule: rule15
}, {
  start: 8224,
  length: 8,
  convRule: rule2
}, {
  start: 8232,
  length: 1,
  convRule: rule161
}, {
  start: 8233,
  length: 1,
  convRule: rule162
}, {
  start: 8234,
  length: 5,
  convRule: rule16
}, {
  start: 8239,
  length: 1,
  convRule: rule1
}, {
  start: 8240,
  length: 9,
  convRule: rule2
}, {
  start: 8249,
  length: 1,
  convRule: rule15
}, {
  start: 8250,
  length: 1,
  convRule: rule19
}, {
  start: 8251,
  length: 4,
  convRule: rule2
}, {
  start: 8255,
  length: 2,
  convRule: rule11
}, {
  start: 8257,
  length: 3,
  convRule: rule2
}, {
  start: 8260,
  length: 1,
  convRule: rule6
}, {
  start: 8261,
  length: 1,
  convRule: rule4
}, {
  start: 8262,
  length: 1,
  convRule: rule5
}, {
  start: 8263,
  length: 11,
  convRule: rule2
}, {
  start: 8274,
  length: 1,
  convRule: rule6
}, {
  start: 8275,
  length: 1,
  convRule: rule2
}, {
  start: 8276,
  length: 1,
  convRule: rule11
}, {
  start: 8277,
  length: 10,
  convRule: rule2
}, {
  start: 8287,
  length: 1,
  convRule: rule1
}, {
  start: 8288,
  length: 5,
  convRule: rule16
}, {
  start: 8294,
  length: 10,
  convRule: rule16
}, {
  start: 8304,
  length: 1,
  convRule: rule17
}, {
  start: 8305,
  length: 1,
  convRule: rule91
}, {
  start: 8308,
  length: 6,
  convRule: rule17
}, {
  start: 8314,
  length: 3,
  convRule: rule6
}, {
  start: 8317,
  length: 1,
  convRule: rule4
}, {
  start: 8318,
  length: 1,
  convRule: rule5
}, {
  start: 8319,
  length: 1,
  convRule: rule91
}, {
  start: 8320,
  length: 10,
  convRule: rule17
}, {
  start: 8330,
  length: 3,
  convRule: rule6
}, {
  start: 8333,
  length: 1,
  convRule: rule4
}, {
  start: 8334,
  length: 1,
  convRule: rule5
}, {
  start: 8336,
  length: 13,
  convRule: rule91
}, {
  start: 8352,
  length: 32,
  convRule: rule3
}, {
  start: 8400,
  length: 13,
  convRule: rule92
}, {
  start: 8413,
  length: 4,
  convRule: rule119
}, {
  start: 8417,
  length: 1,
  convRule: rule92
}, {
  start: 8418,
  length: 3,
  convRule: rule119
}, {
  start: 8421,
  length: 12,
  convRule: rule92
}, {
  start: 8448,
  length: 2,
  convRule: rule13
}, {
  start: 8450,
  length: 1,
  convRule: rule107
}, {
  start: 8451,
  length: 4,
  convRule: rule13
}, {
  start: 8455,
  length: 1,
  convRule: rule107
}, {
  start: 8456,
  length: 2,
  convRule: rule13
}, {
  start: 8458,
  length: 1,
  convRule: rule20
}, {
  start: 8459,
  length: 3,
  convRule: rule107
}, {
  start: 8462,
  length: 2,
  convRule: rule20
}, {
  start: 8464,
  length: 3,
  convRule: rule107
}, {
  start: 8467,
  length: 1,
  convRule: rule20
}, {
  start: 8468,
  length: 1,
  convRule: rule13
}, {
  start: 8469,
  length: 1,
  convRule: rule107
}, {
  start: 8470,
  length: 2,
  convRule: rule13
}, {
  start: 8472,
  length: 1,
  convRule: rule6
}, {
  start: 8473,
  length: 5,
  convRule: rule107
}, {
  start: 8478,
  length: 6,
  convRule: rule13
}, {
  start: 8484,
  length: 1,
  convRule: rule107
}, {
  start: 8485,
  length: 1,
  convRule: rule13
}, {
  start: 8486,
  length: 1,
  convRule: rule163
}, {
  start: 8487,
  length: 1,
  convRule: rule13
}, {
  start: 8488,
  length: 1,
  convRule: rule107
}, {
  start: 8489,
  length: 1,
  convRule: rule13
}, {
  start: 8490,
  length: 1,
  convRule: rule164
}, {
  start: 8491,
  length: 1,
  convRule: rule165
}, {
  start: 8492,
  length: 2,
  convRule: rule107
}, {
  start: 8494,
  length: 1,
  convRule: rule13
}, {
  start: 8495,
  length: 1,
  convRule: rule20
}, {
  start: 8496,
  length: 2,
  convRule: rule107
}, {
  start: 8498,
  length: 1,
  convRule: rule166
}, {
  start: 8499,
  length: 1,
  convRule: rule107
}, {
  start: 8500,
  length: 1,
  convRule: rule20
}, {
  start: 8501,
  length: 4,
  convRule: rule14
}, {
  start: 8505,
  length: 1,
  convRule: rule20
}, {
  start: 8506,
  length: 2,
  convRule: rule13
}, {
  start: 8508,
  length: 2,
  convRule: rule20
}, {
  start: 8510,
  length: 2,
  convRule: rule107
}, {
  start: 8512,
  length: 5,
  convRule: rule6
}, {
  start: 8517,
  length: 1,
  convRule: rule107
}, {
  start: 8518,
  length: 4,
  convRule: rule20
}, {
  start: 8522,
  length: 1,
  convRule: rule13
}, {
  start: 8523,
  length: 1,
  convRule: rule6
}, {
  start: 8524,
  length: 2,
  convRule: rule13
}, {
  start: 8526,
  length: 1,
  convRule: rule167
}, {
  start: 8527,
  length: 1,
  convRule: rule13
}, {
  start: 8528,
  length: 16,
  convRule: rule17
}, {
  start: 8544,
  length: 16,
  convRule: rule168
}, {
  start: 8560,
  length: 16,
  convRule: rule169
}, {
  start: 8576,
  length: 3,
  convRule: rule128
}, {
  start: 8579,
  length: 1,
  convRule: rule22
}, {
  start: 8580,
  length: 1,
  convRule: rule23
}, {
  start: 8581,
  length: 4,
  convRule: rule128
}, {
  start: 8585,
  length: 1,
  convRule: rule17
}, {
  start: 8586,
  length: 2,
  convRule: rule13
}, {
  start: 8592,
  length: 5,
  convRule: rule6
}, {
  start: 8597,
  length: 5,
  convRule: rule13
}, {
  start: 8602,
  length: 2,
  convRule: rule6
}, {
  start: 8604,
  length: 4,
  convRule: rule13
}, {
  start: 8608,
  length: 1,
  convRule: rule6
}, {
  start: 8609,
  length: 2,
  convRule: rule13
}, {
  start: 8611,
  length: 1,
  convRule: rule6
}, {
  start: 8612,
  length: 2,
  convRule: rule13
}, {
  start: 8614,
  length: 1,
  convRule: rule6
}, {
  start: 8615,
  length: 7,
  convRule: rule13
}, {
  start: 8622,
  length: 1,
  convRule: rule6
}, {
  start: 8623,
  length: 31,
  convRule: rule13
}, {
  start: 8654,
  length: 2,
  convRule: rule6
}, {
  start: 8656,
  length: 2,
  convRule: rule13
}, {
  start: 8658,
  length: 1,
  convRule: rule6
}, {
  start: 8659,
  length: 1,
  convRule: rule13
}, {
  start: 8660,
  length: 1,
  convRule: rule6
}, {
  start: 8661,
  length: 31,
  convRule: rule13
}, {
  start: 8692,
  length: 268,
  convRule: rule6
}, {
  start: 8960,
  length: 8,
  convRule: rule13
}, {
  start: 8968,
  length: 1,
  convRule: rule4
}, {
  start: 8969,
  length: 1,
  convRule: rule5
}, {
  start: 8970,
  length: 1,
  convRule: rule4
}, {
  start: 8971,
  length: 1,
  convRule: rule5
}, {
  start: 8972,
  length: 20,
  convRule: rule13
}, {
  start: 8992,
  length: 2,
  convRule: rule6
}, {
  start: 8994,
  length: 7,
  convRule: rule13
}, {
  start: 9001,
  length: 1,
  convRule: rule4
}, {
  start: 9002,
  length: 1,
  convRule: rule5
}, {
  start: 9003,
  length: 81,
  convRule: rule13
}, {
  start: 9084,
  length: 1,
  convRule: rule6
}, {
  start: 9085,
  length: 30,
  convRule: rule13
}, {
  start: 9115,
  length: 25,
  convRule: rule6
}, {
  start: 9140,
  length: 40,
  convRule: rule13
}, {
  start: 9180,
  length: 6,
  convRule: rule6
}, {
  start: 9186,
  length: 69,
  convRule: rule13
}, {
  start: 9280,
  length: 11,
  convRule: rule13
}, {
  start: 9312,
  length: 60,
  convRule: rule17
}, {
  start: 9372,
  length: 26,
  convRule: rule13
}, {
  start: 9398,
  length: 26,
  convRule: rule170
}, {
  start: 9424,
  length: 26,
  convRule: rule171
}, {
  start: 9450,
  length: 22,
  convRule: rule17
}, {
  start: 9472,
  length: 183,
  convRule: rule13
}, {
  start: 9655,
  length: 1,
  convRule: rule6
}, {
  start: 9656,
  length: 9,
  convRule: rule13
}, {
  start: 9665,
  length: 1,
  convRule: rule6
}, {
  start: 9666,
  length: 54,
  convRule: rule13
}, {
  start: 9720,
  length: 8,
  convRule: rule6
}, {
  start: 9728,
  length: 111,
  convRule: rule13
}, {
  start: 9839,
  length: 1,
  convRule: rule6
}, {
  start: 9840,
  length: 248,
  convRule: rule13
}, {
  start: 10088,
  length: 1,
  convRule: rule4
}, {
  start: 10089,
  length: 1,
  convRule: rule5
}, {
  start: 10090,
  length: 1,
  convRule: rule4
}, {
  start: 10091,
  length: 1,
  convRule: rule5
}, {
  start: 10092,
  length: 1,
  convRule: rule4
}, {
  start: 10093,
  length: 1,
  convRule: rule5
}, {
  start: 10094,
  length: 1,
  convRule: rule4
}, {
  start: 10095,
  length: 1,
  convRule: rule5
}, {
  start: 10096,
  length: 1,
  convRule: rule4
}, {
  start: 10097,
  length: 1,
  convRule: rule5
}, {
  start: 10098,
  length: 1,
  convRule: rule4
}, {
  start: 10099,
  length: 1,
  convRule: rule5
}, {
  start: 10100,
  length: 1,
  convRule: rule4
}, {
  start: 10101,
  length: 1,
  convRule: rule5
}, {
  start: 10102,
  length: 30,
  convRule: rule17
}, {
  start: 10132,
  length: 44,
  convRule: rule13
}, {
  start: 10176,
  length: 5,
  convRule: rule6
}, {
  start: 10181,
  length: 1,
  convRule: rule4
}, {
  start: 10182,
  length: 1,
  convRule: rule5
}, {
  start: 10183,
  length: 31,
  convRule: rule6
}, {
  start: 10214,
  length: 1,
  convRule: rule4
}, {
  start: 10215,
  length: 1,
  convRule: rule5
}, {
  start: 10216,
  length: 1,
  convRule: rule4
}, {
  start: 10217,
  length: 1,
  convRule: rule5
}, {
  start: 10218,
  length: 1,
  convRule: rule4
}, {
  start: 10219,
  length: 1,
  convRule: rule5
}, {
  start: 10220,
  length: 1,
  convRule: rule4
}, {
  start: 10221,
  length: 1,
  convRule: rule5
}, {
  start: 10222,
  length: 1,
  convRule: rule4
}, {
  start: 10223,
  length: 1,
  convRule: rule5
}, {
  start: 10224,
  length: 16,
  convRule: rule6
}, {
  start: 10240,
  length: 256,
  convRule: rule13
}, {
  start: 10496,
  length: 131,
  convRule: rule6
}, {
  start: 10627,
  length: 1,
  convRule: rule4
}, {
  start: 10628,
  length: 1,
  convRule: rule5
}, {
  start: 10629,
  length: 1,
  convRule: rule4
}, {
  start: 10630,
  length: 1,
  convRule: rule5
}, {
  start: 10631,
  length: 1,
  convRule: rule4
}, {
  start: 10632,
  length: 1,
  convRule: rule5
}, {
  start: 10633,
  length: 1,
  convRule: rule4
}, {
  start: 10634,
  length: 1,
  convRule: rule5
}, {
  start: 10635,
  length: 1,
  convRule: rule4
}, {
  start: 10636,
  length: 1,
  convRule: rule5
}, {
  start: 10637,
  length: 1,
  convRule: rule4
}, {
  start: 10638,
  length: 1,
  convRule: rule5
}, {
  start: 10639,
  length: 1,
  convRule: rule4
}, {
  start: 10640,
  length: 1,
  convRule: rule5
}, {
  start: 10641,
  length: 1,
  convRule: rule4
}, {
  start: 10642,
  length: 1,
  convRule: rule5
}, {
  start: 10643,
  length: 1,
  convRule: rule4
}, {
  start: 10644,
  length: 1,
  convRule: rule5
}, {
  start: 10645,
  length: 1,
  convRule: rule4
}, {
  start: 10646,
  length: 1,
  convRule: rule5
}, {
  start: 10647,
  length: 1,
  convRule: rule4
}, {
  start: 10648,
  length: 1,
  convRule: rule5
}, {
  start: 10649,
  length: 63,
  convRule: rule6
}, {
  start: 10712,
  length: 1,
  convRule: rule4
}, {
  start: 10713,
  length: 1,
  convRule: rule5
}, {
  start: 10714,
  length: 1,
  convRule: rule4
}, {
  start: 10715,
  length: 1,
  convRule: rule5
}, {
  start: 10716,
  length: 32,
  convRule: rule6
}, {
  start: 10748,
  length: 1,
  convRule: rule4
}, {
  start: 10749,
  length: 1,
  convRule: rule5
}, {
  start: 10750,
  length: 258,
  convRule: rule6
}, {
  start: 11008,
  length: 48,
  convRule: rule13
}, {
  start: 11056,
  length: 21,
  convRule: rule6
}, {
  start: 11077,
  length: 2,
  convRule: rule13
}, {
  start: 11079,
  length: 6,
  convRule: rule6
}, {
  start: 11085,
  length: 39,
  convRule: rule13
}, {
  start: 11126,
  length: 32,
  convRule: rule13
}, {
  start: 11159,
  length: 105,
  convRule: rule13
}, {
  start: 11264,
  length: 47,
  convRule: rule122
}, {
  start: 11312,
  length: 47,
  convRule: rule123
}, {
  start: 11360,
  length: 1,
  convRule: rule22
}, {
  start: 11361,
  length: 1,
  convRule: rule23
}, {
  start: 11362,
  length: 1,
  convRule: rule172
}, {
  start: 11363,
  length: 1,
  convRule: rule173
}, {
  start: 11364,
  length: 1,
  convRule: rule174
}, {
  start: 11365,
  length: 1,
  convRule: rule175
}, {
  start: 11366,
  length: 1,
  convRule: rule176
}, {
  start: 11367,
  length: 1,
  convRule: rule22
}, {
  start: 11368,
  length: 1,
  convRule: rule23
}, {
  start: 11369,
  length: 1,
  convRule: rule22
}, {
  start: 11370,
  length: 1,
  convRule: rule23
}, {
  start: 11371,
  length: 1,
  convRule: rule22
}, {
  start: 11372,
  length: 1,
  convRule: rule23
}, {
  start: 11373,
  length: 1,
  convRule: rule177
}, {
  start: 11374,
  length: 1,
  convRule: rule178
}, {
  start: 11375,
  length: 1,
  convRule: rule179
}, {
  start: 11376,
  length: 1,
  convRule: rule180
}, {
  start: 11377,
  length: 1,
  convRule: rule20
}, {
  start: 11378,
  length: 1,
  convRule: rule22
}, {
  start: 11379,
  length: 1,
  convRule: rule23
}, {
  start: 11380,
  length: 1,
  convRule: rule20
}, {
  start: 11381,
  length: 1,
  convRule: rule22
}, {
  start: 11382,
  length: 1,
  convRule: rule23
}, {
  start: 11383,
  length: 5,
  convRule: rule20
}, {
  start: 11388,
  length: 2,
  convRule: rule91
}, {
  start: 11390,
  length: 2,
  convRule: rule181
}, {
  start: 11392,
  length: 1,
  convRule: rule22
}, {
  start: 11393,
  length: 1,
  convRule: rule23
}, {
  start: 11394,
  length: 1,
  convRule: rule22
}, {
  start: 11395,
  length: 1,
  convRule: rule23
}, {
  start: 11396,
  length: 1,
  convRule: rule22
}, {
  start: 11397,
  length: 1,
  convRule: rule23
}, {
  start: 11398,
  length: 1,
  convRule: rule22
}, {
  start: 11399,
  length: 1,
  convRule: rule23
}, {
  start: 11400,
  length: 1,
  convRule: rule22
}, {
  start: 11401,
  length: 1,
  convRule: rule23
}, {
  start: 11402,
  length: 1,
  convRule: rule22
}, {
  start: 11403,
  length: 1,
  convRule: rule23
}, {
  start: 11404,
  length: 1,
  convRule: rule22
}, {
  start: 11405,
  length: 1,
  convRule: rule23
}, {
  start: 11406,
  length: 1,
  convRule: rule22
}, {
  start: 11407,
  length: 1,
  convRule: rule23
}, {
  start: 11408,
  length: 1,
  convRule: rule22
}, {
  start: 11409,
  length: 1,
  convRule: rule23
}, {
  start: 11410,
  length: 1,
  convRule: rule22
}, {
  start: 11411,
  length: 1,
  convRule: rule23
}, {
  start: 11412,
  length: 1,
  convRule: rule22
}, {
  start: 11413,
  length: 1,
  convRule: rule23
}, {
  start: 11414,
  length: 1,
  convRule: rule22
}, {
  start: 11415,
  length: 1,
  convRule: rule23
}, {
  start: 11416,
  length: 1,
  convRule: rule22
}, {
  start: 11417,
  length: 1,
  convRule: rule23
}, {
  start: 11418,
  length: 1,
  convRule: rule22
}, {
  start: 11419,
  length: 1,
  convRule: rule23
}, {
  start: 11420,
  length: 1,
  convRule: rule22
}, {
  start: 11421,
  length: 1,
  convRule: rule23
}, {
  start: 11422,
  length: 1,
  convRule: rule22
}, {
  start: 11423,
  length: 1,
  convRule: rule23
}, {
  start: 11424,
  length: 1,
  convRule: rule22
}, {
  start: 11425,
  length: 1,
  convRule: rule23
}, {
  start: 11426,
  length: 1,
  convRule: rule22
}, {
  start: 11427,
  length: 1,
  convRule: rule23
}, {
  start: 11428,
  length: 1,
  convRule: rule22
}, {
  start: 11429,
  length: 1,
  convRule: rule23
}, {
  start: 11430,
  length: 1,
  convRule: rule22
}, {
  start: 11431,
  length: 1,
  convRule: rule23
}, {
  start: 11432,
  length: 1,
  convRule: rule22
}, {
  start: 11433,
  length: 1,
  convRule: rule23
}, {
  start: 11434,
  length: 1,
  convRule: rule22
}, {
  start: 11435,
  length: 1,
  convRule: rule23
}, {
  start: 11436,
  length: 1,
  convRule: rule22
}, {
  start: 11437,
  length: 1,
  convRule: rule23
}, {
  start: 11438,
  length: 1,
  convRule: rule22
}, {
  start: 11439,
  length: 1,
  convRule: rule23
}, {
  start: 11440,
  length: 1,
  convRule: rule22
}, {
  start: 11441,
  length: 1,
  convRule: rule23
}, {
  start: 11442,
  length: 1,
  convRule: rule22
}, {
  start: 11443,
  length: 1,
  convRule: rule23
}, {
  start: 11444,
  length: 1,
  convRule: rule22
}, {
  start: 11445,
  length: 1,
  convRule: rule23
}, {
  start: 11446,
  length: 1,
  convRule: rule22
}, {
  start: 11447,
  length: 1,
  convRule: rule23
}, {
  start: 11448,
  length: 1,
  convRule: rule22
}, {
  start: 11449,
  length: 1,
  convRule: rule23
}, {
  start: 11450,
  length: 1,
  convRule: rule22
}, {
  start: 11451,
  length: 1,
  convRule: rule23
}, {
  start: 11452,
  length: 1,
  convRule: rule22
}, {
  start: 11453,
  length: 1,
  convRule: rule23
}, {
  start: 11454,
  length: 1,
  convRule: rule22
}, {
  start: 11455,
  length: 1,
  convRule: rule23
}, {
  start: 11456,
  length: 1,
  convRule: rule22
}, {
  start: 11457,
  length: 1,
  convRule: rule23
}, {
  start: 11458,
  length: 1,
  convRule: rule22
}, {
  start: 11459,
  length: 1,
  convRule: rule23
}, {
  start: 11460,
  length: 1,
  convRule: rule22
}, {
  start: 11461,
  length: 1,
  convRule: rule23
}, {
  start: 11462,
  length: 1,
  convRule: rule22
}, {
  start: 11463,
  length: 1,
  convRule: rule23
}, {
  start: 11464,
  length: 1,
  convRule: rule22
}, {
  start: 11465,
  length: 1,
  convRule: rule23
}, {
  start: 11466,
  length: 1,
  convRule: rule22
}, {
  start: 11467,
  length: 1,
  convRule: rule23
}, {
  start: 11468,
  length: 1,
  convRule: rule22
}, {
  start: 11469,
  length: 1,
  convRule: rule23
}, {
  start: 11470,
  length: 1,
  convRule: rule22
}, {
  start: 11471,
  length: 1,
  convRule: rule23
}, {
  start: 11472,
  length: 1,
  convRule: rule22
}, {
  start: 11473,
  length: 1,
  convRule: rule23
}, {
  start: 11474,
  length: 1,
  convRule: rule22
}, {
  start: 11475,
  length: 1,
  convRule: rule23
}, {
  start: 11476,
  length: 1,
  convRule: rule22
}, {
  start: 11477,
  length: 1,
  convRule: rule23
}, {
  start: 11478,
  length: 1,
  convRule: rule22
}, {
  start: 11479,
  length: 1,
  convRule: rule23
}, {
  start: 11480,
  length: 1,
  convRule: rule22
}, {
  start: 11481,
  length: 1,
  convRule: rule23
}, {
  start: 11482,
  length: 1,
  convRule: rule22
}, {
  start: 11483,
  length: 1,
  convRule: rule23
}, {
  start: 11484,
  length: 1,
  convRule: rule22
}, {
  start: 11485,
  length: 1,
  convRule: rule23
}, {
  start: 11486,
  length: 1,
  convRule: rule22
}, {
  start: 11487,
  length: 1,
  convRule: rule23
}, {
  start: 11488,
  length: 1,
  convRule: rule22
}, {
  start: 11489,
  length: 1,
  convRule: rule23
}, {
  start: 11490,
  length: 1,
  convRule: rule22
}, {
  start: 11491,
  length: 1,
  convRule: rule23
}, {
  start: 11492,
  length: 1,
  convRule: rule20
}, {
  start: 11493,
  length: 6,
  convRule: rule13
}, {
  start: 11499,
  length: 1,
  convRule: rule22
}, {
  start: 11500,
  length: 1,
  convRule: rule23
}, {
  start: 11501,
  length: 1,
  convRule: rule22
}, {
  start: 11502,
  length: 1,
  convRule: rule23
}, {
  start: 11503,
  length: 3,
  convRule: rule92
}, {
  start: 11506,
  length: 1,
  convRule: rule22
}, {
  start: 11507,
  length: 1,
  convRule: rule23
}, {
  start: 11513,
  length: 4,
  convRule: rule2
}, {
  start: 11517,
  length: 1,
  convRule: rule17
}, {
  start: 11518,
  length: 2,
  convRule: rule2
}, {
  start: 11520,
  length: 38,
  convRule: rule182
}, {
  start: 11559,
  length: 1,
  convRule: rule182
}, {
  start: 11565,
  length: 1,
  convRule: rule182
}, {
  start: 11568,
  length: 56,
  convRule: rule14
}, {
  start: 11631,
  length: 1,
  convRule: rule91
}, {
  start: 11632,
  length: 1,
  convRule: rule2
}, {
  start: 11647,
  length: 1,
  convRule: rule92
}, {
  start: 11648,
  length: 23,
  convRule: rule14
}, {
  start: 11680,
  length: 7,
  convRule: rule14
}, {
  start: 11688,
  length: 7,
  convRule: rule14
}, {
  start: 11696,
  length: 7,
  convRule: rule14
}, {
  start: 11704,
  length: 7,
  convRule: rule14
}, {
  start: 11712,
  length: 7,
  convRule: rule14
}, {
  start: 11720,
  length: 7,
  convRule: rule14
}, {
  start: 11728,
  length: 7,
  convRule: rule14
}, {
  start: 11736,
  length: 7,
  convRule: rule14
}, {
  start: 11744,
  length: 32,
  convRule: rule92
}, {
  start: 11776,
  length: 2,
  convRule: rule2
}, {
  start: 11778,
  length: 1,
  convRule: rule15
}, {
  start: 11779,
  length: 1,
  convRule: rule19
}, {
  start: 11780,
  length: 1,
  convRule: rule15
}, {
  start: 11781,
  length: 1,
  convRule: rule19
}, {
  start: 11782,
  length: 3,
  convRule: rule2
}, {
  start: 11785,
  length: 1,
  convRule: rule15
}, {
  start: 11786,
  length: 1,
  convRule: rule19
}, {
  start: 11787,
  length: 1,
  convRule: rule2
}, {
  start: 11788,
  length: 1,
  convRule: rule15
}, {
  start: 11789,
  length: 1,
  convRule: rule19
}, {
  start: 11790,
  length: 9,
  convRule: rule2
}, {
  start: 11799,
  length: 1,
  convRule: rule7
}, {
  start: 11800,
  length: 2,
  convRule: rule2
}, {
  start: 11802,
  length: 1,
  convRule: rule7
}, {
  start: 11803,
  length: 1,
  convRule: rule2
}, {
  start: 11804,
  length: 1,
  convRule: rule15
}, {
  start: 11805,
  length: 1,
  convRule: rule19
}, {
  start: 11806,
  length: 2,
  convRule: rule2
}, {
  start: 11808,
  length: 1,
  convRule: rule15
}, {
  start: 11809,
  length: 1,
  convRule: rule19
}, {
  start: 11810,
  length: 1,
  convRule: rule4
}, {
  start: 11811,
  length: 1,
  convRule: rule5
}, {
  start: 11812,
  length: 1,
  convRule: rule4
}, {
  start: 11813,
  length: 1,
  convRule: rule5
}, {
  start: 11814,
  length: 1,
  convRule: rule4
}, {
  start: 11815,
  length: 1,
  convRule: rule5
}, {
  start: 11816,
  length: 1,
  convRule: rule4
}, {
  start: 11817,
  length: 1,
  convRule: rule5
}, {
  start: 11818,
  length: 5,
  convRule: rule2
}, {
  start: 11823,
  length: 1,
  convRule: rule91
}, {
  start: 11824,
  length: 10,
  convRule: rule2
}, {
  start: 11834,
  length: 2,
  convRule: rule7
}, {
  start: 11836,
  length: 4,
  convRule: rule2
}, {
  start: 11840,
  length: 1,
  convRule: rule7
}, {
  start: 11841,
  length: 1,
  convRule: rule2
}, {
  start: 11842,
  length: 1,
  convRule: rule4
}, {
  start: 11843,
  length: 13,
  convRule: rule2
}, {
  start: 11856,
  length: 2,
  convRule: rule13
}, {
  start: 11858,
  length: 1,
  convRule: rule2
}, {
  start: 11904,
  length: 26,
  convRule: rule13
}, {
  start: 11931,
  length: 89,
  convRule: rule13
}, {
  start: 12032,
  length: 214,
  convRule: rule13
}, {
  start: 12272,
  length: 12,
  convRule: rule13
}, {
  start: 12288,
  length: 1,
  convRule: rule1
}, {
  start: 12289,
  length: 3,
  convRule: rule2
}, {
  start: 12292,
  length: 1,
  convRule: rule13
}, {
  start: 12293,
  length: 1,
  convRule: rule91
}, {
  start: 12294,
  length: 1,
  convRule: rule14
}, {
  start: 12295,
  length: 1,
  convRule: rule128
}, {
  start: 12296,
  length: 1,
  convRule: rule4
}, {
  start: 12297,
  length: 1,
  convRule: rule5
}, {
  start: 12298,
  length: 1,
  convRule: rule4
}, {
  start: 12299,
  length: 1,
  convRule: rule5
}, {
  start: 12300,
  length: 1,
  convRule: rule4
}, {
  start: 12301,
  length: 1,
  convRule: rule5
}, {
  start: 12302,
  length: 1,
  convRule: rule4
}, {
  start: 12303,
  length: 1,
  convRule: rule5
}, {
  start: 12304,
  length: 1,
  convRule: rule4
}, {
  start: 12305,
  length: 1,
  convRule: rule5
}, {
  start: 12306,
  length: 2,
  convRule: rule13
}, {
  start: 12308,
  length: 1,
  convRule: rule4
}, {
  start: 12309,
  length: 1,
  convRule: rule5
}, {
  start: 12310,
  length: 1,
  convRule: rule4
}, {
  start: 12311,
  length: 1,
  convRule: rule5
}, {
  start: 12312,
  length: 1,
  convRule: rule4
}, {
  start: 12313,
  length: 1,
  convRule: rule5
}, {
  start: 12314,
  length: 1,
  convRule: rule4
}, {
  start: 12315,
  length: 1,
  convRule: rule5
}, {
  start: 12316,
  length: 1,
  convRule: rule7
}, {
  start: 12317,
  length: 1,
  convRule: rule4
}, {
  start: 12318,
  length: 2,
  convRule: rule5
}, {
  start: 12320,
  length: 1,
  convRule: rule13
}, {
  start: 12321,
  length: 9,
  convRule: rule128
}, {
  start: 12330,
  length: 4,
  convRule: rule92
}, {
  start: 12334,
  length: 2,
  convRule: rule124
}, {
  start: 12336,
  length: 1,
  convRule: rule7
}, {
  start: 12337,
  length: 5,
  convRule: rule91
}, {
  start: 12342,
  length: 2,
  convRule: rule13
}, {
  start: 12344,
  length: 3,
  convRule: rule128
}, {
  start: 12347,
  length: 1,
  convRule: rule91
}, {
  start: 12348,
  length: 1,
  convRule: rule14
}, {
  start: 12349,
  length: 1,
  convRule: rule2
}, {
  start: 12350,
  length: 2,
  convRule: rule13
}, {
  start: 12353,
  length: 86,
  convRule: rule14
}, {
  start: 12441,
  length: 2,
  convRule: rule92
}, {
  start: 12443,
  length: 2,
  convRule: rule10
}, {
  start: 12445,
  length: 2,
  convRule: rule91
}, {
  start: 12447,
  length: 1,
  convRule: rule14
}, {
  start: 12448,
  length: 1,
  convRule: rule7
}, {
  start: 12449,
  length: 90,
  convRule: rule14
}, {
  start: 12539,
  length: 1,
  convRule: rule2
}, {
  start: 12540,
  length: 3,
  convRule: rule91
}, {
  start: 12543,
  length: 1,
  convRule: rule14
}, {
  start: 12549,
  length: 43,
  convRule: rule14
}, {
  start: 12593,
  length: 94,
  convRule: rule14
}, {
  start: 12688,
  length: 2,
  convRule: rule13
}, {
  start: 12690,
  length: 4,
  convRule: rule17
}, {
  start: 12694,
  length: 10,
  convRule: rule13
}, {
  start: 12704,
  length: 32,
  convRule: rule14
}, {
  start: 12736,
  length: 36,
  convRule: rule13
}, {
  start: 12784,
  length: 16,
  convRule: rule14
}, {
  start: 12800,
  length: 31,
  convRule: rule13
}, {
  start: 12832,
  length: 10,
  convRule: rule17
}, {
  start: 12842,
  length: 30,
  convRule: rule13
}, {
  start: 12872,
  length: 8,
  convRule: rule17
}, {
  start: 12880,
  length: 1,
  convRule: rule13
}, {
  start: 12881,
  length: 15,
  convRule: rule17
}, {
  start: 12896,
  length: 32,
  convRule: rule13
}, {
  start: 12928,
  length: 10,
  convRule: rule17
}, {
  start: 12938,
  length: 39,
  convRule: rule13
}, {
  start: 12977,
  length: 15,
  convRule: rule17
}, {
  start: 12992,
  length: 320,
  convRule: rule13
}, {
  start: 13312,
  length: 6592,
  convRule: rule14
}, {
  start: 19904,
  length: 64,
  convRule: rule13
}, {
  start: 19968,
  length: 20989,
  convRule: rule14
}, {
  start: 40960,
  length: 21,
  convRule: rule14
}, {
  start: 40981,
  length: 1,
  convRule: rule91
}, {
  start: 40982,
  length: 1143,
  convRule: rule14
}, {
  start: 42128,
  length: 55,
  convRule: rule13
}, {
  start: 42192,
  length: 40,
  convRule: rule14
}, {
  start: 42232,
  length: 6,
  convRule: rule91
}, {
  start: 42238,
  length: 2,
  convRule: rule2
}, {
  start: 42240,
  length: 268,
  convRule: rule14
}, {
  start: 42508,
  length: 1,
  convRule: rule91
}, {
  start: 42509,
  length: 3,
  convRule: rule2
}, {
  start: 42512,
  length: 16,
  convRule: rule14
}, {
  start: 42528,
  length: 10,
  convRule: rule8
}, {
  start: 42538,
  length: 2,
  convRule: rule14
}, {
  start: 42560,
  length: 1,
  convRule: rule22
}, {
  start: 42561,
  length: 1,
  convRule: rule23
}, {
  start: 42562,
  length: 1,
  convRule: rule22
}, {
  start: 42563,
  length: 1,
  convRule: rule23
}, {
  start: 42564,
  length: 1,
  convRule: rule22
}, {
  start: 42565,
  length: 1,
  convRule: rule23
}, {
  start: 42566,
  length: 1,
  convRule: rule22
}, {
  start: 42567,
  length: 1,
  convRule: rule23
}, {
  start: 42568,
  length: 1,
  convRule: rule22
}, {
  start: 42569,
  length: 1,
  convRule: rule23
}, {
  start: 42570,
  length: 1,
  convRule: rule22
}, {
  start: 42571,
  length: 1,
  convRule: rule23
}, {
  start: 42572,
  length: 1,
  convRule: rule22
}, {
  start: 42573,
  length: 1,
  convRule: rule23
}, {
  start: 42574,
  length: 1,
  convRule: rule22
}, {
  start: 42575,
  length: 1,
  convRule: rule23
}, {
  start: 42576,
  length: 1,
  convRule: rule22
}, {
  start: 42577,
  length: 1,
  convRule: rule23
}, {
  start: 42578,
  length: 1,
  convRule: rule22
}, {
  start: 42579,
  length: 1,
  convRule: rule23
}, {
  start: 42580,
  length: 1,
  convRule: rule22
}, {
  start: 42581,
  length: 1,
  convRule: rule23
}, {
  start: 42582,
  length: 1,
  convRule: rule22
}, {
  start: 42583,
  length: 1,
  convRule: rule23
}, {
  start: 42584,
  length: 1,
  convRule: rule22
}, {
  start: 42585,
  length: 1,
  convRule: rule23
}, {
  start: 42586,
  length: 1,
  convRule: rule22
}, {
  start: 42587,
  length: 1,
  convRule: rule23
}, {
  start: 42588,
  length: 1,
  convRule: rule22
}, {
  start: 42589,
  length: 1,
  convRule: rule23
}, {
  start: 42590,
  length: 1,
  convRule: rule22
}, {
  start: 42591,
  length: 1,
  convRule: rule23
}, {
  start: 42592,
  length: 1,
  convRule: rule22
}, {
  start: 42593,
  length: 1,
  convRule: rule23
}, {
  start: 42594,
  length: 1,
  convRule: rule22
}, {
  start: 42595,
  length: 1,
  convRule: rule23
}, {
  start: 42596,
  length: 1,
  convRule: rule22
}, {
  start: 42597,
  length: 1,
  convRule: rule23
}, {
  start: 42598,
  length: 1,
  convRule: rule22
}, {
  start: 42599,
  length: 1,
  convRule: rule23
}, {
  start: 42600,
  length: 1,
  convRule: rule22
}, {
  start: 42601,
  length: 1,
  convRule: rule23
}, {
  start: 42602,
  length: 1,
  convRule: rule22
}, {
  start: 42603,
  length: 1,
  convRule: rule23
}, {
  start: 42604,
  length: 1,
  convRule: rule22
}, {
  start: 42605,
  length: 1,
  convRule: rule23
}, {
  start: 42606,
  length: 1,
  convRule: rule14
}, {
  start: 42607,
  length: 1,
  convRule: rule92
}, {
  start: 42608,
  length: 3,
  convRule: rule119
}, {
  start: 42611,
  length: 1,
  convRule: rule2
}, {
  start: 42612,
  length: 10,
  convRule: rule92
}, {
  start: 42622,
  length: 1,
  convRule: rule2
}, {
  start: 42623,
  length: 1,
  convRule: rule91
}, {
  start: 42624,
  length: 1,
  convRule: rule22
}, {
  start: 42625,
  length: 1,
  convRule: rule23
}, {
  start: 42626,
  length: 1,
  convRule: rule22
}, {
  start: 42627,
  length: 1,
  convRule: rule23
}, {
  start: 42628,
  length: 1,
  convRule: rule22
}, {
  start: 42629,
  length: 1,
  convRule: rule23
}, {
  start: 42630,
  length: 1,
  convRule: rule22
}, {
  start: 42631,
  length: 1,
  convRule: rule23
}, {
  start: 42632,
  length: 1,
  convRule: rule22
}, {
  start: 42633,
  length: 1,
  convRule: rule23
}, {
  start: 42634,
  length: 1,
  convRule: rule22
}, {
  start: 42635,
  length: 1,
  convRule: rule23
}, {
  start: 42636,
  length: 1,
  convRule: rule22
}, {
  start: 42637,
  length: 1,
  convRule: rule23
}, {
  start: 42638,
  length: 1,
  convRule: rule22
}, {
  start: 42639,
  length: 1,
  convRule: rule23
}, {
  start: 42640,
  length: 1,
  convRule: rule22
}, {
  start: 42641,
  length: 1,
  convRule: rule23
}, {
  start: 42642,
  length: 1,
  convRule: rule22
}, {
  start: 42643,
  length: 1,
  convRule: rule23
}, {
  start: 42644,
  length: 1,
  convRule: rule22
}, {
  start: 42645,
  length: 1,
  convRule: rule23
}, {
  start: 42646,
  length: 1,
  convRule: rule22
}, {
  start: 42647,
  length: 1,
  convRule: rule23
}, {
  start: 42648,
  length: 1,
  convRule: rule22
}, {
  start: 42649,
  length: 1,
  convRule: rule23
}, {
  start: 42650,
  length: 1,
  convRule: rule22
}, {
  start: 42651,
  length: 1,
  convRule: rule23
}, {
  start: 42652,
  length: 2,
  convRule: rule91
}, {
  start: 42654,
  length: 2,
  convRule: rule92
}, {
  start: 42656,
  length: 70,
  convRule: rule14
}, {
  start: 42726,
  length: 10,
  convRule: rule128
}, {
  start: 42736,
  length: 2,
  convRule: rule92
}, {
  start: 42738,
  length: 6,
  convRule: rule2
}, {
  start: 42752,
  length: 23,
  convRule: rule10
}, {
  start: 42775,
  length: 9,
  convRule: rule91
}, {
  start: 42784,
  length: 2,
  convRule: rule10
}, {
  start: 42786,
  length: 1,
  convRule: rule22
}, {
  start: 42787,
  length: 1,
  convRule: rule23
}, {
  start: 42788,
  length: 1,
  convRule: rule22
}, {
  start: 42789,
  length: 1,
  convRule: rule23
}, {
  start: 42790,
  length: 1,
  convRule: rule22
}, {
  start: 42791,
  length: 1,
  convRule: rule23
}, {
  start: 42792,
  length: 1,
  convRule: rule22
}, {
  start: 42793,
  length: 1,
  convRule: rule23
}, {
  start: 42794,
  length: 1,
  convRule: rule22
}, {
  start: 42795,
  length: 1,
  convRule: rule23
}, {
  start: 42796,
  length: 1,
  convRule: rule22
}, {
  start: 42797,
  length: 1,
  convRule: rule23
}, {
  start: 42798,
  length: 1,
  convRule: rule22
}, {
  start: 42799,
  length: 1,
  convRule: rule23
}, {
  start: 42800,
  length: 2,
  convRule: rule20
}, {
  start: 42802,
  length: 1,
  convRule: rule22
}, {
  start: 42803,
  length: 1,
  convRule: rule23
}, {
  start: 42804,
  length: 1,
  convRule: rule22
}, {
  start: 42805,
  length: 1,
  convRule: rule23
}, {
  start: 42806,
  length: 1,
  convRule: rule22
}, {
  start: 42807,
  length: 1,
  convRule: rule23
}, {
  start: 42808,
  length: 1,
  convRule: rule22
}, {
  start: 42809,
  length: 1,
  convRule: rule23
}, {
  start: 42810,
  length: 1,
  convRule: rule22
}, {
  start: 42811,
  length: 1,
  convRule: rule23
}, {
  start: 42812,
  length: 1,
  convRule: rule22
}, {
  start: 42813,
  length: 1,
  convRule: rule23
}, {
  start: 42814,
  length: 1,
  convRule: rule22
}, {
  start: 42815,
  length: 1,
  convRule: rule23
}, {
  start: 42816,
  length: 1,
  convRule: rule22
}, {
  start: 42817,
  length: 1,
  convRule: rule23
}, {
  start: 42818,
  length: 1,
  convRule: rule22
}, {
  start: 42819,
  length: 1,
  convRule: rule23
}, {
  start: 42820,
  length: 1,
  convRule: rule22
}, {
  start: 42821,
  length: 1,
  convRule: rule23
}, {
  start: 42822,
  length: 1,
  convRule: rule22
}, {
  start: 42823,
  length: 1,
  convRule: rule23
}, {
  start: 42824,
  length: 1,
  convRule: rule22
}, {
  start: 42825,
  length: 1,
  convRule: rule23
}, {
  start: 42826,
  length: 1,
  convRule: rule22
}, {
  start: 42827,
  length: 1,
  convRule: rule23
}, {
  start: 42828,
  length: 1,
  convRule: rule22
}, {
  start: 42829,
  length: 1,
  convRule: rule23
}, {
  start: 42830,
  length: 1,
  convRule: rule22
}, {
  start: 42831,
  length: 1,
  convRule: rule23
}, {
  start: 42832,
  length: 1,
  convRule: rule22
}, {
  start: 42833,
  length: 1,
  convRule: rule23
}, {
  start: 42834,
  length: 1,
  convRule: rule22
}, {
  start: 42835,
  length: 1,
  convRule: rule23
}, {
  start: 42836,
  length: 1,
  convRule: rule22
}, {
  start: 42837,
  length: 1,
  convRule: rule23
}, {
  start: 42838,
  length: 1,
  convRule: rule22
}, {
  start: 42839,
  length: 1,
  convRule: rule23
}, {
  start: 42840,
  length: 1,
  convRule: rule22
}, {
  start: 42841,
  length: 1,
  convRule: rule23
}, {
  start: 42842,
  length: 1,
  convRule: rule22
}, {
  start: 42843,
  length: 1,
  convRule: rule23
}, {
  start: 42844,
  length: 1,
  convRule: rule22
}, {
  start: 42845,
  length: 1,
  convRule: rule23
}, {
  start: 42846,
  length: 1,
  convRule: rule22
}, {
  start: 42847,
  length: 1,
  convRule: rule23
}, {
  start: 42848,
  length: 1,
  convRule: rule22
}, {
  start: 42849,
  length: 1,
  convRule: rule23
}, {
  start: 42850,
  length: 1,
  convRule: rule22
}, {
  start: 42851,
  length: 1,
  convRule: rule23
}, {
  start: 42852,
  length: 1,
  convRule: rule22
}, {
  start: 42853,
  length: 1,
  convRule: rule23
}, {
  start: 42854,
  length: 1,
  convRule: rule22
}, {
  start: 42855,
  length: 1,
  convRule: rule23
}, {
  start: 42856,
  length: 1,
  convRule: rule22
}, {
  start: 42857,
  length: 1,
  convRule: rule23
}, {
  start: 42858,
  length: 1,
  convRule: rule22
}, {
  start: 42859,
  length: 1,
  convRule: rule23
}, {
  start: 42860,
  length: 1,
  convRule: rule22
}, {
  start: 42861,
  length: 1,
  convRule: rule23
}, {
  start: 42862,
  length: 1,
  convRule: rule22
}, {
  start: 42863,
  length: 1,
  convRule: rule23
}, {
  start: 42864,
  length: 1,
  convRule: rule91
}, {
  start: 42865,
  length: 8,
  convRule: rule20
}, {
  start: 42873,
  length: 1,
  convRule: rule22
}, {
  start: 42874,
  length: 1,
  convRule: rule23
}, {
  start: 42875,
  length: 1,
  convRule: rule22
}, {
  start: 42876,
  length: 1,
  convRule: rule23
}, {
  start: 42877,
  length: 1,
  convRule: rule183
}, {
  start: 42878,
  length: 1,
  convRule: rule22
}, {
  start: 42879,
  length: 1,
  convRule: rule23
}, {
  start: 42880,
  length: 1,
  convRule: rule22
}, {
  start: 42881,
  length: 1,
  convRule: rule23
}, {
  start: 42882,
  length: 1,
  convRule: rule22
}, {
  start: 42883,
  length: 1,
  convRule: rule23
}, {
  start: 42884,
  length: 1,
  convRule: rule22
}, {
  start: 42885,
  length: 1,
  convRule: rule23
}, {
  start: 42886,
  length: 1,
  convRule: rule22
}, {
  start: 42887,
  length: 1,
  convRule: rule23
}, {
  start: 42888,
  length: 1,
  convRule: rule91
}, {
  start: 42889,
  length: 2,
  convRule: rule10
}, {
  start: 42891,
  length: 1,
  convRule: rule22
}, {
  start: 42892,
  length: 1,
  convRule: rule23
}, {
  start: 42893,
  length: 1,
  convRule: rule184
}, {
  start: 42894,
  length: 1,
  convRule: rule20
}, {
  start: 42895,
  length: 1,
  convRule: rule14
}, {
  start: 42896,
  length: 1,
  convRule: rule22
}, {
  start: 42897,
  length: 1,
  convRule: rule23
}, {
  start: 42898,
  length: 1,
  convRule: rule22
}, {
  start: 42899,
  length: 1,
  convRule: rule23
}, {
  start: 42900,
  length: 1,
  convRule: rule185
}, {
  start: 42901,
  length: 1,
  convRule: rule20
}, {
  start: 42902,
  length: 1,
  convRule: rule22
}, {
  start: 42903,
  length: 1,
  convRule: rule23
}, {
  start: 42904,
  length: 1,
  convRule: rule22
}, {
  start: 42905,
  length: 1,
  convRule: rule23
}, {
  start: 42906,
  length: 1,
  convRule: rule22
}, {
  start: 42907,
  length: 1,
  convRule: rule23
}, {
  start: 42908,
  length: 1,
  convRule: rule22
}, {
  start: 42909,
  length: 1,
  convRule: rule23
}, {
  start: 42910,
  length: 1,
  convRule: rule22
}, {
  start: 42911,
  length: 1,
  convRule: rule23
}, {
  start: 42912,
  length: 1,
  convRule: rule22
}, {
  start: 42913,
  length: 1,
  convRule: rule23
}, {
  start: 42914,
  length: 1,
  convRule: rule22
}, {
  start: 42915,
  length: 1,
  convRule: rule23
}, {
  start: 42916,
  length: 1,
  convRule: rule22
}, {
  start: 42917,
  length: 1,
  convRule: rule23
}, {
  start: 42918,
  length: 1,
  convRule: rule22
}, {
  start: 42919,
  length: 1,
  convRule: rule23
}, {
  start: 42920,
  length: 1,
  convRule: rule22
}, {
  start: 42921,
  length: 1,
  convRule: rule23
}, {
  start: 42922,
  length: 1,
  convRule: rule186
}, {
  start: 42923,
  length: 1,
  convRule: rule187
}, {
  start: 42924,
  length: 1,
  convRule: rule188
}, {
  start: 42925,
  length: 1,
  convRule: rule189
}, {
  start: 42926,
  length: 1,
  convRule: rule186
}, {
  start: 42927,
  length: 1,
  convRule: rule20
}, {
  start: 42928,
  length: 1,
  convRule: rule190
}, {
  start: 42929,
  length: 1,
  convRule: rule191
}, {
  start: 42930,
  length: 1,
  convRule: rule192
}, {
  start: 42931,
  length: 1,
  convRule: rule193
}, {
  start: 42932,
  length: 1,
  convRule: rule22
}, {
  start: 42933,
  length: 1,
  convRule: rule23
}, {
  start: 42934,
  length: 1,
  convRule: rule22
}, {
  start: 42935,
  length: 1,
  convRule: rule23
}, {
  start: 42936,
  length: 1,
  convRule: rule22
}, {
  start: 42937,
  length: 1,
  convRule: rule23
}, {
  start: 42938,
  length: 1,
  convRule: rule22
}, {
  start: 42939,
  length: 1,
  convRule: rule23
}, {
  start: 42940,
  length: 1,
  convRule: rule22
}, {
  start: 42941,
  length: 1,
  convRule: rule23
}, {
  start: 42942,
  length: 1,
  convRule: rule22
}, {
  start: 42943,
  length: 1,
  convRule: rule23
}, {
  start: 42946,
  length: 1,
  convRule: rule22
}, {
  start: 42947,
  length: 1,
  convRule: rule23
}, {
  start: 42948,
  length: 1,
  convRule: rule194
}, {
  start: 42949,
  length: 1,
  convRule: rule195
}, {
  start: 42950,
  length: 1,
  convRule: rule196
}, {
  start: 42951,
  length: 1,
  convRule: rule22
}, {
  start: 42952,
  length: 1,
  convRule: rule23
}, {
  start: 42953,
  length: 1,
  convRule: rule22
}, {
  start: 42954,
  length: 1,
  convRule: rule23
}, {
  start: 42997,
  length: 1,
  convRule: rule22
}, {
  start: 42998,
  length: 1,
  convRule: rule23
}, {
  start: 42999,
  length: 1,
  convRule: rule14
}, {
  start: 43e3,
  length: 2,
  convRule: rule91
}, {
  start: 43002,
  length: 1,
  convRule: rule20
}, {
  start: 43003,
  length: 7,
  convRule: rule14
}, {
  start: 43010,
  length: 1,
  convRule: rule92
}, {
  start: 43011,
  length: 3,
  convRule: rule14
}, {
  start: 43014,
  length: 1,
  convRule: rule92
}, {
  start: 43015,
  length: 4,
  convRule: rule14
}, {
  start: 43019,
  length: 1,
  convRule: rule92
}, {
  start: 43020,
  length: 23,
  convRule: rule14
}, {
  start: 43043,
  length: 2,
  convRule: rule124
}, {
  start: 43045,
  length: 2,
  convRule: rule92
}, {
  start: 43047,
  length: 1,
  convRule: rule124
}, {
  start: 43048,
  length: 4,
  convRule: rule13
}, {
  start: 43052,
  length: 1,
  convRule: rule92
}, {
  start: 43056,
  length: 6,
  convRule: rule17
}, {
  start: 43062,
  length: 2,
  convRule: rule13
}, {
  start: 43064,
  length: 1,
  convRule: rule3
}, {
  start: 43065,
  length: 1,
  convRule: rule13
}, {
  start: 43072,
  length: 52,
  convRule: rule14
}, {
  start: 43124,
  length: 4,
  convRule: rule2
}, {
  start: 43136,
  length: 2,
  convRule: rule124
}, {
  start: 43138,
  length: 50,
  convRule: rule14
}, {
  start: 43188,
  length: 16,
  convRule: rule124
}, {
  start: 43204,
  length: 2,
  convRule: rule92
}, {
  start: 43214,
  length: 2,
  convRule: rule2
}, {
  start: 43216,
  length: 10,
  convRule: rule8
}, {
  start: 43232,
  length: 18,
  convRule: rule92
}, {
  start: 43250,
  length: 6,
  convRule: rule14
}, {
  start: 43256,
  length: 3,
  convRule: rule2
}, {
  start: 43259,
  length: 1,
  convRule: rule14
}, {
  start: 43260,
  length: 1,
  convRule: rule2
}, {
  start: 43261,
  length: 2,
  convRule: rule14
}, {
  start: 43263,
  length: 1,
  convRule: rule92
}, {
  start: 43264,
  length: 10,
  convRule: rule8
}, {
  start: 43274,
  length: 28,
  convRule: rule14
}, {
  start: 43302,
  length: 8,
  convRule: rule92
}, {
  start: 43310,
  length: 2,
  convRule: rule2
}, {
  start: 43312,
  length: 23,
  convRule: rule14
}, {
  start: 43335,
  length: 11,
  convRule: rule92
}, {
  start: 43346,
  length: 2,
  convRule: rule124
}, {
  start: 43359,
  length: 1,
  convRule: rule2
}, {
  start: 43360,
  length: 29,
  convRule: rule14
}, {
  start: 43392,
  length: 3,
  convRule: rule92
}, {
  start: 43395,
  length: 1,
  convRule: rule124
}, {
  start: 43396,
  length: 47,
  convRule: rule14
}, {
  start: 43443,
  length: 1,
  convRule: rule92
}, {
  start: 43444,
  length: 2,
  convRule: rule124
}, {
  start: 43446,
  length: 4,
  convRule: rule92
}, {
  start: 43450,
  length: 2,
  convRule: rule124
}, {
  start: 43452,
  length: 2,
  convRule: rule92
}, {
  start: 43454,
  length: 3,
  convRule: rule124
}, {
  start: 43457,
  length: 13,
  convRule: rule2
}, {
  start: 43471,
  length: 1,
  convRule: rule91
}, {
  start: 43472,
  length: 10,
  convRule: rule8
}, {
  start: 43486,
  length: 2,
  convRule: rule2
}, {
  start: 43488,
  length: 5,
  convRule: rule14
}, {
  start: 43493,
  length: 1,
  convRule: rule92
}, {
  start: 43494,
  length: 1,
  convRule: rule91
}, {
  start: 43495,
  length: 9,
  convRule: rule14
}, {
  start: 43504,
  length: 10,
  convRule: rule8
}, {
  start: 43514,
  length: 5,
  convRule: rule14
}, {
  start: 43520,
  length: 41,
  convRule: rule14
}, {
  start: 43561,
  length: 6,
  convRule: rule92
}, {
  start: 43567,
  length: 2,
  convRule: rule124
}, {
  start: 43569,
  length: 2,
  convRule: rule92
}, {
  start: 43571,
  length: 2,
  convRule: rule124
}, {
  start: 43573,
  length: 2,
  convRule: rule92
}, {
  start: 43584,
  length: 3,
  convRule: rule14
}, {
  start: 43587,
  length: 1,
  convRule: rule92
}, {
  start: 43588,
  length: 8,
  convRule: rule14
}, {
  start: 43596,
  length: 1,
  convRule: rule92
}, {
  start: 43597,
  length: 1,
  convRule: rule124
}, {
  start: 43600,
  length: 10,
  convRule: rule8
}, {
  start: 43612,
  length: 4,
  convRule: rule2
}, {
  start: 43616,
  length: 16,
  convRule: rule14
}, {
  start: 43632,
  length: 1,
  convRule: rule91
}, {
  start: 43633,
  length: 6,
  convRule: rule14
}, {
  start: 43639,
  length: 3,
  convRule: rule13
}, {
  start: 43642,
  length: 1,
  convRule: rule14
}, {
  start: 43643,
  length: 1,
  convRule: rule124
}, {
  start: 43644,
  length: 1,
  convRule: rule92
}, {
  start: 43645,
  length: 1,
  convRule: rule124
}, {
  start: 43646,
  length: 50,
  convRule: rule14
}, {
  start: 43696,
  length: 1,
  convRule: rule92
}, {
  start: 43697,
  length: 1,
  convRule: rule14
}, {
  start: 43698,
  length: 3,
  convRule: rule92
}, {
  start: 43701,
  length: 2,
  convRule: rule14
}, {
  start: 43703,
  length: 2,
  convRule: rule92
}, {
  start: 43705,
  length: 5,
  convRule: rule14
}, {
  start: 43710,
  length: 2,
  convRule: rule92
}, {
  start: 43712,
  length: 1,
  convRule: rule14
}, {
  start: 43713,
  length: 1,
  convRule: rule92
}, {
  start: 43714,
  length: 1,
  convRule: rule14
}, {
  start: 43739,
  length: 2,
  convRule: rule14
}, {
  start: 43741,
  length: 1,
  convRule: rule91
}, {
  start: 43742,
  length: 2,
  convRule: rule2
}, {
  start: 43744,
  length: 11,
  convRule: rule14
}, {
  start: 43755,
  length: 1,
  convRule: rule124
}, {
  start: 43756,
  length: 2,
  convRule: rule92
}, {
  start: 43758,
  length: 2,
  convRule: rule124
}, {
  start: 43760,
  length: 2,
  convRule: rule2
}, {
  start: 43762,
  length: 1,
  convRule: rule14
}, {
  start: 43763,
  length: 2,
  convRule: rule91
}, {
  start: 43765,
  length: 1,
  convRule: rule124
}, {
  start: 43766,
  length: 1,
  convRule: rule92
}, {
  start: 43777,
  length: 6,
  convRule: rule14
}, {
  start: 43785,
  length: 6,
  convRule: rule14
}, {
  start: 43793,
  length: 6,
  convRule: rule14
}, {
  start: 43808,
  length: 7,
  convRule: rule14
}, {
  start: 43816,
  length: 7,
  convRule: rule14
}, {
  start: 43824,
  length: 35,
  convRule: rule20
}, {
  start: 43859,
  length: 1,
  convRule: rule197
}, {
  start: 43860,
  length: 7,
  convRule: rule20
}, {
  start: 43867,
  length: 1,
  convRule: rule10
}, {
  start: 43868,
  length: 4,
  convRule: rule91
}, {
  start: 43872,
  length: 9,
  convRule: rule20
}, {
  start: 43881,
  length: 1,
  convRule: rule91
}, {
  start: 43882,
  length: 2,
  convRule: rule10
}, {
  start: 43888,
  length: 80,
  convRule: rule198
}, {
  start: 43968,
  length: 35,
  convRule: rule14
}, {
  start: 44003,
  length: 2,
  convRule: rule124
}, {
  start: 44005,
  length: 1,
  convRule: rule92
}, {
  start: 44006,
  length: 2,
  convRule: rule124
}, {
  start: 44008,
  length: 1,
  convRule: rule92
}, {
  start: 44009,
  length: 2,
  convRule: rule124
}, {
  start: 44011,
  length: 1,
  convRule: rule2
}, {
  start: 44012,
  length: 1,
  convRule: rule124
}, {
  start: 44013,
  length: 1,
  convRule: rule92
}, {
  start: 44016,
  length: 10,
  convRule: rule8
}, {
  start: 44032,
  length: 11172,
  convRule: rule14
}, {
  start: 55216,
  length: 23,
  convRule: rule14
}, {
  start: 55243,
  length: 49,
  convRule: rule14
}, {
  start: 55296,
  length: 896,
  convRule: rule199
}, {
  start: 56192,
  length: 128,
  convRule: rule199
}, {
  start: 56320,
  length: 1024,
  convRule: rule199
}, {
  start: 57344,
  length: 6400,
  convRule: rule200
}, {
  start: 63744,
  length: 366,
  convRule: rule14
}, {
  start: 64112,
  length: 106,
  convRule: rule14
}, {
  start: 64256,
  length: 7,
  convRule: rule20
}, {
  start: 64275,
  length: 5,
  convRule: rule20
}, {
  start: 64285,
  length: 1,
  convRule: rule14
}, {
  start: 64286,
  length: 1,
  convRule: rule92
}, {
  start: 64287,
  length: 10,
  convRule: rule14
}, {
  start: 64297,
  length: 1,
  convRule: rule6
}, {
  start: 64298,
  length: 13,
  convRule: rule14
}, {
  start: 64312,
  length: 5,
  convRule: rule14
}, {
  start: 64318,
  length: 1,
  convRule: rule14
}, {
  start: 64320,
  length: 2,
  convRule: rule14
}, {
  start: 64323,
  length: 2,
  convRule: rule14
}, {
  start: 64326,
  length: 108,
  convRule: rule14
}, {
  start: 64434,
  length: 16,
  convRule: rule10
}, {
  start: 64467,
  length: 363,
  convRule: rule14
}, {
  start: 64830,
  length: 1,
  convRule: rule5
}, {
  start: 64831,
  length: 1,
  convRule: rule4
}, {
  start: 64848,
  length: 64,
  convRule: rule14
}, {
  start: 64914,
  length: 54,
  convRule: rule14
}, {
  start: 65008,
  length: 12,
  convRule: rule14
}, {
  start: 65020,
  length: 1,
  convRule: rule3
}, {
  start: 65021,
  length: 1,
  convRule: rule13
}, {
  start: 65024,
  length: 16,
  convRule: rule92
}, {
  start: 65040,
  length: 7,
  convRule: rule2
}, {
  start: 65047,
  length: 1,
  convRule: rule4
}, {
  start: 65048,
  length: 1,
  convRule: rule5
}, {
  start: 65049,
  length: 1,
  convRule: rule2
}, {
  start: 65056,
  length: 16,
  convRule: rule92
}, {
  start: 65072,
  length: 1,
  convRule: rule2
}, {
  start: 65073,
  length: 2,
  convRule: rule7
}, {
  start: 65075,
  length: 2,
  convRule: rule11
}, {
  start: 65077,
  length: 1,
  convRule: rule4
}, {
  start: 65078,
  length: 1,
  convRule: rule5
}, {
  start: 65079,
  length: 1,
  convRule: rule4
}, {
  start: 65080,
  length: 1,
  convRule: rule5
}, {
  start: 65081,
  length: 1,
  convRule: rule4
}, {
  start: 65082,
  length: 1,
  convRule: rule5
}, {
  start: 65083,
  length: 1,
  convRule: rule4
}, {
  start: 65084,
  length: 1,
  convRule: rule5
}, {
  start: 65085,
  length: 1,
  convRule: rule4
}, {
  start: 65086,
  length: 1,
  convRule: rule5
}, {
  start: 65087,
  length: 1,
  convRule: rule4
}, {
  start: 65088,
  length: 1,
  convRule: rule5
}, {
  start: 65089,
  length: 1,
  convRule: rule4
}, {
  start: 65090,
  length: 1,
  convRule: rule5
}, {
  start: 65091,
  length: 1,
  convRule: rule4
}, {
  start: 65092,
  length: 1,
  convRule: rule5
}, {
  start: 65093,
  length: 2,
  convRule: rule2
}, {
  start: 65095,
  length: 1,
  convRule: rule4
}, {
  start: 65096,
  length: 1,
  convRule: rule5
}, {
  start: 65097,
  length: 4,
  convRule: rule2
}, {
  start: 65101,
  length: 3,
  convRule: rule11
}, {
  start: 65104,
  length: 3,
  convRule: rule2
}, {
  start: 65108,
  length: 4,
  convRule: rule2
}, {
  start: 65112,
  length: 1,
  convRule: rule7
}, {
  start: 65113,
  length: 1,
  convRule: rule4
}, {
  start: 65114,
  length: 1,
  convRule: rule5
}, {
  start: 65115,
  length: 1,
  convRule: rule4
}, {
  start: 65116,
  length: 1,
  convRule: rule5
}, {
  start: 65117,
  length: 1,
  convRule: rule4
}, {
  start: 65118,
  length: 1,
  convRule: rule5
}, {
  start: 65119,
  length: 3,
  convRule: rule2
}, {
  start: 65122,
  length: 1,
  convRule: rule6
}, {
  start: 65123,
  length: 1,
  convRule: rule7
}, {
  start: 65124,
  length: 3,
  convRule: rule6
}, {
  start: 65128,
  length: 1,
  convRule: rule2
}, {
  start: 65129,
  length: 1,
  convRule: rule3
}, {
  start: 65130,
  length: 2,
  convRule: rule2
}, {
  start: 65136,
  length: 5,
  convRule: rule14
}, {
  start: 65142,
  length: 135,
  convRule: rule14
}, {
  start: 65279,
  length: 1,
  convRule: rule16
}, {
  start: 65281,
  length: 3,
  convRule: rule2
}, {
  start: 65284,
  length: 1,
  convRule: rule3
}, {
  start: 65285,
  length: 3,
  convRule: rule2
}, {
  start: 65288,
  length: 1,
  convRule: rule4
}, {
  start: 65289,
  length: 1,
  convRule: rule5
}, {
  start: 65290,
  length: 1,
  convRule: rule2
}, {
  start: 65291,
  length: 1,
  convRule: rule6
}, {
  start: 65292,
  length: 1,
  convRule: rule2
}, {
  start: 65293,
  length: 1,
  convRule: rule7
}, {
  start: 65294,
  length: 2,
  convRule: rule2
}, {
  start: 65296,
  length: 10,
  convRule: rule8
}, {
  start: 65306,
  length: 2,
  convRule: rule2
}, {
  start: 65308,
  length: 3,
  convRule: rule6
}, {
  start: 65311,
  length: 2,
  convRule: rule2
}, {
  start: 65313,
  length: 26,
  convRule: rule9
}, {
  start: 65339,
  length: 1,
  convRule: rule4
}, {
  start: 65340,
  length: 1,
  convRule: rule2
}, {
  start: 65341,
  length: 1,
  convRule: rule5
}, {
  start: 65342,
  length: 1,
  convRule: rule10
}, {
  start: 65343,
  length: 1,
  convRule: rule11
}, {
  start: 65344,
  length: 1,
  convRule: rule10
}, {
  start: 65345,
  length: 26,
  convRule: rule12
}, {
  start: 65371,
  length: 1,
  convRule: rule4
}, {
  start: 65372,
  length: 1,
  convRule: rule6
}, {
  start: 65373,
  length: 1,
  convRule: rule5
}, {
  start: 65374,
  length: 1,
  convRule: rule6
}, {
  start: 65375,
  length: 1,
  convRule: rule4
}, {
  start: 65376,
  length: 1,
  convRule: rule5
}, {
  start: 65377,
  length: 1,
  convRule: rule2
}, {
  start: 65378,
  length: 1,
  convRule: rule4
}, {
  start: 65379,
  length: 1,
  convRule: rule5
}, {
  start: 65380,
  length: 2,
  convRule: rule2
}, {
  start: 65382,
  length: 10,
  convRule: rule14
}, {
  start: 65392,
  length: 1,
  convRule: rule91
}, {
  start: 65393,
  length: 45,
  convRule: rule14
}, {
  start: 65438,
  length: 2,
  convRule: rule91
}, {
  start: 65440,
  length: 31,
  convRule: rule14
}, {
  start: 65474,
  length: 6,
  convRule: rule14
}, {
  start: 65482,
  length: 6,
  convRule: rule14
}, {
  start: 65490,
  length: 6,
  convRule: rule14
}, {
  start: 65498,
  length: 3,
  convRule: rule14
}, {
  start: 65504,
  length: 2,
  convRule: rule3
}, {
  start: 65506,
  length: 1,
  convRule: rule6
}, {
  start: 65507,
  length: 1,
  convRule: rule10
}, {
  start: 65508,
  length: 1,
  convRule: rule13
}, {
  start: 65509,
  length: 2,
  convRule: rule3
}, {
  start: 65512,
  length: 1,
  convRule: rule13
}, {
  start: 65513,
  length: 4,
  convRule: rule6
}, {
  start: 65517,
  length: 2,
  convRule: rule13
}, {
  start: 65529,
  length: 3,
  convRule: rule16
}, {
  start: 65532,
  length: 2,
  convRule: rule13
}, {
  start: 65536,
  length: 12,
  convRule: rule14
}, {
  start: 65549,
  length: 26,
  convRule: rule14
}, {
  start: 65576,
  length: 19,
  convRule: rule14
}, {
  start: 65596,
  length: 2,
  convRule: rule14
}, {
  start: 65599,
  length: 15,
  convRule: rule14
}, {
  start: 65616,
  length: 14,
  convRule: rule14
}, {
  start: 65664,
  length: 123,
  convRule: rule14
}, {
  start: 65792,
  length: 3,
  convRule: rule2
}, {
  start: 65799,
  length: 45,
  convRule: rule17
}, {
  start: 65847,
  length: 9,
  convRule: rule13
}, {
  start: 65856,
  length: 53,
  convRule: rule128
}, {
  start: 65909,
  length: 4,
  convRule: rule17
}, {
  start: 65913,
  length: 17,
  convRule: rule13
}, {
  start: 65930,
  length: 2,
  convRule: rule17
}, {
  start: 65932,
  length: 3,
  convRule: rule13
}, {
  start: 65936,
  length: 13,
  convRule: rule13
}, {
  start: 65952,
  length: 1,
  convRule: rule13
}, {
  start: 66e3,
  length: 45,
  convRule: rule13
}, {
  start: 66045,
  length: 1,
  convRule: rule92
}, {
  start: 66176,
  length: 29,
  convRule: rule14
}, {
  start: 66208,
  length: 49,
  convRule: rule14
}, {
  start: 66272,
  length: 1,
  convRule: rule92
}, {
  start: 66273,
  length: 27,
  convRule: rule17
}, {
  start: 66304,
  length: 32,
  convRule: rule14
}, {
  start: 66336,
  length: 4,
  convRule: rule17
}, {
  start: 66349,
  length: 20,
  convRule: rule14
}, {
  start: 66369,
  length: 1,
  convRule: rule128
}, {
  start: 66370,
  length: 8,
  convRule: rule14
}, {
  start: 66378,
  length: 1,
  convRule: rule128
}, {
  start: 66384,
  length: 38,
  convRule: rule14
}, {
  start: 66422,
  length: 5,
  convRule: rule92
}, {
  start: 66432,
  length: 30,
  convRule: rule14
}, {
  start: 66463,
  length: 1,
  convRule: rule2
}, {
  start: 66464,
  length: 36,
  convRule: rule14
}, {
  start: 66504,
  length: 8,
  convRule: rule14
}, {
  start: 66512,
  length: 1,
  convRule: rule2
}, {
  start: 66513,
  length: 5,
  convRule: rule128
}, {
  start: 66560,
  length: 40,
  convRule: rule201
}, {
  start: 66600,
  length: 40,
  convRule: rule202
}, {
  start: 66640,
  length: 78,
  convRule: rule14
}, {
  start: 66720,
  length: 10,
  convRule: rule8
}, {
  start: 66736,
  length: 36,
  convRule: rule201
}, {
  start: 66776,
  length: 36,
  convRule: rule202
}, {
  start: 66816,
  length: 40,
  convRule: rule14
}, {
  start: 66864,
  length: 52,
  convRule: rule14
}, {
  start: 66927,
  length: 1,
  convRule: rule2
}, {
  start: 67072,
  length: 311,
  convRule: rule14
}, {
  start: 67392,
  length: 22,
  convRule: rule14
}, {
  start: 67424,
  length: 8,
  convRule: rule14
}, {
  start: 67584,
  length: 6,
  convRule: rule14
}, {
  start: 67592,
  length: 1,
  convRule: rule14
}, {
  start: 67594,
  length: 44,
  convRule: rule14
}, {
  start: 67639,
  length: 2,
  convRule: rule14
}, {
  start: 67644,
  length: 1,
  convRule: rule14
}, {
  start: 67647,
  length: 23,
  convRule: rule14
}, {
  start: 67671,
  length: 1,
  convRule: rule2
}, {
  start: 67672,
  length: 8,
  convRule: rule17
}, {
  start: 67680,
  length: 23,
  convRule: rule14
}, {
  start: 67703,
  length: 2,
  convRule: rule13
}, {
  start: 67705,
  length: 7,
  convRule: rule17
}, {
  start: 67712,
  length: 31,
  convRule: rule14
}, {
  start: 67751,
  length: 9,
  convRule: rule17
}, {
  start: 67808,
  length: 19,
  convRule: rule14
}, {
  start: 67828,
  length: 2,
  convRule: rule14
}, {
  start: 67835,
  length: 5,
  convRule: rule17
}, {
  start: 67840,
  length: 22,
  convRule: rule14
}, {
  start: 67862,
  length: 6,
  convRule: rule17
}, {
  start: 67871,
  length: 1,
  convRule: rule2
}, {
  start: 67872,
  length: 26,
  convRule: rule14
}, {
  start: 67903,
  length: 1,
  convRule: rule2
}, {
  start: 67968,
  length: 56,
  convRule: rule14
}, {
  start: 68028,
  length: 2,
  convRule: rule17
}, {
  start: 68030,
  length: 2,
  convRule: rule14
}, {
  start: 68032,
  length: 16,
  convRule: rule17
}, {
  start: 68050,
  length: 46,
  convRule: rule17
}, {
  start: 68096,
  length: 1,
  convRule: rule14
}, {
  start: 68097,
  length: 3,
  convRule: rule92
}, {
  start: 68101,
  length: 2,
  convRule: rule92
}, {
  start: 68108,
  length: 4,
  convRule: rule92
}, {
  start: 68112,
  length: 4,
  convRule: rule14
}, {
  start: 68117,
  length: 3,
  convRule: rule14
}, {
  start: 68121,
  length: 29,
  convRule: rule14
}, {
  start: 68152,
  length: 3,
  convRule: rule92
}, {
  start: 68159,
  length: 1,
  convRule: rule92
}, {
  start: 68160,
  length: 9,
  convRule: rule17
}, {
  start: 68176,
  length: 9,
  convRule: rule2
}, {
  start: 68192,
  length: 29,
  convRule: rule14
}, {
  start: 68221,
  length: 2,
  convRule: rule17
}, {
  start: 68223,
  length: 1,
  convRule: rule2
}, {
  start: 68224,
  length: 29,
  convRule: rule14
}, {
  start: 68253,
  length: 3,
  convRule: rule17
}, {
  start: 68288,
  length: 8,
  convRule: rule14
}, {
  start: 68296,
  length: 1,
  convRule: rule13
}, {
  start: 68297,
  length: 28,
  convRule: rule14
}, {
  start: 68325,
  length: 2,
  convRule: rule92
}, {
  start: 68331,
  length: 5,
  convRule: rule17
}, {
  start: 68336,
  length: 7,
  convRule: rule2
}, {
  start: 68352,
  length: 54,
  convRule: rule14
}, {
  start: 68409,
  length: 7,
  convRule: rule2
}, {
  start: 68416,
  length: 22,
  convRule: rule14
}, {
  start: 68440,
  length: 8,
  convRule: rule17
}, {
  start: 68448,
  length: 19,
  convRule: rule14
}, {
  start: 68472,
  length: 8,
  convRule: rule17
}, {
  start: 68480,
  length: 18,
  convRule: rule14
}, {
  start: 68505,
  length: 4,
  convRule: rule2
}, {
  start: 68521,
  length: 7,
  convRule: rule17
}, {
  start: 68608,
  length: 73,
  convRule: rule14
}, {
  start: 68736,
  length: 51,
  convRule: rule97
}, {
  start: 68800,
  length: 51,
  convRule: rule102
}, {
  start: 68858,
  length: 6,
  convRule: rule17
}, {
  start: 68864,
  length: 36,
  convRule: rule14
}, {
  start: 68900,
  length: 4,
  convRule: rule92
}, {
  start: 68912,
  length: 10,
  convRule: rule8
}, {
  start: 69216,
  length: 31,
  convRule: rule17
}, {
  start: 69248,
  length: 42,
  convRule: rule14
}, {
  start: 69291,
  length: 2,
  convRule: rule92
}, {
  start: 69293,
  length: 1,
  convRule: rule7
}, {
  start: 69296,
  length: 2,
  convRule: rule14
}, {
  start: 69376,
  length: 29,
  convRule: rule14
}, {
  start: 69405,
  length: 10,
  convRule: rule17
}, {
  start: 69415,
  length: 1,
  convRule: rule14
}, {
  start: 69424,
  length: 22,
  convRule: rule14
}, {
  start: 69446,
  length: 11,
  convRule: rule92
}, {
  start: 69457,
  length: 4,
  convRule: rule17
}, {
  start: 69461,
  length: 5,
  convRule: rule2
}, {
  start: 69552,
  length: 21,
  convRule: rule14
}, {
  start: 69573,
  length: 7,
  convRule: rule17
}, {
  start: 69600,
  length: 23,
  convRule: rule14
}, {
  start: 69632,
  length: 1,
  convRule: rule124
}, {
  start: 69633,
  length: 1,
  convRule: rule92
}, {
  start: 69634,
  length: 1,
  convRule: rule124
}, {
  start: 69635,
  length: 53,
  convRule: rule14
}, {
  start: 69688,
  length: 15,
  convRule: rule92
}, {
  start: 69703,
  length: 7,
  convRule: rule2
}, {
  start: 69714,
  length: 20,
  convRule: rule17
}, {
  start: 69734,
  length: 10,
  convRule: rule8
}, {
  start: 69759,
  length: 3,
  convRule: rule92
}, {
  start: 69762,
  length: 1,
  convRule: rule124
}, {
  start: 69763,
  length: 45,
  convRule: rule14
}, {
  start: 69808,
  length: 3,
  convRule: rule124
}, {
  start: 69811,
  length: 4,
  convRule: rule92
}, {
  start: 69815,
  length: 2,
  convRule: rule124
}, {
  start: 69817,
  length: 2,
  convRule: rule92
}, {
  start: 69819,
  length: 2,
  convRule: rule2
}, {
  start: 69821,
  length: 1,
  convRule: rule16
}, {
  start: 69822,
  length: 4,
  convRule: rule2
}, {
  start: 69837,
  length: 1,
  convRule: rule16
}, {
  start: 69840,
  length: 25,
  convRule: rule14
}, {
  start: 69872,
  length: 10,
  convRule: rule8
}, {
  start: 69888,
  length: 3,
  convRule: rule92
}, {
  start: 69891,
  length: 36,
  convRule: rule14
}, {
  start: 69927,
  length: 5,
  convRule: rule92
}, {
  start: 69932,
  length: 1,
  convRule: rule124
}, {
  start: 69933,
  length: 8,
  convRule: rule92
}, {
  start: 69942,
  length: 10,
  convRule: rule8
}, {
  start: 69952,
  length: 4,
  convRule: rule2
}, {
  start: 69956,
  length: 1,
  convRule: rule14
}, {
  start: 69957,
  length: 2,
  convRule: rule124
}, {
  start: 69959,
  length: 1,
  convRule: rule14
}, {
  start: 69968,
  length: 35,
  convRule: rule14
}, {
  start: 70003,
  length: 1,
  convRule: rule92
}, {
  start: 70004,
  length: 2,
  convRule: rule2
}, {
  start: 70006,
  length: 1,
  convRule: rule14
}, {
  start: 70016,
  length: 2,
  convRule: rule92
}, {
  start: 70018,
  length: 1,
  convRule: rule124
}, {
  start: 70019,
  length: 48,
  convRule: rule14
}, {
  start: 70067,
  length: 3,
  convRule: rule124
}, {
  start: 70070,
  length: 9,
  convRule: rule92
}, {
  start: 70079,
  length: 2,
  convRule: rule124
}, {
  start: 70081,
  length: 4,
  convRule: rule14
}, {
  start: 70085,
  length: 4,
  convRule: rule2
}, {
  start: 70089,
  length: 4,
  convRule: rule92
}, {
  start: 70093,
  length: 1,
  convRule: rule2
}, {
  start: 70094,
  length: 1,
  convRule: rule124
}, {
  start: 70095,
  length: 1,
  convRule: rule92
}, {
  start: 70096,
  length: 10,
  convRule: rule8
}, {
  start: 70106,
  length: 1,
  convRule: rule14
}, {
  start: 70107,
  length: 1,
  convRule: rule2
}, {
  start: 70108,
  length: 1,
  convRule: rule14
}, {
  start: 70109,
  length: 3,
  convRule: rule2
}, {
  start: 70113,
  length: 20,
  convRule: rule17
}, {
  start: 70144,
  length: 18,
  convRule: rule14
}, {
  start: 70163,
  length: 25,
  convRule: rule14
}, {
  start: 70188,
  length: 3,
  convRule: rule124
}, {
  start: 70191,
  length: 3,
  convRule: rule92
}, {
  start: 70194,
  length: 2,
  convRule: rule124
}, {
  start: 70196,
  length: 1,
  convRule: rule92
}, {
  start: 70197,
  length: 1,
  convRule: rule124
}, {
  start: 70198,
  length: 2,
  convRule: rule92
}, {
  start: 70200,
  length: 6,
  convRule: rule2
}, {
  start: 70206,
  length: 1,
  convRule: rule92
}, {
  start: 70272,
  length: 7,
  convRule: rule14
}, {
  start: 70280,
  length: 1,
  convRule: rule14
}, {
  start: 70282,
  length: 4,
  convRule: rule14
}, {
  start: 70287,
  length: 15,
  convRule: rule14
}, {
  start: 70303,
  length: 10,
  convRule: rule14
}, {
  start: 70313,
  length: 1,
  convRule: rule2
}, {
  start: 70320,
  length: 47,
  convRule: rule14
}, {
  start: 70367,
  length: 1,
  convRule: rule92
}, {
  start: 70368,
  length: 3,
  convRule: rule124
}, {
  start: 70371,
  length: 8,
  convRule: rule92
}, {
  start: 70384,
  length: 10,
  convRule: rule8
}, {
  start: 70400,
  length: 2,
  convRule: rule92
}, {
  start: 70402,
  length: 2,
  convRule: rule124
}, {
  start: 70405,
  length: 8,
  convRule: rule14
}, {
  start: 70415,
  length: 2,
  convRule: rule14
}, {
  start: 70419,
  length: 22,
  convRule: rule14
}, {
  start: 70442,
  length: 7,
  convRule: rule14
}, {
  start: 70450,
  length: 2,
  convRule: rule14
}, {
  start: 70453,
  length: 5,
  convRule: rule14
}, {
  start: 70459,
  length: 2,
  convRule: rule92
}, {
  start: 70461,
  length: 1,
  convRule: rule14
}, {
  start: 70462,
  length: 2,
  convRule: rule124
}, {
  start: 70464,
  length: 1,
  convRule: rule92
}, {
  start: 70465,
  length: 4,
  convRule: rule124
}, {
  start: 70471,
  length: 2,
  convRule: rule124
}, {
  start: 70475,
  length: 3,
  convRule: rule124
}, {
  start: 70480,
  length: 1,
  convRule: rule14
}, {
  start: 70487,
  length: 1,
  convRule: rule124
}, {
  start: 70493,
  length: 5,
  convRule: rule14
}, {
  start: 70498,
  length: 2,
  convRule: rule124
}, {
  start: 70502,
  length: 7,
  convRule: rule92
}, {
  start: 70512,
  length: 5,
  convRule: rule92
}, {
  start: 70656,
  length: 53,
  convRule: rule14
}, {
  start: 70709,
  length: 3,
  convRule: rule124
}, {
  start: 70712,
  length: 8,
  convRule: rule92
}, {
  start: 70720,
  length: 2,
  convRule: rule124
}, {
  start: 70722,
  length: 3,
  convRule: rule92
}, {
  start: 70725,
  length: 1,
  convRule: rule124
}, {
  start: 70726,
  length: 1,
  convRule: rule92
}, {
  start: 70727,
  length: 4,
  convRule: rule14
}, {
  start: 70731,
  length: 5,
  convRule: rule2
}, {
  start: 70736,
  length: 10,
  convRule: rule8
}, {
  start: 70746,
  length: 2,
  convRule: rule2
}, {
  start: 70749,
  length: 1,
  convRule: rule2
}, {
  start: 70750,
  length: 1,
  convRule: rule92
}, {
  start: 70751,
  length: 3,
  convRule: rule14
}, {
  start: 70784,
  length: 48,
  convRule: rule14
}, {
  start: 70832,
  length: 3,
  convRule: rule124
}, {
  start: 70835,
  length: 6,
  convRule: rule92
}, {
  start: 70841,
  length: 1,
  convRule: rule124
}, {
  start: 70842,
  length: 1,
  convRule: rule92
}, {
  start: 70843,
  length: 4,
  convRule: rule124
}, {
  start: 70847,
  length: 2,
  convRule: rule92
}, {
  start: 70849,
  length: 1,
  convRule: rule124
}, {
  start: 70850,
  length: 2,
  convRule: rule92
}, {
  start: 70852,
  length: 2,
  convRule: rule14
}, {
  start: 70854,
  length: 1,
  convRule: rule2
}, {
  start: 70855,
  length: 1,
  convRule: rule14
}, {
  start: 70864,
  length: 10,
  convRule: rule8
}, {
  start: 71040,
  length: 47,
  convRule: rule14
}, {
  start: 71087,
  length: 3,
  convRule: rule124
}, {
  start: 71090,
  length: 4,
  convRule: rule92
}, {
  start: 71096,
  length: 4,
  convRule: rule124
}, {
  start: 71100,
  length: 2,
  convRule: rule92
}, {
  start: 71102,
  length: 1,
  convRule: rule124
}, {
  start: 71103,
  length: 2,
  convRule: rule92
}, {
  start: 71105,
  length: 23,
  convRule: rule2
}, {
  start: 71128,
  length: 4,
  convRule: rule14
}, {
  start: 71132,
  length: 2,
  convRule: rule92
}, {
  start: 71168,
  length: 48,
  convRule: rule14
}, {
  start: 71216,
  length: 3,
  convRule: rule124
}, {
  start: 71219,
  length: 8,
  convRule: rule92
}, {
  start: 71227,
  length: 2,
  convRule: rule124
}, {
  start: 71229,
  length: 1,
  convRule: rule92
}, {
  start: 71230,
  length: 1,
  convRule: rule124
}, {
  start: 71231,
  length: 2,
  convRule: rule92
}, {
  start: 71233,
  length: 3,
  convRule: rule2
}, {
  start: 71236,
  length: 1,
  convRule: rule14
}, {
  start: 71248,
  length: 10,
  convRule: rule8
}, {
  start: 71264,
  length: 13,
  convRule: rule2
}, {
  start: 71296,
  length: 43,
  convRule: rule14
}, {
  start: 71339,
  length: 1,
  convRule: rule92
}, {
  start: 71340,
  length: 1,
  convRule: rule124
}, {
  start: 71341,
  length: 1,
  convRule: rule92
}, {
  start: 71342,
  length: 2,
  convRule: rule124
}, {
  start: 71344,
  length: 6,
  convRule: rule92
}, {
  start: 71350,
  length: 1,
  convRule: rule124
}, {
  start: 71351,
  length: 1,
  convRule: rule92
}, {
  start: 71352,
  length: 1,
  convRule: rule14
}, {
  start: 71360,
  length: 10,
  convRule: rule8
}, {
  start: 71424,
  length: 27,
  convRule: rule14
}, {
  start: 71453,
  length: 3,
  convRule: rule92
}, {
  start: 71456,
  length: 2,
  convRule: rule124
}, {
  start: 71458,
  length: 4,
  convRule: rule92
}, {
  start: 71462,
  length: 1,
  convRule: rule124
}, {
  start: 71463,
  length: 5,
  convRule: rule92
}, {
  start: 71472,
  length: 10,
  convRule: rule8
}, {
  start: 71482,
  length: 2,
  convRule: rule17
}, {
  start: 71484,
  length: 3,
  convRule: rule2
}, {
  start: 71487,
  length: 1,
  convRule: rule13
}, {
  start: 71680,
  length: 44,
  convRule: rule14
}, {
  start: 71724,
  length: 3,
  convRule: rule124
}, {
  start: 71727,
  length: 9,
  convRule: rule92
}, {
  start: 71736,
  length: 1,
  convRule: rule124
}, {
  start: 71737,
  length: 2,
  convRule: rule92
}, {
  start: 71739,
  length: 1,
  convRule: rule2
}, {
  start: 71840,
  length: 32,
  convRule: rule9
}, {
  start: 71872,
  length: 32,
  convRule: rule12
}, {
  start: 71904,
  length: 10,
  convRule: rule8
}, {
  start: 71914,
  length: 9,
  convRule: rule17
}, {
  start: 71935,
  length: 8,
  convRule: rule14
}, {
  start: 71945,
  length: 1,
  convRule: rule14
}, {
  start: 71948,
  length: 8,
  convRule: rule14
}, {
  start: 71957,
  length: 2,
  convRule: rule14
}, {
  start: 71960,
  length: 24,
  convRule: rule14
}, {
  start: 71984,
  length: 6,
  convRule: rule124
}, {
  start: 71991,
  length: 2,
  convRule: rule124
}, {
  start: 71995,
  length: 2,
  convRule: rule92
}, {
  start: 71997,
  length: 1,
  convRule: rule124
}, {
  start: 71998,
  length: 1,
  convRule: rule92
}, {
  start: 71999,
  length: 1,
  convRule: rule14
}, {
  start: 72e3,
  length: 1,
  convRule: rule124
}, {
  start: 72001,
  length: 1,
  convRule: rule14
}, {
  start: 72002,
  length: 1,
  convRule: rule124
}, {
  start: 72003,
  length: 1,
  convRule: rule92
}, {
  start: 72004,
  length: 3,
  convRule: rule2
}, {
  start: 72016,
  length: 10,
  convRule: rule8
}, {
  start: 72096,
  length: 8,
  convRule: rule14
}, {
  start: 72106,
  length: 39,
  convRule: rule14
}, {
  start: 72145,
  length: 3,
  convRule: rule124
}, {
  start: 72148,
  length: 4,
  convRule: rule92
}, {
  start: 72154,
  length: 2,
  convRule: rule92
}, {
  start: 72156,
  length: 4,
  convRule: rule124
}, {
  start: 72160,
  length: 1,
  convRule: rule92
}, {
  start: 72161,
  length: 1,
  convRule: rule14
}, {
  start: 72162,
  length: 1,
  convRule: rule2
}, {
  start: 72163,
  length: 1,
  convRule: rule14
}, {
  start: 72164,
  length: 1,
  convRule: rule124
}, {
  start: 72192,
  length: 1,
  convRule: rule14
}, {
  start: 72193,
  length: 10,
  convRule: rule92
}, {
  start: 72203,
  length: 40,
  convRule: rule14
}, {
  start: 72243,
  length: 6,
  convRule: rule92
}, {
  start: 72249,
  length: 1,
  convRule: rule124
}, {
  start: 72250,
  length: 1,
  convRule: rule14
}, {
  start: 72251,
  length: 4,
  convRule: rule92
}, {
  start: 72255,
  length: 8,
  convRule: rule2
}, {
  start: 72263,
  length: 1,
  convRule: rule92
}, {
  start: 72272,
  length: 1,
  convRule: rule14
}, {
  start: 72273,
  length: 6,
  convRule: rule92
}, {
  start: 72279,
  length: 2,
  convRule: rule124
}, {
  start: 72281,
  length: 3,
  convRule: rule92
}, {
  start: 72284,
  length: 46,
  convRule: rule14
}, {
  start: 72330,
  length: 13,
  convRule: rule92
}, {
  start: 72343,
  length: 1,
  convRule: rule124
}, {
  start: 72344,
  length: 2,
  convRule: rule92
}, {
  start: 72346,
  length: 3,
  convRule: rule2
}, {
  start: 72349,
  length: 1,
  convRule: rule14
}, {
  start: 72350,
  length: 5,
  convRule: rule2
}, {
  start: 72384,
  length: 57,
  convRule: rule14
}, {
  start: 72704,
  length: 9,
  convRule: rule14
}, {
  start: 72714,
  length: 37,
  convRule: rule14
}, {
  start: 72751,
  length: 1,
  convRule: rule124
}, {
  start: 72752,
  length: 7,
  convRule: rule92
}, {
  start: 72760,
  length: 6,
  convRule: rule92
}, {
  start: 72766,
  length: 1,
  convRule: rule124
}, {
  start: 72767,
  length: 1,
  convRule: rule92
}, {
  start: 72768,
  length: 1,
  convRule: rule14
}, {
  start: 72769,
  length: 5,
  convRule: rule2
}, {
  start: 72784,
  length: 10,
  convRule: rule8
}, {
  start: 72794,
  length: 19,
  convRule: rule17
}, {
  start: 72816,
  length: 2,
  convRule: rule2
}, {
  start: 72818,
  length: 30,
  convRule: rule14
}, {
  start: 72850,
  length: 22,
  convRule: rule92
}, {
  start: 72873,
  length: 1,
  convRule: rule124
}, {
  start: 72874,
  length: 7,
  convRule: rule92
}, {
  start: 72881,
  length: 1,
  convRule: rule124
}, {
  start: 72882,
  length: 2,
  convRule: rule92
}, {
  start: 72884,
  length: 1,
  convRule: rule124
}, {
  start: 72885,
  length: 2,
  convRule: rule92
}, {
  start: 72960,
  length: 7,
  convRule: rule14
}, {
  start: 72968,
  length: 2,
  convRule: rule14
}, {
  start: 72971,
  length: 38,
  convRule: rule14
}, {
  start: 73009,
  length: 6,
  convRule: rule92
}, {
  start: 73018,
  length: 1,
  convRule: rule92
}, {
  start: 73020,
  length: 2,
  convRule: rule92
}, {
  start: 73023,
  length: 7,
  convRule: rule92
}, {
  start: 73030,
  length: 1,
  convRule: rule14
}, {
  start: 73031,
  length: 1,
  convRule: rule92
}, {
  start: 73040,
  length: 10,
  convRule: rule8
}, {
  start: 73056,
  length: 6,
  convRule: rule14
}, {
  start: 73063,
  length: 2,
  convRule: rule14
}, {
  start: 73066,
  length: 32,
  convRule: rule14
}, {
  start: 73098,
  length: 5,
  convRule: rule124
}, {
  start: 73104,
  length: 2,
  convRule: rule92
}, {
  start: 73107,
  length: 2,
  convRule: rule124
}, {
  start: 73109,
  length: 1,
  convRule: rule92
}, {
  start: 73110,
  length: 1,
  convRule: rule124
}, {
  start: 73111,
  length: 1,
  convRule: rule92
}, {
  start: 73112,
  length: 1,
  convRule: rule14
}, {
  start: 73120,
  length: 10,
  convRule: rule8
}, {
  start: 73440,
  length: 19,
  convRule: rule14
}, {
  start: 73459,
  length: 2,
  convRule: rule92
}, {
  start: 73461,
  length: 2,
  convRule: rule124
}, {
  start: 73463,
  length: 2,
  convRule: rule2
}, {
  start: 73648,
  length: 1,
  convRule: rule14
}, {
  start: 73664,
  length: 21,
  convRule: rule17
}, {
  start: 73685,
  length: 8,
  convRule: rule13
}, {
  start: 73693,
  length: 4,
  convRule: rule3
}, {
  start: 73697,
  length: 17,
  convRule: rule13
}, {
  start: 73727,
  length: 1,
  convRule: rule2
}, {
  start: 73728,
  length: 922,
  convRule: rule14
}, {
  start: 74752,
  length: 111,
  convRule: rule128
}, {
  start: 74864,
  length: 5,
  convRule: rule2
}, {
  start: 74880,
  length: 196,
  convRule: rule14
}, {
  start: 77824,
  length: 1071,
  convRule: rule14
}, {
  start: 78896,
  length: 9,
  convRule: rule16
}, {
  start: 82944,
  length: 583,
  convRule: rule14
}, {
  start: 92160,
  length: 569,
  convRule: rule14
}, {
  start: 92736,
  length: 31,
  convRule: rule14
}, {
  start: 92768,
  length: 10,
  convRule: rule8
}, {
  start: 92782,
  length: 2,
  convRule: rule2
}, {
  start: 92880,
  length: 30,
  convRule: rule14
}, {
  start: 92912,
  length: 5,
  convRule: rule92
}, {
  start: 92917,
  length: 1,
  convRule: rule2
}, {
  start: 92928,
  length: 48,
  convRule: rule14
}, {
  start: 92976,
  length: 7,
  convRule: rule92
}, {
  start: 92983,
  length: 5,
  convRule: rule2
}, {
  start: 92988,
  length: 4,
  convRule: rule13
}, {
  start: 92992,
  length: 4,
  convRule: rule91
}, {
  start: 92996,
  length: 1,
  convRule: rule2
}, {
  start: 92997,
  length: 1,
  convRule: rule13
}, {
  start: 93008,
  length: 10,
  convRule: rule8
}, {
  start: 93019,
  length: 7,
  convRule: rule17
}, {
  start: 93027,
  length: 21,
  convRule: rule14
}, {
  start: 93053,
  length: 19,
  convRule: rule14
}, {
  start: 93760,
  length: 32,
  convRule: rule9
}, {
  start: 93792,
  length: 32,
  convRule: rule12
}, {
  start: 93824,
  length: 23,
  convRule: rule17
}, {
  start: 93847,
  length: 4,
  convRule: rule2
}, {
  start: 93952,
  length: 75,
  convRule: rule14
}, {
  start: 94031,
  length: 1,
  convRule: rule92
}, {
  start: 94032,
  length: 1,
  convRule: rule14
}, {
  start: 94033,
  length: 55,
  convRule: rule124
}, {
  start: 94095,
  length: 4,
  convRule: rule92
}, {
  start: 94099,
  length: 13,
  convRule: rule91
}, {
  start: 94176,
  length: 2,
  convRule: rule91
}, {
  start: 94178,
  length: 1,
  convRule: rule2
}, {
  start: 94179,
  length: 1,
  convRule: rule91
}, {
  start: 94180,
  length: 1,
  convRule: rule92
}, {
  start: 94192,
  length: 2,
  convRule: rule124
}, {
  start: 94208,
  length: 6136,
  convRule: rule14
}, {
  start: 100352,
  length: 1238,
  convRule: rule14
}, {
  start: 101632,
  length: 9,
  convRule: rule14
}, {
  start: 110592,
  length: 287,
  convRule: rule14
}, {
  start: 110928,
  length: 3,
  convRule: rule14
}, {
  start: 110948,
  length: 4,
  convRule: rule14
}, {
  start: 110960,
  length: 396,
  convRule: rule14
}, {
  start: 113664,
  length: 107,
  convRule: rule14
}, {
  start: 113776,
  length: 13,
  convRule: rule14
}, {
  start: 113792,
  length: 9,
  convRule: rule14
}, {
  start: 113808,
  length: 10,
  convRule: rule14
}, {
  start: 113820,
  length: 1,
  convRule: rule13
}, {
  start: 113821,
  length: 2,
  convRule: rule92
}, {
  start: 113823,
  length: 1,
  convRule: rule2
}, {
  start: 113824,
  length: 4,
  convRule: rule16
}, {
  start: 118784,
  length: 246,
  convRule: rule13
}, {
  start: 119040,
  length: 39,
  convRule: rule13
}, {
  start: 119081,
  length: 60,
  convRule: rule13
}, {
  start: 119141,
  length: 2,
  convRule: rule124
}, {
  start: 119143,
  length: 3,
  convRule: rule92
}, {
  start: 119146,
  length: 3,
  convRule: rule13
}, {
  start: 119149,
  length: 6,
  convRule: rule124
}, {
  start: 119155,
  length: 8,
  convRule: rule16
}, {
  start: 119163,
  length: 8,
  convRule: rule92
}, {
  start: 119171,
  length: 2,
  convRule: rule13
}, {
  start: 119173,
  length: 7,
  convRule: rule92
}, {
  start: 119180,
  length: 30,
  convRule: rule13
}, {
  start: 119210,
  length: 4,
  convRule: rule92
}, {
  start: 119214,
  length: 59,
  convRule: rule13
}, {
  start: 119296,
  length: 66,
  convRule: rule13
}, {
  start: 119362,
  length: 3,
  convRule: rule92
}, {
  start: 119365,
  length: 1,
  convRule: rule13
}, {
  start: 119520,
  length: 20,
  convRule: rule17
}, {
  start: 119552,
  length: 87,
  convRule: rule13
}, {
  start: 119648,
  length: 25,
  convRule: rule17
}, {
  start: 119808,
  length: 26,
  convRule: rule107
}, {
  start: 119834,
  length: 26,
  convRule: rule20
}, {
  start: 119860,
  length: 26,
  convRule: rule107
}, {
  start: 119886,
  length: 7,
  convRule: rule20
}, {
  start: 119894,
  length: 18,
  convRule: rule20
}, {
  start: 119912,
  length: 26,
  convRule: rule107
}, {
  start: 119938,
  length: 26,
  convRule: rule20
}, {
  start: 119964,
  length: 1,
  convRule: rule107
}, {
  start: 119966,
  length: 2,
  convRule: rule107
}, {
  start: 119970,
  length: 1,
  convRule: rule107
}, {
  start: 119973,
  length: 2,
  convRule: rule107
}, {
  start: 119977,
  length: 4,
  convRule: rule107
}, {
  start: 119982,
  length: 8,
  convRule: rule107
}, {
  start: 119990,
  length: 4,
  convRule: rule20
}, {
  start: 119995,
  length: 1,
  convRule: rule20
}, {
  start: 119997,
  length: 7,
  convRule: rule20
}, {
  start: 120005,
  length: 11,
  convRule: rule20
}, {
  start: 120016,
  length: 26,
  convRule: rule107
}, {
  start: 120042,
  length: 26,
  convRule: rule20
}, {
  start: 120068,
  length: 2,
  convRule: rule107
}, {
  start: 120071,
  length: 4,
  convRule: rule107
}, {
  start: 120077,
  length: 8,
  convRule: rule107
}, {
  start: 120086,
  length: 7,
  convRule: rule107
}, {
  start: 120094,
  length: 26,
  convRule: rule20
}, {
  start: 120120,
  length: 2,
  convRule: rule107
}, {
  start: 120123,
  length: 4,
  convRule: rule107
}, {
  start: 120128,
  length: 5,
  convRule: rule107
}, {
  start: 120134,
  length: 1,
  convRule: rule107
}, {
  start: 120138,
  length: 7,
  convRule: rule107
}, {
  start: 120146,
  length: 26,
  convRule: rule20
}, {
  start: 120172,
  length: 26,
  convRule: rule107
}, {
  start: 120198,
  length: 26,
  convRule: rule20
}, {
  start: 120224,
  length: 26,
  convRule: rule107
}, {
  start: 120250,
  length: 26,
  convRule: rule20
}, {
  start: 120276,
  length: 26,
  convRule: rule107
}, {
  start: 120302,
  length: 26,
  convRule: rule20
}, {
  start: 120328,
  length: 26,
  convRule: rule107
}, {
  start: 120354,
  length: 26,
  convRule: rule20
}, {
  start: 120380,
  length: 26,
  convRule: rule107
}, {
  start: 120406,
  length: 26,
  convRule: rule20
}, {
  start: 120432,
  length: 26,
  convRule: rule107
}, {
  start: 120458,
  length: 28,
  convRule: rule20
}, {
  start: 120488,
  length: 25,
  convRule: rule107
}, {
  start: 120513,
  length: 1,
  convRule: rule6
}, {
  start: 120514,
  length: 25,
  convRule: rule20
}, {
  start: 120539,
  length: 1,
  convRule: rule6
}, {
  start: 120540,
  length: 6,
  convRule: rule20
}, {
  start: 120546,
  length: 25,
  convRule: rule107
}, {
  start: 120571,
  length: 1,
  convRule: rule6
}, {
  start: 120572,
  length: 25,
  convRule: rule20
}, {
  start: 120597,
  length: 1,
  convRule: rule6
}, {
  start: 120598,
  length: 6,
  convRule: rule20
}, {
  start: 120604,
  length: 25,
  convRule: rule107
}, {
  start: 120629,
  length: 1,
  convRule: rule6
}, {
  start: 120630,
  length: 25,
  convRule: rule20
}, {
  start: 120655,
  length: 1,
  convRule: rule6
}, {
  start: 120656,
  length: 6,
  convRule: rule20
}, {
  start: 120662,
  length: 25,
  convRule: rule107
}, {
  start: 120687,
  length: 1,
  convRule: rule6
}, {
  start: 120688,
  length: 25,
  convRule: rule20
}, {
  start: 120713,
  length: 1,
  convRule: rule6
}, {
  start: 120714,
  length: 6,
  convRule: rule20
}, {
  start: 120720,
  length: 25,
  convRule: rule107
}, {
  start: 120745,
  length: 1,
  convRule: rule6
}, {
  start: 120746,
  length: 25,
  convRule: rule20
}, {
  start: 120771,
  length: 1,
  convRule: rule6
}, {
  start: 120772,
  length: 6,
  convRule: rule20
}, {
  start: 120778,
  length: 1,
  convRule: rule107
}, {
  start: 120779,
  length: 1,
  convRule: rule20
}, {
  start: 120782,
  length: 50,
  convRule: rule8
}, {
  start: 120832,
  length: 512,
  convRule: rule13
}, {
  start: 121344,
  length: 55,
  convRule: rule92
}, {
  start: 121399,
  length: 4,
  convRule: rule13
}, {
  start: 121403,
  length: 50,
  convRule: rule92
}, {
  start: 121453,
  length: 8,
  convRule: rule13
}, {
  start: 121461,
  length: 1,
  convRule: rule92
}, {
  start: 121462,
  length: 14,
  convRule: rule13
}, {
  start: 121476,
  length: 1,
  convRule: rule92
}, {
  start: 121477,
  length: 2,
  convRule: rule13
}, {
  start: 121479,
  length: 5,
  convRule: rule2
}, {
  start: 121499,
  length: 5,
  convRule: rule92
}, {
  start: 121505,
  length: 15,
  convRule: rule92
}, {
  start: 122880,
  length: 7,
  convRule: rule92
}, {
  start: 122888,
  length: 17,
  convRule: rule92
}, {
  start: 122907,
  length: 7,
  convRule: rule92
}, {
  start: 122915,
  length: 2,
  convRule: rule92
}, {
  start: 122918,
  length: 5,
  convRule: rule92
}, {
  start: 123136,
  length: 45,
  convRule: rule14
}, {
  start: 123184,
  length: 7,
  convRule: rule92
}, {
  start: 123191,
  length: 7,
  convRule: rule91
}, {
  start: 123200,
  length: 10,
  convRule: rule8
}, {
  start: 123214,
  length: 1,
  convRule: rule14
}, {
  start: 123215,
  length: 1,
  convRule: rule13
}, {
  start: 123584,
  length: 44,
  convRule: rule14
}, {
  start: 123628,
  length: 4,
  convRule: rule92
}, {
  start: 123632,
  length: 10,
  convRule: rule8
}, {
  start: 123647,
  length: 1,
  convRule: rule3
}, {
  start: 124928,
  length: 197,
  convRule: rule14
}, {
  start: 125127,
  length: 9,
  convRule: rule17
}, {
  start: 125136,
  length: 7,
  convRule: rule92
}, {
  start: 125184,
  length: 34,
  convRule: rule203
}, {
  start: 125218,
  length: 34,
  convRule: rule204
}, {
  start: 125252,
  length: 7,
  convRule: rule92
}, {
  start: 125259,
  length: 1,
  convRule: rule91
}, {
  start: 125264,
  length: 10,
  convRule: rule8
}, {
  start: 125278,
  length: 2,
  convRule: rule2
}, {
  start: 126065,
  length: 59,
  convRule: rule17
}, {
  start: 126124,
  length: 1,
  convRule: rule13
}, {
  start: 126125,
  length: 3,
  convRule: rule17
}, {
  start: 126128,
  length: 1,
  convRule: rule3
}, {
  start: 126129,
  length: 4,
  convRule: rule17
}, {
  start: 126209,
  length: 45,
  convRule: rule17
}, {
  start: 126254,
  length: 1,
  convRule: rule13
}, {
  start: 126255,
  length: 15,
  convRule: rule17
}, {
  start: 126464,
  length: 4,
  convRule: rule14
}, {
  start: 126469,
  length: 27,
  convRule: rule14
}, {
  start: 126497,
  length: 2,
  convRule: rule14
}, {
  start: 126500,
  length: 1,
  convRule: rule14
}, {
  start: 126503,
  length: 1,
  convRule: rule14
}, {
  start: 126505,
  length: 10,
  convRule: rule14
}, {
  start: 126516,
  length: 4,
  convRule: rule14
}, {
  start: 126521,
  length: 1,
  convRule: rule14
}, {
  start: 126523,
  length: 1,
  convRule: rule14
}, {
  start: 126530,
  length: 1,
  convRule: rule14
}, {
  start: 126535,
  length: 1,
  convRule: rule14
}, {
  start: 126537,
  length: 1,
  convRule: rule14
}, {
  start: 126539,
  length: 1,
  convRule: rule14
}, {
  start: 126541,
  length: 3,
  convRule: rule14
}, {
  start: 126545,
  length: 2,
  convRule: rule14
}, {
  start: 126548,
  length: 1,
  convRule: rule14
}, {
  start: 126551,
  length: 1,
  convRule: rule14
}, {
  start: 126553,
  length: 1,
  convRule: rule14
}, {
  start: 126555,
  length: 1,
  convRule: rule14
}, {
  start: 126557,
  length: 1,
  convRule: rule14
}, {
  start: 126559,
  length: 1,
  convRule: rule14
}, {
  start: 126561,
  length: 2,
  convRule: rule14
}, {
  start: 126564,
  length: 1,
  convRule: rule14
}, {
  start: 126567,
  length: 4,
  convRule: rule14
}, {
  start: 126572,
  length: 7,
  convRule: rule14
}, {
  start: 126580,
  length: 4,
  convRule: rule14
}, {
  start: 126585,
  length: 4,
  convRule: rule14
}, {
  start: 126590,
  length: 1,
  convRule: rule14
}, {
  start: 126592,
  length: 10,
  convRule: rule14
}, {
  start: 126603,
  length: 17,
  convRule: rule14
}, {
  start: 126625,
  length: 3,
  convRule: rule14
}, {
  start: 126629,
  length: 5,
  convRule: rule14
}, {
  start: 126635,
  length: 17,
  convRule: rule14
}, {
  start: 126704,
  length: 2,
  convRule: rule6
}, {
  start: 126976,
  length: 44,
  convRule: rule13
}, {
  start: 127024,
  length: 100,
  convRule: rule13
}, {
  start: 127136,
  length: 15,
  convRule: rule13
}, {
  start: 127153,
  length: 15,
  convRule: rule13
}, {
  start: 127169,
  length: 15,
  convRule: rule13
}, {
  start: 127185,
  length: 37,
  convRule: rule13
}, {
  start: 127232,
  length: 13,
  convRule: rule17
}, {
  start: 127245,
  length: 161,
  convRule: rule13
}, {
  start: 127462,
  length: 29,
  convRule: rule13
}, {
  start: 127504,
  length: 44,
  convRule: rule13
}, {
  start: 127552,
  length: 9,
  convRule: rule13
}, {
  start: 127568,
  length: 2,
  convRule: rule13
}, {
  start: 127584,
  length: 6,
  convRule: rule13
}, {
  start: 127744,
  length: 251,
  convRule: rule13
}, {
  start: 127995,
  length: 5,
  convRule: rule10
}, {
  start: 128e3,
  length: 728,
  convRule: rule13
}, {
  start: 128736,
  length: 13,
  convRule: rule13
}, {
  start: 128752,
  length: 13,
  convRule: rule13
}, {
  start: 128768,
  length: 116,
  convRule: rule13
}, {
  start: 128896,
  length: 89,
  convRule: rule13
}, {
  start: 128992,
  length: 12,
  convRule: rule13
}, {
  start: 129024,
  length: 12,
  convRule: rule13
}, {
  start: 129040,
  length: 56,
  convRule: rule13
}, {
  start: 129104,
  length: 10,
  convRule: rule13
}, {
  start: 129120,
  length: 40,
  convRule: rule13
}, {
  start: 129168,
  length: 30,
  convRule: rule13
}, {
  start: 129200,
  length: 2,
  convRule: rule13
}, {
  start: 129280,
  length: 121,
  convRule: rule13
}, {
  start: 129402,
  length: 82,
  convRule: rule13
}, {
  start: 129485,
  length: 135,
  convRule: rule13
}, {
  start: 129632,
  length: 14,
  convRule: rule13
}, {
  start: 129648,
  length: 5,
  convRule: rule13
}, {
  start: 129656,
  length: 3,
  convRule: rule13
}, {
  start: 129664,
  length: 7,
  convRule: rule13
}, {
  start: 129680,
  length: 25,
  convRule: rule13
}, {
  start: 129712,
  length: 7,
  convRule: rule13
}, {
  start: 129728,
  length: 3,
  convRule: rule13
}, {
  start: 129744,
  length: 7,
  convRule: rule13
}, {
  start: 129792,
  length: 147,
  convRule: rule13
}, {
  start: 129940,
  length: 55,
  convRule: rule13
}, {
  start: 130032,
  length: 10,
  convRule: rule8
}, {
  start: 131072,
  length: 42718,
  convRule: rule14
}, {
  start: 173824,
  length: 4149,
  convRule: rule14
}, {
  start: 177984,
  length: 222,
  convRule: rule14
}, {
  start: 178208,
  length: 5762,
  convRule: rule14
}, {
  start: 183984,
  length: 7473,
  convRule: rule14
}, {
  start: 194560,
  length: 542,
  convRule: rule14
}, {
  start: 196608,
  length: 4939,
  convRule: rule14
}, {
  start: 917505,
  length: 1,
  convRule: rule16
}, {
  start: 917536,
  length: 96,
  convRule: rule16
}, {
  start: 917760,
  length: 240,
  convRule: rule92
}, {
  start: 983040,
  length: 65534,
  convRule: rule200
}, {
  start: 1048576,
  length: 65534,
  convRule: rule200
}];
var checkAttr = function(categories) {
  return function($$char2) {
    var numOfBlocks = function() {
      var $43 = $$char2 < 256;
      if ($43) {
        return numLat1Blocks;
      }
      ;
      return numBlocks;
    }();
    var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
    if (maybeConversionRule instanceof Nothing) {
      return false;
    }
    ;
    if (maybeConversionRule instanceof Just) {
      return isJust(elemIndex3(maybeConversionRule.value0.category)(categories));
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
  };
};
var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);
var uIswalpha = /* @__PURE__ */ checkAttr([gencatLL, gencatLU, gencatLT, gencatLM, gencatLO]);
var uIswupper = /* @__PURE__ */ checkAttr([gencatLU, gencatLT]);

// output/Data.CodePoint.Unicode/index.js
var fromEnum10 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
var modify4 = unsafeCoerce2;
var toLowerSimple = /* @__PURE__ */ modify4(uTowlower);
var toUpperSimple = /* @__PURE__ */ modify4(uTowupper);
var isUpper = function($66) {
  return uIswupper(fromEnum10($66));
};
var isSpace = function(c) {
  var uc = fromEnum10(c);
  var $28 = uc <= 823;
  if ($28) {
    return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
  }
  ;
  return uIswspace(uc);
};
var isOctDigit = function(c) {
  var diff4 = fromEnum10(c) - toCharCode2("0") | 0;
  return diff4 <= 7 && diff4 >= 0;
};
var isDecDigit = function(c) {
  var diff4 = fromEnum10(c) - toCharCode2("0") | 0;
  return diff4 <= 9 && diff4 >= 0;
};
var isHexDigit = function(c) {
  return isDecDigit(c) || (function() {
    var diff4 = fromEnum10(c) - toCharCode2("A") | 0;
    return diff4 <= 5 && diff4 >= 0;
  }() || function() {
    var diff4 = fromEnum10(c) - toCharCode2("a") | 0;
    return diff4 <= 5 && diff4 >= 0;
  }());
};
var isAlphaNum = function($70) {
  return uIswalnum(fromEnum10($70));
};
var isAlpha = function($71) {
  return uIswalpha(fromEnum10($71));
};
var hexDigitToInt = function(c) {
  var hexUpper = fromEnum10(c) - toCharCode2("A") | 0;
  var hexLower = fromEnum10(c) - toCharCode2("a") | 0;
  var dec = fromEnum10(c) - toCharCode2("0") | 0;
  var result = function() {
    if (dec <= 9 && dec >= 0) {
      return new Just(dec);
    }
    ;
    if (hexLower <= 5 && hexLower >= 0) {
      return new Just(hexLower + 10 | 0);
    }
    ;
    if (hexUpper <= 5 && hexUpper >= 0) {
      return new Just(hexUpper + 10 | 0);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 591, column 3 - line 591, column 22): " + []);
  }();
  return result;
};

// output/Parsing.String.Basic/index.js
var elem1 = /* @__PURE__ */ elem2(eqChar);
var show13 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showChar));
var notElem1 = /* @__PURE__ */ notElem2(eqChar);
var satisfyCP = function(p) {
  return satisfy(function($32) {
    return p(codePointFromChar($32));
  });
};
var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
var upper2 = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isUpper))("uppercase letter");
var oneOf2 = function(ss) {
  return withLazyErrorMessage(satisfy(flip(elem1)(ss)))(function(v) {
    return "one of " + show13(ss);
  });
};
var octDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isOctDigit))("oct digit");
var noneOf = function(ss) {
  return withLazyErrorMessage(satisfy(flip(notElem1)(ss)))(function(v) {
    return "none of " + show13(ss);
  });
};
var letter = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlpha))("letter");
var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
var digit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isDecDigit))("digit");
var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

// output/Data.String.Unicode/index.js
var map9 = /* @__PURE__ */ map(functorArray);
var convert = function(f) {
  var $6 = map9(f);
  return function($7) {
    return fromCodePointArray($6(toCodePointArray($7)));
  };
};
var toLowerSimple2 = /* @__PURE__ */ convert(toLowerSimple);
var toUpperSimple2 = /* @__PURE__ */ convert(toUpperSimple);

// output/Parsing.Token/index.js
var bind7 = /* @__PURE__ */ bind(bindParserT);
var pure6 = /* @__PURE__ */ pure(applicativeParserT);
var sort2 = /* @__PURE__ */ sort(ordString);
var map10 = /* @__PURE__ */ map(functorArray);
var applySecond3 = /* @__PURE__ */ applySecond(applyParserT);
var compare3 = /* @__PURE__ */ compare(ordString);
var append3 = /* @__PURE__ */ append(semigroupArray);
var fix2 = /* @__PURE__ */ fix(lazyParserT);
var alt3 = /* @__PURE__ */ alt(altParserT);
var $$void2 = /* @__PURE__ */ $$void(functorParserT);
var voidLeft3 = /* @__PURE__ */ voidLeft(functorParserT);
var identity10 = /* @__PURE__ */ identity(categoryFn);
var many4 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
var map1 = /* @__PURE__ */ map(functorMaybe);
var some3 = /* @__PURE__ */ some(alternativeParserT)(lazyParserT);
var foldl3 = /* @__PURE__ */ foldl(foldableArray);
var applyFirst3 = /* @__PURE__ */ applyFirst(applyParserT);
var show5 = /* @__PURE__ */ show(showString);
var bind1 = /* @__PURE__ */ bind(bindMaybe);
var pure12 = /* @__PURE__ */ pure(applicativeMaybe);
var foldr2 = /* @__PURE__ */ foldr(foldableArray);
var map22 = /* @__PURE__ */ map(functorParserT);
var choice2 = /* @__PURE__ */ choice(foldableArray);
var many1 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
var toUnfoldable3 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
var foldr12 = /* @__PURE__ */ foldr(foldableList);
var unGenLanguageDef = function(v) {
  return v;
};
var theReservedNames = function(v) {
  if (v.caseSensitive) {
    return sort2(v.reservedNames);
  }
  ;
  if (otherwise) {
    return sort2(map10(toLower)(v.reservedNames));
  }
  ;
  throw new Error("Failed pattern match at Parsing.Token (line 825, column 1 - line 825, column 70): " + [v.constructor.name]);
};
var simpleSpace = /* @__PURE__ */ skipMany1(/* @__PURE__ */ satisfyCodePoint(isSpace));
var oneLineComment = function(v) {
  return applySecond3($$try(string(v.commentLine)))(skipMany(satisfy(function(v1) {
    return v1 !== "\n";
  })));
};
var isReserved = function($copy_names) {
  return function($copy_name) {
    var $tco_var_names = $copy_names;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(names, name2) {
      var v = uncons(names);
      if (v instanceof Nothing) {
        $tco_done = true;
        return false;
      }
      ;
      if (v instanceof Just) {
        var v1 = compare3(v.value0.head)(name2);
        if (v1 instanceof LT) {
          $tco_var_names = v.value0.tail;
          $copy_name = name2;
          return;
        }
        ;
        if (v1 instanceof EQ) {
          $tco_done = true;
          return true;
        }
        ;
        if (v1 instanceof GT) {
          $tco_done = true;
          return false;
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 820, column 35 - line 823, column 18): " + [v1.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 818, column 3 - line 823, column 18): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_names, $copy_name);
    }
    ;
    return $tco_result;
  };
};
var isReservedName = function(v) {
  return function(name2) {
    var caseName = function() {
      if (v.caseSensitive) {
        return name2;
      }
      ;
      if (otherwise) {
        return toLower(name2);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 812, column 3 - line 814, column 31): " + []);
    }();
    return isReserved(theReservedNames(v))(caseName);
  };
};
var inCommentSingle = function(v) {
  var startEnd = append3(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
  return fix2(function(p) {
    return alt3($$void2($$try(string(v.commentEnd))))(alt3(applySecond3(skipMany1(noneOf(startEnd)))(p))(withErrorMessage(applySecond3(oneOf2(startEnd))(p))("end of comment")));
  });
};
var multiLineComment = function(v) {
  return applySecond3($$try(string(v.commentStart)))(inComment(v));
};
var inCommentMulti = function(v) {
  var startEnd = append3(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
  return fix2(function(p) {
    return alt3($$void2($$try(string(v.commentEnd))))(alt3(applySecond3(multiLineComment(v))(p))(alt3(applySecond3(skipMany1(noneOf(startEnd)))(p))(withErrorMessage(applySecond3(oneOf2(startEnd))(p))("end of comment"))));
  });
};
var inComment = function(v) {
  if (v.nestedComments) {
    return inCommentMulti(v);
  }
  ;
  return inCommentSingle(v);
};
var whiteSpace$prime = function(v) {
  if ($$null(v.commentLine) && $$null(v.commentStart)) {
    return skipMany(withErrorMessage(simpleSpace)(""));
  }
  ;
  if ($$null(v.commentLine)) {
    return skipMany(alt3(simpleSpace)(withErrorMessage(multiLineComment(v))("")));
  }
  ;
  if ($$null(v.commentStart)) {
    return skipMany(alt3(simpleSpace)(withErrorMessage(oneLineComment(v))("")));
  }
  ;
  if (otherwise) {
    return skipMany(alt3(simpleSpace)(alt3(oneLineComment(v))(withErrorMessage(multiLineComment(v))(""))));
  }
  ;
  throw new Error("Failed pattern match at Parsing.Token (line 834, column 1 - line 834, column 74): " + [v.constructor.name]);
};
var makeTokenParser = function(v) {
  var stringLetter = satisfy(function(c) {
    return c !== '"' && (c !== "\\" && c > "");
  });
  var sign2 = function(dictRing) {
    return alt3(voidLeft3($$char("-"))(negate(dictRing)))(alt3(voidLeft3($$char("+"))(identity10))(pure6(identity10)));
  };
  var sign1 = sign2(ringInt);
  var oper = function() {
    var go = bind7(v.opStart)(function(c) {
      return bind7(many4(v.opLetter))(function(cs) {
        return pure6(singleton4(c) + fromCharArray(cs));
      });
    });
    return withErrorMessage(go)("operator");
  }();
  var number = function(base) {
    return function(baseDigit) {
      var folder = function(v1) {
        return function(v2) {
          if (v1 instanceof Nothing) {
            return Nothing.value;
          }
          ;
          if (v1 instanceof Just) {
            return map1(function(v3) {
              return (base * v1.value0 | 0) + v3 | 0;
            })(hexDigitToInt(codePointFromChar(v2)));
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 704, column 5 - line 704, column 45): " + [v1.constructor.name, v2.constructor.name]);
        };
      };
      return bind7(some3(baseDigit))(function(digits) {
        return maybe(fail("not digits"))(pure6)(foldl3(folder)(new Just(0))(digits));
      });
    };
  };
  var octal2 = applySecond3(oneOf2(["o", "O"]))(number(8)(octDigit));
  var lexeme = function(p) {
    return applyFirst3(p)(whiteSpace$prime(v));
  };
  var reservedOp2 = function(name2) {
    var go = bind7(string(name2))(function() {
      return withErrorMessage(notFollowedBy(v.opLetter))("end of " + name2);
    });
    return lexeme($$try(go));
  };
  var symbol = function(name2) {
    return voidLeft3(lexeme(string(name2)))(name2);
  };
  var parens2 = function(p) {
    return between(symbol("("))(symbol(")"))(p);
  };
  var semi = symbol(";");
  var semiSep2 = function(p) {
    return sepBy(p)(semi);
  };
  var semiSep1 = function(p) {
    return sepBy1(p)(semi);
  };
  var isReservedOp = function(name2) {
    return isReserved(sort2(v.reservedOpNames))(name2);
  };
  var operator = function() {
    var go = bind7(oper)(function(name2) {
      var $114 = isReservedOp(name2);
      if ($114) {
        return fail("reserved operator " + name2);
      }
      ;
      return pure6(name2);
    });
    return lexeme($$try(go));
  }();
  var ident = function() {
    var go = bind7(v.identStart)(function(c) {
      return bind7(many4(v.identLetter))(function(cs) {
        return pure6(singleton4(c) + fromCharArray(cs));
      });
    });
    return withErrorMessage(go)("identifier");
  }();
  var identifier2 = function() {
    var go = bind7(ident)(function(name2) {
      var $115 = isReservedName(v)(name2);
      if ($115) {
        return fail("reserved word " + show5(name2));
      }
      ;
      return pure6(name2);
    });
    return lexeme($$try(go));
  }();
  var hexadecimal2 = applySecond3(oneOf2(["x", "X"]))(number(16)(hexDigit));
  var fraction = function() {
    var op = function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (v2 instanceof Just) {
          return bind1(hexDigitToInt(codePointFromChar(v1)))(function(int$prime) {
            return pure12((v2.value0 + toNumber(int$prime)) / 10);
          });
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 651, column 5 - line 651, column 47): " + [v1.constructor.name, v2.constructor.name]);
      };
    };
    return asErrorMessage("fraction")(bind7($$char("."))(function() {
      return bind7(withErrorMessage(some3(digit))("fraction"))(function(digits) {
        return maybe(fail("not digit"))(pure6)(foldr2(op)(new Just(0))(digits));
      });
    }));
  }();
  var escapeGap = withErrorMessage(applySecond3(some3(space))($$char("\\")))("end of string gap");
  var escapeEmpty = $$char("&");
  var escMap = zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"]);
  var dot = symbol(".");
  var decimal2 = number(10)(digit);
  var exponent$prime = function() {
    var power = function(e) {
      if (e < 0) {
        return 1 / power(-e | 0);
      }
      ;
      if (otherwise) {
        return pow(10)(toNumber(e));
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 664, column 5 - line 664, column 27): " + [e.constructor.name]);
    };
    return asErrorMessage("exponent")(bind7(oneOf2(["e", "E"]))(function() {
      return bind7(sign1)(function(f) {
        return bind7(withErrorMessage(decimal2)("exponent"))(function(e) {
          return pure6(power(f(e)));
        });
      });
    }));
  }();
  var fractExponent = function(n) {
    var justExponent = bind7(exponent$prime)(function(expo) {
      return pure6(toNumber(n) * expo);
    });
    var fractExponent$prime = bind7(fraction)(function(fract) {
      return bind7(option(1)(exponent$prime))(function(expo) {
        return pure6((toNumber(n) + fract) * expo);
      });
    });
    return alt3(fractExponent$prime)(justExponent);
  };
  var fractFloat = function(n) {
    return map22(Right.create)(fractExponent(n));
  };
  var decimalFloat = bind7(decimal2)(function(n) {
    return option(new Left(n))(fractFloat(n));
  });
  var zeroNumFloat = alt3(map22(Left.create)(alt3(hexadecimal2)(octal2)))(alt3(decimalFloat)(alt3(fractFloat(0))(pure6(new Left(0)))));
  var natFloat = alt3(applySecond3($$char("0"))(zeroNumFloat))(decimalFloat);
  var naturalOrFloat2 = withErrorMessage(lexeme(natFloat))("number");
  var floating = bind7(decimal2)(fractExponent);
  var $$float = withErrorMessage(lexeme(floating))("float");
  var zeroNumber = withErrorMessage(applySecond3($$char("0"))(alt3(hexadecimal2)(alt3(octal2)(alt3(decimal2)(pure6(0))))))("");
  var nat = alt3(zeroNumber)(decimal2);
  var $$int = bind7(lexeme(sign1))(function(f) {
    return bind7(nat)(function(n) {
      return pure6(f(n));
    });
  });
  var integer = withErrorMessage(lexeme($$int))("integer");
  var natural = withErrorMessage(lexeme(nat))("natural");
  var comma = symbol(",");
  var commaSep2 = function(p) {
    return sepBy(p)(comma);
  };
  var commaSep1 = function(p) {
    return sepBy1(p)(comma);
  };
  var colon = symbol(":");
  var charNum = bind7(alt3(decimal2)(alt3(applySecond3($$char("o"))(number(8)(octDigit)))(applySecond3($$char("x"))(number(16)(hexDigit)))))(function(code) {
    var $120 = code > 1114111;
    if ($120) {
      return fail("invalid escape sequence");
    }
    ;
    var v1 = fromCharCode3(code);
    if (v1 instanceof Just) {
      return pure6(v1.value0);
    }
    ;
    if (v1 instanceof Nothing) {
      return fail("invalid character code (should not happen)");
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 498, column 10 - line 500, column 67): " + [v1.constructor.name]);
  });
  var charLetter = satisfy(function(c) {
    return c !== "'" && (c !== "\\" && c > "");
  });
  var charEsc = function() {
    var parseEsc = function(v1) {
      return voidLeft3($$char(v1.value0))(v1.value1);
    };
    return choice2(map10(parseEsc)(escMap));
  }();
  var charControl = bind7($$char("^"))(function() {
    return bind7(upper2)(function(code) {
      var v1 = fromCharCode3((toCharCode2(code) - toCharCode2("A") | 0) + 1 | 0);
      if (v1 instanceof Just) {
        return pure6(v1.value0);
      }
      ;
      if (v1 instanceof Nothing) {
        return fail("invalid character code (should not happen)");
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 488, column 5 - line 490, column 67): " + [v1.constructor.name]);
    });
  });
  var caseString = function(name2) {
    if (v.caseSensitive) {
      return voidLeft3(string(name2))(name2);
    }
    ;
    if (otherwise) {
      var msg = show5(name2);
      var caseChar = function(c) {
        var v1 = function(v2) {
          if (otherwise) {
            return $$char(c);
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 355, column 1 - line 355, column 80): " + [c.constructor.name]);
        };
        var $131 = isAlpha(codePointFromChar(c));
        if ($131) {
          var $132 = toChar(toLowerSimple2(singleton4(c)));
          if ($132 instanceof Just) {
            var $133 = toChar(toUpperSimple2(singleton4(c)));
            if ($133 instanceof Just) {
              return alt3($$char($132.value0))($$char($133.value0));
            }
            ;
            return v1(true);
          }
          ;
          return v1(true);
        }
        ;
        return v1(true);
      };
      var walk = function(name$prime) {
        var v1 = uncons2(name$prime);
        if (v1 instanceof Nothing) {
          return pure6(unit);
        }
        ;
        if (v1 instanceof Just) {
          return applySecond3(withErrorMessage(caseChar(v1.value0.head))(msg))(walk(v1.value0.tail));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 757, column 22 - line 759, column 72): " + [v1.constructor.name]);
      };
      return voidLeft3(walk(name2))(name2);
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 751, column 3 - line 751, column 50): " + [name2.constructor.name]);
  };
  var reserved2 = function(name2) {
    var go = applySecond3(caseString(name2))(withErrorMessage(notFollowedBy(v.identLetter))("end of " + name2));
    return lexeme($$try(go));
  };
  var brackets = function(p) {
    return between(symbol("["))(symbol("]"))(p);
  };
  var braces = function(p) {
    return between(symbol("{"))(symbol("}"))(p);
  };
  var ascii3codes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"];
  var ascii3 = ["\0", "", "", "", "", "", "", "\x07", "", "", "", "", "", "", "", "", "", "", "\x1B", "\x7F"];
  var ascii2codes = ["BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP"];
  var ascii2 = ["\b", "	", "\n", "\v", "\f", "\r", "", "", "", "", "", "", "", " "];
  var asciiMap = zip(append3(ascii3codes)(ascii2codes))(append3(ascii3)(ascii2));
  var charAscii = function() {
    var parseAscii = function(v1) {
      return $$try(voidLeft3(string(v1.value0))(v1.value1));
    };
    return choice2(map10(parseAscii)(asciiMap));
  }();
  var escapeCode = alt3(charEsc)(alt3(charNum)(alt3(charAscii)(withErrorMessage(charControl)("escape code"))));
  var charEscape = applySecond3($$char("\\"))(escapeCode);
  var characterChar = alt3(charLetter)(withErrorMessage(charEscape)("literal character"));
  var charLiteral = function() {
    var go = between($$char("'"))(withErrorMessage($$char("'"))("end of character"))(characterChar);
    return withErrorMessage(lexeme(go))("character");
  }();
  var stringEscape = bind7($$char("\\"))(function() {
    return alt3(voidLeft3(escapeGap)(Nothing.value))(alt3(voidLeft3(escapeEmpty)(Nothing.value))(map22(Just.create)(escapeCode)));
  });
  var stringChar = alt3(map22(Just.create)(stringLetter))(withErrorMessage(stringEscape)("string character"));
  var stringLiteral2 = function() {
    var folder = function(v1) {
      return function(v2) {
        if (v1 instanceof Nothing) {
          return v2;
        }
        ;
        if (v1 instanceof Just) {
          return new Cons(v1.value0, v2);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 455, column 5 - line 455, column 51): " + [v1.constructor.name, v2.constructor.name]);
      };
    };
    var go = bind7(between($$char('"'))(withErrorMessage($$char('"'))("end of string"))(many1(stringChar)))(function(maybeChars) {
      return pure6(fromCharArray(toUnfoldable3(foldr12(folder)(Nil.value)(maybeChars))));
    });
    return lexeme(withErrorMessage(go)("literal string"));
  }();
  var angles = function(p) {
    return between(symbol("<"))(symbol(">"))(p);
  };
  return {
    identifier: identifier2,
    reserved: reserved2,
    operator,
    reservedOp: reservedOp2,
    charLiteral,
    stringLiteral: stringLiteral2,
    natural,
    integer,
    "float": $$float,
    naturalOrFloat: naturalOrFloat2,
    decimal: decimal2,
    hexadecimal: hexadecimal2,
    octal: octal2,
    symbol,
    lexeme,
    whiteSpace: whiteSpace$prime(v),
    parens: parens2,
    braces,
    angles,
    brackets,
    semi,
    comma,
    colon,
    dot,
    semiSep: semiSep2,
    semiSep1,
    commaSep: commaSep2,
    commaSep1
  };
};

// output/Parsing.Language/index.js
var alt4 = /* @__PURE__ */ alt(altParserT);
var emptyDef = /* @__PURE__ */ function() {
  var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
  return {
    commentStart: "",
    commentEnd: "",
    commentLine: "",
    nestedComments: true,
    identStart: alt4(letter)($$char("_")),
    identLetter: alt4(alphaNum)(oneOf2(["_", "'"])),
    opStart: op$prime,
    opLetter: op$prime,
    reservedOpNames: [],
    reservedNames: [],
    caseSensitive: true
  };
}();

// output/TokenParser/index.js
var choice3 = /* @__PURE__ */ choice(foldableArray);
var voidLeft4 = /* @__PURE__ */ voidLeft(functorParserT);
var tokenParser = /* @__PURE__ */ makeTokenParser(/* @__PURE__ */ function() {
  var v = unGenLanguageDef(emptyDef);
  return {
    commentStart: "{-",
    commentEnd: "-}",
    commentLine: "--",
    nestedComments: true,
    identStart: v.identStart,
    identLetter: v.identLetter,
    opStart: v.opStart,
    opLetter: v.opLetter,
    reservedNames: ["dancer", "camera", "plane", "osc", "range", "clear", "ambient", "directional", "hemisphere", "point", "rectarea", "spot"],
    reservedOpNames: [";", "=", "*", "+", "-", "/"],
    caseSensitive: v.caseSensitive
  };
}());
var whiteSpace = /* @__PURE__ */ function() {
  return tokenParser.whiteSpace;
}();
var stringLiteral = /* @__PURE__ */ function() {
  return tokenParser.stringLiteral;
}();
var semiSep = /* @__PURE__ */ function() {
  return tokenParser.semiSep;
}();
var reservedOp = /* @__PURE__ */ function() {
  return tokenParser.reservedOp;
}();
var reserved = /* @__PURE__ */ function() {
  return tokenParser.reserved;
}();
var parens = /* @__PURE__ */ function() {
  return tokenParser.parens;
}();
var naturalOrFloat = /* @__PURE__ */ function() {
  return tokenParser.naturalOrFloat;
}();
var identifier = /* @__PURE__ */ function() {
  return tokenParser.identifier;
}();
var commaSep = /* @__PURE__ */ function() {
  return tokenParser.commaSep;
}();
var $$boolean = /* @__PURE__ */ choice3([/* @__PURE__ */ voidLeft4(/* @__PURE__ */ reserved("true"))(true), /* @__PURE__ */ voidLeft4(/* @__PURE__ */ reserved("false"))(false)]);

// output/AST/index.js
var $runtime_lazy8 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var bind8 = /* @__PURE__ */ bind(bindParserT);
var discard5 = /* @__PURE__ */ discard(discardUnit)(bindParserT);
var map11 = /* @__PURE__ */ map(functorParserT);
var choice4 = /* @__PURE__ */ choice(foldableArray);
var voidLeft5 = /* @__PURE__ */ voidLeft(functorParserT);
var voidRight2 = /* @__PURE__ */ voidRight(functorParserT);
var pure7 = /* @__PURE__ */ pure(applicativeParserT);
var alt5 = /* @__PURE__ */ alt(altParserT);
var foldl4 = /* @__PURE__ */ foldl(foldableList);
var Element = /* @__PURE__ */ function() {
  function Element2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Element2.create = function(value0) {
    return function(value1) {
      return new Element2(value0, value1);
    };
  };
  return Element2;
}();
var Camera = /* @__PURE__ */ function() {
  function Camera2(value0) {
    this.value0 = value0;
  }
  ;
  Camera2.create = function(value0) {
    return new Camera2(value0);
  };
  return Camera2;
}();
var Osc2 = /* @__PURE__ */ function() {
  function Osc3(value0) {
    this.value0 = value0;
  }
  ;
  Osc3.create = function(value0) {
    return new Osc3(value0);
  };
  return Osc3;
}();
var Range = /* @__PURE__ */ function() {
  function Range2(value0) {
    this.value0 = value0;
  }
  ;
  Range2.create = function(value0) {
    return new Range2(value0);
  };
  return Range2;
}();
var Clear = /* @__PURE__ */ function() {
  function Clear2(value0) {
    this.value0 = value0;
  }
  ;
  Clear2.create = function(value0) {
    return new Clear2(value0);
  };
  return Clear2;
}();
var LiteralNumber = /* @__PURE__ */ function() {
  function LiteralNumber2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  LiteralNumber2.create = function(value0) {
    return function(value1) {
      return new LiteralNumber2(value0, value1);
    };
  };
  return LiteralNumber2;
}();
var LiteralString = /* @__PURE__ */ function() {
  function LiteralString2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  LiteralString2.create = function(value0) {
    return function(value1) {
      return new LiteralString2(value0, value1);
    };
  };
  return LiteralString2;
}();
var LiteralInt = /* @__PURE__ */ function() {
  function LiteralInt2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  LiteralInt2.create = function(value0) {
    return function(value1) {
      return new LiteralInt2(value0, value1);
    };
  };
  return LiteralInt2;
}();
var LiteralBoolean = /* @__PURE__ */ function() {
  function LiteralBoolean2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  LiteralBoolean2.create = function(value0) {
    return function(value1) {
      return new LiteralBoolean2(value0, value1);
    };
  };
  return LiteralBoolean2;
}();
var This = /* @__PURE__ */ function() {
  function This2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  This2.create = function(value0) {
    return function(value1) {
      return new This2(value0, value1);
    };
  };
  return This2;
}();
var SemiGlobal = /* @__PURE__ */ function() {
  function SemiGlobal2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  SemiGlobal2.create = function(value0) {
    return function(value1) {
      return new SemiGlobal2(value0, value1);
    };
  };
  return SemiGlobal2;
}();
var Transformer = /* @__PURE__ */ function() {
  function Transformer2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Transformer2.create = function(value0) {
    return function(value1) {
      return new Transformer2(value0, value1);
    };
  };
  return Transformer2;
}();
var Application = /* @__PURE__ */ function() {
  function Application2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Application2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Application2(value0, value1, value2);
      };
    };
  };
  return Application2;
}();
var Sum2 = /* @__PURE__ */ function() {
  function Sum3(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Sum3.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Sum3(value0, value1, value2);
      };
    };
  };
  return Sum3;
}();
var Difference = /* @__PURE__ */ function() {
  function Difference2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Difference2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Difference2(value0, value1, value2);
      };
    };
  };
  return Difference2;
}();
var Product3 = /* @__PURE__ */ function() {
  function Product4(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Product4.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Product4(value0, value1, value2);
      };
    };
  };
  return Product4;
}();
var Divide2 = /* @__PURE__ */ function() {
  function Divide3(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Divide3.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Divide3(value0, value1, value2);
      };
    };
  };
  return Divide3;
}();
var Assignment = /* @__PURE__ */ function() {
  function Assignment2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Assignment2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Assignment2(value0, value1, value2);
      };
    };
  };
  return Assignment2;
}();
var Action = /* @__PURE__ */ function() {
  function Action2(value0) {
    this.value0 = value0;
  }
  ;
  Action2.create = function(value0) {
    return new Action2(value0);
  };
  return Action2;
}();
var EmptyStatement = /* @__PURE__ */ function() {
  function EmptyStatement2(value0) {
    this.value0 = value0;
  }
  ;
  EmptyStatement2.create = function(value0) {
    return new EmptyStatement2(value0);
  };
  return EmptyStatement2;
}();
var translateModifierIdentifiers = function(v) {
  if (v === "color") {
    return "colour";
  }
  ;
  return v;
};
var thisRef = /* @__PURE__ */ bind8(position)(function(p) {
  return discard5(reserved("this"))(function() {
    return discard5(reservedOp("."))(function() {
      return map11(This.create(p))(identifier);
    });
  });
});
var semiGlobalRef = /* @__PURE__ */ function() {
  return apply(applyParserT)(map11(SemiGlobal.create)(position))(identifier);
}();
var multiplicationDivision = /* @__PURE__ */ bind8(position)(function(p) {
  return choice4([voidLeft5(reservedOp("*"))(Product3.create(p)), voidLeft5(reservedOp("/"))(Divide2.create(p))]);
});
var intOrNumber = /* @__PURE__ */ bind8(position)(function(p) {
  return bind8(option(true)(voidRight2(false)(reservedOp("-"))))(function(isPositive) {
    return bind8(naturalOrFloat)(function(x) {
      if (x instanceof Left) {
        if (isPositive) {
          return pure7(new LiteralInt(p, x.value0));
        }
        ;
        return pure7(new LiteralInt(p, x.value0 * (-1 | 0) | 0));
      }
      ;
      if (x instanceof Right) {
        if (isPositive) {
          return pure7(new LiteralNumber(p, x.value0));
        }
        ;
        return pure7(new LiteralNumber(p, x.value0 * -1));
      }
      ;
      throw new Error("Failed pattern match at AST (line 226, column 3 - line 228, column 102): " + [x.constructor.name]);
    });
  });
});
var expressionPosition = function(v) {
  if (v instanceof LiteralNumber) {
    return v.value0;
  }
  ;
  if (v instanceof LiteralString) {
    return v.value0;
  }
  ;
  if (v instanceof LiteralInt) {
    return v.value0;
  }
  ;
  if (v instanceof LiteralBoolean) {
    return v.value0;
  }
  ;
  if (v instanceof This) {
    return v.value0;
  }
  ;
  if (v instanceof SemiGlobal) {
    return v.value0;
  }
  ;
  if (v instanceof Application) {
    return v.value0;
  }
  ;
  if (v instanceof Transformer) {
    return v.value0;
  }
  ;
  if (v instanceof Element) {
    return v.value0;
  }
  ;
  if (v instanceof Camera) {
    return v.value0;
  }
  ;
  if (v instanceof Osc2) {
    return v.value0;
  }
  ;
  if (v instanceof Range) {
    return v.value0;
  }
  ;
  if (v instanceof Clear) {
    return v.value0;
  }
  ;
  if (v instanceof Sum2) {
    return v.value0;
  }
  ;
  if (v instanceof Difference) {
    return v.value0;
  }
  ;
  if (v instanceof Product3) {
    return v.value0;
  }
  ;
  if (v instanceof Divide2) {
    return v.value0;
  }
  ;
  throw new Error("Failed pattern match at AST (line 102, column 1 - line 102, column 45): " + [v.constructor.name]);
};
var emptyStatement = /* @__PURE__ */ bind8(position)(function(p) {
  return discard5(lookAhead(whiteSpace))(function() {
    return discard5(alt5(lookAhead(eof))(lookAhead(reservedOp(";"))))(function() {
      return pure7(new EmptyStatement(p));
    });
  });
});
var additionSubtraction = /* @__PURE__ */ bind8(position)(function(p) {
  return choice4([voidLeft5(reservedOp("+"))(Sum2.create(p)), voidLeft5(reservedOp("-"))(Difference.create(p))]);
});
var $lazy_application = /* @__PURE__ */ $runtime_lazy8("application", "AST", function() {
  return bind8(pure7(unit))(function() {
    return bind8(position)(function(p) {
      return bind8($lazy_argument(190))(function(f) {
        return bind8($lazy_argument(191))(function(firstArg) {
          return bind8(many3($lazy_argument(192)))(function(otherArgs) {
            return pure7(foldl4(Application.create(p))(new Application(p, f, firstArg))(otherArgs));
          });
        });
      });
    });
  });
});
var $lazy_argument = /* @__PURE__ */ $runtime_lazy8("argument", "AST", function() {
  return bind8(pure7(unit))(function() {
    return bind8(position)(function(p) {
      return choice4([parens($lazy_expression(200)), $$try($lazy_transformer(201)), $$try(intOrNumber), $$try(map11(LiteralString.create(p))(stringLiteral)), $$try(map11(LiteralBoolean.create(p))($$boolean)), $$try(voidRight2(new Element(p, Dancer.value))(reserved("dancer"))), $$try(voidRight2(new Element(p, Plane.value))(reserved("plane"))), $$try(voidRight2(new Element(p, Ambient.value))(reserved("ambient"))), $$try(voidRight2(new Element(p, Directional.value))(reserved("directional"))), $$try(voidRight2(new Element(p, Hemisphere.value))(reserved("hemisphere"))), $$try(voidRight2(new Element(p, Point.value))(reserved("point"))), $$try(voidRight2(new Element(p, RectArea.value))(reserved("rectarea"))), $$try(voidRight2(new Element(p, Spot.value))(reserved("spot"))), $$try(voidRight2(new Camera(p))(reserved("camera"))), $$try(voidRight2(new Osc2(p))(reserved("osc"))), $$try(voidRight2(new Range(p))(reserved("range"))), $$try(voidRight2(new Clear(p))(reserved("clear"))), $$try(thisRef), semiGlobalRef]);
    });
  });
});
var $lazy_expression = /* @__PURE__ */ $runtime_lazy8("expression", "AST", function() {
  return bind8(pure7(unit))(function() {
    return chainl1($lazy_expression$prime(155))(additionSubtraction);
  });
});
var $lazy_expression$prime = /* @__PURE__ */ $runtime_lazy8("expression'", "AST", function() {
  return bind8(pure7(unit))(function() {
    return chainl1($lazy_expression$prime$prime(168))(multiplicationDivision);
  });
});
var $lazy_expression$prime$prime = /* @__PURE__ */ $runtime_lazy8("expression''", "AST", function() {
  return bind8(pure7(unit))(function() {
    return choice4([$$try($lazy_application(182)), $lazy_argument(183)]);
  });
});
var $lazy_modifier = /* @__PURE__ */ $runtime_lazy8("modifier", "AST", function() {
  return bind8(identifier)(function(k) {
    return discard5(alt5(reservedOp("="))(reservedOp(":")))(function() {
      return bind8($lazy_expression(243))(function(e) {
        return pure7(new Tuple(translateModifierIdentifiers(k), e));
      });
    });
  });
});
var $lazy_transformer = /* @__PURE__ */ $runtime_lazy8("transformer", "AST", function() {
  return bind8(pure7(unit))(function() {
    return bind8(position)(function(p) {
      return discard5(reservedOp("{"))(function() {
        return bind8(commaSep($lazy_modifier(235)))(function(xs) {
          return discard5(reservedOp("}"))(function() {
            return pure7(new Transformer(p, xs));
          });
        });
      });
    });
  });
});
var expression = /* @__PURE__ */ $lazy_expression(152);
var assignment = /* @__PURE__ */ bind8(position)(function(p) {
  return bind8(identifier)(function(k) {
    return discard5(reservedOp("="))(function() {
      return bind8(expression)(function(v) {
        return pure7(new Assignment(p, k, v));
      });
    });
  });
});
var action = /* @__PURE__ */ function() {
  return map11(Action.create)(expression);
}();
var statement = /* @__PURE__ */ alt5(/* @__PURE__ */ $$try(assignment))(/* @__PURE__ */ alt5(/* @__PURE__ */ $$try(action))(emptyStatement));
var ast = /* @__PURE__ */ discard5(whiteSpace)(function() {
  return bind8(semiSep(statement))(function(xs) {
    return discard5(eof)(function() {
      return pure7(xs);
    });
  });
});
var parseAST = function(x) {
  return runParser(x)(ast);
};

// output/Effect.Unsafe/foreign.js
var unsafePerformEffect = function(f) {
  return f();
};

// output/Program/index.js
var fromFoldable4 = /* @__PURE__ */ fromFoldable(ordString)(foldableArray);
var defaultPlane = /* @__PURE__ */ function() {
  return fromFoldable4([new Tuple("colour", new ValueInt(8947848)), new Tuple("shadows", new ValueBoolean(true))]);
}();
var defaultDancer = /* @__PURE__ */ function() {
  return fromFoldable4([new Tuple("x", new ValueNumber(0)), new Tuple("y", new ValueNumber(0)), new Tuple("z", new ValueNumber(0)), new Tuple("rx", new ValueNumber(0)), new Tuple("ry", new ValueNumber(0)), new Tuple("rz", new ValueNumber(0)), new Tuple("sx", new ValueNumber(1)), new Tuple("sy", new ValueNumber(1)), new Tuple("sz", new ValueNumber(1)), new Tuple("size", new ValueNumber(1))]);
}();
var defaultClear = /* @__PURE__ */ function() {
  return fromFoldable4([new Tuple("colour", new ValueInt(0)), new Tuple("alpha", new ValueNumber(1))]);
}();
var defaultCamera = /* @__PURE__ */ function() {
  return fromFoldable4([new Tuple("x", new ValueNumber(0)), new Tuple("y", new ValueNumber(1)), new Tuple("z", new ValueNumber(10)), new Tuple("rx", new ValueNumber(0)), new Tuple("ry", new ValueNumber(0)), new Tuple("rz", new ValueNumber(0))]);
}();
var defaultProgram = /* @__PURE__ */ function() {
  return {
    elements: [],
    cameraMap: defaultCamera,
    clearMap: Nothing.value,
    hasCustomLights: false
  };
}();

// output/P/index.js
var bindStateT5 = /* @__PURE__ */ bindStateT(monadEither);
var bind9 = /* @__PURE__ */ bind(bindStateT5);
var monadStateStateT2 = /* @__PURE__ */ monadStateStateT(monadEither);
var get2 = /* @__PURE__ */ get(monadStateStateT2);
var pure8 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadEither));
var modify5 = /* @__PURE__ */ modify(monadStateStateT2);
var throwError3 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowStateT(monadThrowEither));
var discard6 = /* @__PURE__ */ discard(discardUnit)(bindStateT5);
var modify_2 = /* @__PURE__ */ modify_(monadStateStateT2);
var put2 = /* @__PURE__ */ put(monadStateStateT2);
var evalStateT2 = /* @__PURE__ */ evalStateT(functorEither);
var readClear = /* @__PURE__ */ bind9(get2)(function(s) {
  return pure8(function() {
    if (s.program.clearMap instanceof Just) {
      return s.program.clearMap.value0;
    }
    ;
    if (s.program.clearMap instanceof Nothing) {
      return defaultClear;
    }
    ;
    throw new Error("Failed pattern match at P (line 86, column 10 - line 88, column 28): " + [s.program.clearMap.constructor.name]);
  }());
});
var readCamera = /* @__PURE__ */ bind9(get2)(function(s) {
  return pure8(s.program.cameraMap);
});
var newElement = function(t) {
  return function(vm) {
    var e = new Tuple(t, vm);
    return bind9(modify5(function(s) {
      var $32 = {};
      for (var $33 in s) {
        if ({}.hasOwnProperty.call(s, $33)) {
          $32[$33] = s[$33];
        }
        ;
      }
      ;
      $32.program = function() {
        var $29 = {};
        for (var $30 in s.program) {
          if ({}.hasOwnProperty.call(s.program, $30)) {
            $29[$30] = s["program"][$30];
          }
          ;
        }
        ;
        $29.elements = snoc(s.program.elements)(e);
        return $29;
      }();
      return $32;
    }))(function(s) {
      var n = length(s.program.elements) - 1 | 0;
      return pure8(new ValueElement(t, n, vm));
    });
  };
};
var liftEitherParseError = function(v) {
  if (v instanceof Left) {
    return throwError3(v.value0);
  }
  ;
  if (v instanceof Right) {
    return pure8(v.value0);
  }
  ;
  throw new Error("Failed pattern match at P (line 36, column 1 - line 36, column 61): " + [v.constructor.name]);
};
var modifyCamera = function(t) {
  return bind9(readCamera)(function(cm) {
    return bind9(liftEitherParseError(t(cm)))(function(cm$prime) {
      return discard6(modify_2(function(s) {
        var $41 = {};
        for (var $42 in s) {
          if ({}.hasOwnProperty.call(s, $42)) {
            $41[$42] = s[$42];
          }
          ;
        }
        ;
        $41.program = function() {
          var $38 = {};
          for (var $39 in s.program) {
            if ({}.hasOwnProperty.call(s.program, $39)) {
              $38[$39] = s["program"][$39];
            }
            ;
          }
          ;
          $38.cameraMap = cm$prime;
          return $38;
        }();
        return $41;
      }))(function() {
        return pure8(ValueCamera.value);
      });
    });
  });
};
var modifyClear = function(t) {
  return bind9(readClear)(function(cm) {
    return bind9(liftEitherParseError(t(cm)))(function(cm$prime) {
      return discard6(modify_2(function(s) {
        var $47 = {};
        for (var $48 in s) {
          if ({}.hasOwnProperty.call(s, $48)) {
            $47[$48] = s[$48];
          }
          ;
        }
        ;
        $47.program = function() {
          var $44 = {};
          for (var $45 in s.program) {
            if ({}.hasOwnProperty.call(s.program, $45)) {
              $44[$45] = s["program"][$45];
            }
            ;
          }
          ;
          $44.clearMap = new Just(cm$prime);
          return $44;
        }();
        return $47;
      }))(function() {
        return pure8(ValueClear.value);
      });
    });
  });
};
var modifyElement = function(n) {
  return function(t) {
    return bind9(get2)(function(s) {
      return bind9(function() {
        var v = index(s.program.elements)(n);
        if (v instanceof Nothing) {
          return discard6(pure8(unsafePerformEffect(log2("modifyElement: this should not ever happen 1"))))(function() {
            return pure8(new Tuple(Dancer.value, empty2));
          });
        }
        ;
        if (v instanceof Just) {
          return bind9(liftEitherParseError(t(v.value0.value1)))(function(newVm) {
            return pure8(new Tuple(v.value0.value0, newVm));
          });
        }
        ;
        throw new Error("Failed pattern match at P (line 50, column 24 - line 56, column 31): " + [v.constructor.name]);
      }())(function(v) {
        return discard6(function() {
          var v1 = updateAt(n)(new Tuple(v.value0, v.value1))(s.program.elements);
          if (v1 instanceof Nothing) {
            return discard6(pure8(unsafePerformEffect(log2("modifyElement: this should not ever happen 2"))))(function() {
              return pure8(unit);
            });
          }
          ;
          if (v1 instanceof Just) {
            return put2(function() {
              var $59 = {};
              for (var $60 in s) {
                if ({}.hasOwnProperty.call(s, $60)) {
                  $59[$60] = s[$60];
                }
                ;
              }
              ;
              $59.program = function() {
                var $56 = {};
                for (var $57 in s.program) {
                  if ({}.hasOwnProperty.call(s.program, $57)) {
                    $56[$57] = s["program"][$57];
                  }
                  ;
                }
                ;
                $56.elements = v1.value0;
                return $56;
              }();
              return $59;
            }());
          }
          ;
          throw new Error("Failed pattern match at P (line 57, column 3 - line 61, column 63): " + [v1.constructor.name]);
        }())(function() {
          return pure8(new ValueElement(v.value0, n, v.value1));
        });
      });
    });
  };
};
var evalP = function(sm) {
  return function(tm) {
    return function(prog) {
      return function(p) {
        return evalStateT2(p)({
          semiMap: sm,
          thisMap: tm,
          program: prog
        });
      };
    };
  };
};
var runP = /* @__PURE__ */ evalP(empty2)(empty2)(defaultProgram);

// output/Parser/index.js
var show6 = /* @__PURE__ */ show(showInt);
var elem3 = /* @__PURE__ */ elem(foldableArray)(eqBoolean);
var map12 = /* @__PURE__ */ map(functorArray);
var add3 = /* @__PURE__ */ add(semiringVariable);
var mul3 = /* @__PURE__ */ mul(semiringVariable);
var sub3 = /* @__PURE__ */ sub(ringVariable);
var pure9 = /* @__PURE__ */ pure(applicativeEither);
var throwError4 = /* @__PURE__ */ throwError(monadThrowEither);
var throwError1 = /* @__PURE__ */ throwError(/* @__PURE__ */ monadThrowStateT(monadThrowEither));
var applicativeStateT3 = /* @__PURE__ */ applicativeStateT(monadEither);
var pure13 = /* @__PURE__ */ pure(applicativeStateT3);
var bindStateT6 = /* @__PURE__ */ bindStateT(monadEither);
var bind10 = /* @__PURE__ */ bind(bindStateT6);
var traverse3 = /* @__PURE__ */ traverse(traversableList)(applicativeStateT3);
var foldl5 = /* @__PURE__ */ foldl(foldableList);
var monadStateStateT3 = /* @__PURE__ */ monadStateStateT(monadEither);
var get3 = /* @__PURE__ */ get(monadStateStateT3);
var insert3 = /* @__PURE__ */ insert(ordString);
var lookup5 = /* @__PURE__ */ lookup(ordString);
var add1 = /* @__PURE__ */ add(semiringValue);
var sub1 = /* @__PURE__ */ sub(ringValue);
var mul1 = /* @__PURE__ */ mul(semiringValue);
var modify_3 = /* @__PURE__ */ modify_(monadStateStateT3);
var discard7 = /* @__PURE__ */ discard(discardUnit);
var discard12 = /* @__PURE__ */ discard7(bindStateT6);
var traverse_2 = /* @__PURE__ */ traverse_(applicativeStateT3)(foldableList);
var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
var bind12 = /* @__PURE__ */ bind(bindEither);
var showParseError2 = function(v) {
  return show6(v.value1.line) + (":" + (show6(v.value1.column) + (" " + v.value0)));
};
var setCustomLightsFlag = function(p) {
  return {
    elements: p.elements,
    cameraMap: p.cameraMap,
    clearMap: p.clearMap,
    hasCustomLights: elem3(true)(map12(function($196) {
      return isLight(fst($196));
    })(p.elements))
  };
};
var rangeFunction = function(v) {
  return function(r1) {
    var f = function(r1$prime) {
      return function(r2$prime) {
        return function(x$prime) {
          return add3(mul3(add3(mul3(x$prime)(new ConstantVariable(0.5)))(new ConstantVariable(0.5)))(sub3(r2$prime)(r1$prime)))(r1$prime);
        };
      };
    };
    return pure9(new ValueFunction(function(v1) {
      return function(r2) {
        return pure9(new ValueFunction(function(v2) {
          return function(x) {
            return pure9(new ValueVariable(f(valueToVariable(r1))(valueToVariable(r2))(valueToVariable(x))));
          };
        }));
      };
    }));
  };
};
var oscFunction = function(v) {
  return function(v1) {
    if (v1 instanceof ValueVariable) {
      return pure9(new ValueVariable(new Osc(v1.value0)));
    }
    ;
    if (v1 instanceof ValueNumber) {
      return pure9(new ValueVariable(new Osc(new ConstantVariable(v1.value0))));
    }
    ;
    if (v1 instanceof ValueInt) {
      return pure9(new ValueVariable(new Osc(new ConstantVariable(toNumber(v1.value0)))));
    }
    ;
    return throwError4(new ParseError("argument to osc must be Variable/Number/Int", v));
  };
};
var applyTransformer = function(tF) {
  return function(x) {
    var v = tF(x);
    if (v instanceof Left) {
      return throwError1(v.value0);
    }
    ;
    if (v instanceof Right) {
      return pure13(new ValueTransformer(valueMapToTransformer(v.value0)));
    }
    ;
    throw new Error("Failed pattern match at Parser (line 202, column 3 - line 204, column 67): " + [v.constructor.name]);
  };
};
var transformerToValue = function(xs) {
  return bind10(traverse3(parseModifier)(xs))(function(ts) {
    return pure13(new ValueTransformer(foldl5(appendTransformers)(pure9)(ts)));
  });
};
var parseModifier = function(v) {
  return bind10(get3)(function(s) {
    return pure13(function(tm) {
      return evalP(s.semiMap)(tm)(s.program)(bind10(expressionToValue(v.value1))(function(v1) {
        return pure13(insert3(v.value0)(v1)(tm));
      }));
    });
  });
};
var expressionToValue = function(v) {
  if (v instanceof LiteralNumber) {
    return pure13(new ValueNumber(v.value1));
  }
  ;
  if (v instanceof LiteralString) {
    return pure13(new ValueString(v.value1));
  }
  ;
  if (v instanceof LiteralInt) {
    return pure13(new ValueInt(v.value1));
  }
  ;
  if (v instanceof LiteralBoolean) {
    return pure13(new ValueBoolean(v.value1));
  }
  ;
  if (v instanceof This) {
    return bind10(get3)(function(s) {
      var v1 = lookup5(v.value1)(s.thisMap);
      if (v1 instanceof Nothing) {
        return throwError1(new ParseError("unknown this reference " + v.value1, v.value0));
      }
      ;
      if (v1 instanceof Just) {
        return pure13(v1.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser (line 77, column 3 - line 79, column 21): " + [v1.constructor.name]);
    });
  }
  ;
  if (v instanceof SemiGlobal) {
    return bind10(get3)(function(s) {
      var v1 = lookup5(v.value1)(s.semiMap);
      if (v1 instanceof Nothing) {
        return throwError1(new ParseError("unknown semiglobal reference " + v.value1, v.value0));
      }
      ;
      if (v1 instanceof Just) {
        return pure13(v1.value0);
      }
      ;
      throw new Error("Failed pattern match at Parser (line 83, column 3 - line 85, column 21): " + [v1.constructor.name]);
    });
  }
  ;
  if (v instanceof Application) {
    return applicationToValue(v.value0)(v.value1)(v.value2);
  }
  ;
  if (v instanceof Transformer) {
    return transformerToValue(v.value1);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Dancer) {
    return newElement(Dancer.value)(defaultDancer);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Plane) {
    return newElement(Plane.value)(defaultPlane);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Ambient) {
    return newElement(Ambient.value)(empty2);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Directional) {
    return newElement(Directional.value)(empty2);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Hemisphere) {
    return newElement(Hemisphere.value)(empty2);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Point) {
    return newElement(Point.value)(empty2);
  }
  ;
  if (v instanceof Element && v.value1 instanceof RectArea) {
    return newElement(RectArea.value)(empty2);
  }
  ;
  if (v instanceof Element && v.value1 instanceof Spot) {
    return newElement(Spot.value)(empty2);
  }
  ;
  if (v instanceof Camera) {
    return pure13(ValueCamera.value);
  }
  ;
  if (v instanceof Clear) {
    return pure13(ValueClear.value);
  }
  ;
  if (v instanceof Osc2) {
    return pure13(new ValueFunction(oscFunction));
  }
  ;
  if (v instanceof Range) {
    return pure13(new ValueFunction(rangeFunction));
  }
  ;
  if (v instanceof Sum2) {
    return bind10(expressionToValue(v.value1))(function(v1) {
      return bind10(expressionToValue(v.value2))(function(v2) {
        return pure13(add1(v1)(v2));
      });
    });
  }
  ;
  if (v instanceof Difference) {
    return bind10(expressionToValue(v.value1))(function(v1) {
      return bind10(expressionToValue(v.value2))(function(v2) {
        return pure13(sub1(v1)(v2));
      });
    });
  }
  ;
  if (v instanceof Product3) {
    return bind10(expressionToValue(v.value1))(function(v1) {
      return bind10(expressionToValue(v.value2))(function(v2) {
        return pure13(mul1(v1)(v2));
      });
    });
  }
  ;
  if (v instanceof Divide2) {
    return bind10(expressionToValue(v.value1))(function(v1) {
      return bind10(expressionToValue(v.value2))(function(v2) {
        return pure13(divideValues(v1)(v2));
      });
    });
  }
  ;
  throw new Error("Failed pattern match at Parser (line 69, column 1 - line 69, column 43): " + [v.constructor.name]);
};
var applicationToValue = function(p) {
  return function(eF) {
    return function(eX) {
      return bind10(expressionToValue(eF))(function(f) {
        return bind10(expressionToValue(eX))(function(x) {
          if (f instanceof ValueNumber) {
            return throwError1(new ParseError("A Number can't be the left side of a function application", p));
          }
          ;
          if (f instanceof ValueString) {
            return throwError1(new ParseError("A String can't be the left side of a function application", p));
          }
          ;
          if (f instanceof ValueInt) {
            return throwError1(new ParseError("An Int can't be the left side of a function application", p));
          }
          ;
          if (f instanceof ValueBoolean) {
            return throwError1(new ParseError("A Boolean can't be the left side of a function application", p));
          }
          ;
          if (f instanceof ValueVariable) {
            return throwError1(new ParseError("A Variable can't be the left side of a function application", p));
          }
          ;
          if (f instanceof ValueTransformer) {
            if (x instanceof ValueTransformer) {
              return pure13(new ValueTransformer(appendTransformers(f.value0)(x.value0)));
            }
            ;
            if (x instanceof ValueElement) {
              return applyTransformer(f.value0)(x.value2);
            }
            ;
            if (x instanceof ValueCamera) {
              return bind10(readCamera)(function(vmX) {
                return applyTransformer(f.value0)(vmX);
              });
            }
            ;
            if (x instanceof ValueClear) {
              return bind10(readClear)(function(vmX) {
                return applyTransformer(f.value0)(vmX);
              });
            }
            ;
            return throwError1(new ParseError("invalid argument applied to Transformer", expressionPosition(eX)));
          }
          ;
          if (f instanceof ValueElement) {
            if (x instanceof ValueTransformer) {
              return modifyElement(f.value1)(x.value0);
            }
            ;
            if (x instanceof ValueElement) {
              return modifyElement(f.value1)(valueMapToTransformer(x.value2));
            }
            ;
            if (x instanceof ValueCamera) {
              return bind10(readCamera)(function(vmX) {
                return modifyElement(f.value1)(valueMapToTransformer(vmX));
              });
            }
            ;
            if (x instanceof ValueClear) {
              return bind10(readClear)(function(vmX) {
                return modifyElement(f.value1)(valueMapToTransformer(vmX));
              });
            }
            ;
            return throwError1(new ParseError("invalid argument applied to Dancer", expressionPosition(eX)));
          }
          ;
          if (f instanceof ValueCamera) {
            if (x instanceof ValueTransformer) {
              return modifyCamera(x.value0);
            }
            ;
            if (x instanceof ValueElement) {
              return modifyCamera(valueMapToTransformer(x.value2));
            }
            ;
            if (x instanceof ValueCamera) {
              return pure13(ValueCamera.value);
            }
            ;
            if (x instanceof ValueClear) {
              return bind10(readClear)(function(vmX) {
                return modifyCamera(valueMapToTransformer(vmX));
              });
            }
            ;
            return throwError1(new ParseError("invalid argument applied to Camera", expressionPosition(eX)));
          }
          ;
          if (f instanceof ValueClear) {
            if (x instanceof ValueTransformer) {
              return modifyClear(x.value0);
            }
            ;
            if (x instanceof ValueElement) {
              return modifyClear(valueMapToTransformer(x.value2));
            }
            ;
            if (x instanceof ValueCamera) {
              return bind10(readCamera)(function(vmX) {
                return modifyClear(valueMapToTransformer(vmX));
              });
            }
            ;
            if (x instanceof ValueClear) {
              return pure13(ValueClear.value);
            }
            ;
            return throwError1(new ParseError("invalid argument applied to Clear", expressionPosition(eX)));
          }
          ;
          if (f instanceof ValueFunction) {
            var v = f.value0(expressionPosition(eX))(x);
            if (v instanceof Left) {
              return throwError1(v.value0);
            }
            ;
            if (v instanceof Right) {
              return pure13(v.value0);
            }
            ;
            throw new Error("Failed pattern match at Parser (line 168, column 7 - line 170, column 26): " + [v.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Parser (line 121, column 3 - line 170, column 26): " + [f.constructor.name]);
        });
      });
    };
  };
};
var parseStatement = function(v) {
  if (v instanceof Assignment) {
    return bind10(expressionToValue(v.value2))(function(v1) {
      return modify_3(function(s) {
        var $185 = {};
        for (var $186 in s) {
          if ({}.hasOwnProperty.call(s, $186)) {
            $185[$186] = s[$186];
          }
          ;
        }
        ;
        $185.semiMap = insert3(v.value1)(v1)(s.semiMap);
        return $185;
      });
    });
  }
  ;
  if (v instanceof Action) {
    return bind10(expressionToValue(v.value0))(function() {
      return pure13(unit);
    });
  }
  ;
  if (v instanceof EmptyStatement) {
    return pure13(unit);
  }
  ;
  throw new Error("Failed pattern match at Parser (line 60, column 1 - line 60, column 38): " + [v.constructor.name]);
};
var astToProgram = function(ast2) {
  return discard12(traverse_2(parseStatement)(ast2))(function() {
    return bind10(get3)(function(s) {
      return pure13(setCustomLightsFlag(s.program));
    });
  });
};
var parseProgram = function(x) {
  return lmap2(showParseError2)(bind12(parseAST(x))(function($197) {
    return runP(astToProgram($197));
  }));
};

// output/Plane/index.js
var monadReaderT5 = /* @__PURE__ */ monadReaderT(monadEffect);
var bindStateT7 = /* @__PURE__ */ bindStateT(monadReaderT5);
var discard8 = /* @__PURE__ */ discard(discardUnit)(bindStateT7);
var liftEffect5 = /* @__PURE__ */ liftEffect(/* @__PURE__ */ monadEffectState(/* @__PURE__ */ monadEffectReader(monadEffectEffect)));
var updatePosition4 = /* @__PURE__ */ updatePosition();
var updateScale4 = /* @__PURE__ */ updateScale();
var updateRotation4 = /* @__PURE__ */ updateRotation();
var pure10 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeStateT(monadReaderT5));
var bind11 = /* @__PURE__ */ bind(bindStateT7);
var ask5 = /* @__PURE__ */ ask(/* @__PURE__ */ monadAskStateT(/* @__PURE__ */ monadAskReaderT(monadEffect)));
var updatePlane = function(vm) {
  return function(fs) {
    var colour = lookupInt(8947848)("colour")(vm);
    var shadows = lookupBoolean(true)("shadows")(vm);
    return discard8(liftEffect5(setColorInt(fs.material)(colour)))(function() {
      return discard8(liftEffect5(setReceiveShadow(fs.mesh)(shadows)))(function() {
        return discard8(updatePosition4(vm)(fs.mesh))(function() {
          return discard8(updateScale4(vm)(fs.mesh))(function() {
            return discard8(updateRotation4(vm)(fs.mesh))(function() {
              return pure10(fs);
            });
          });
        });
      });
    });
  };
};
var removePlane = function(fState) {
  return liftEffect5(removeFromParent(fState.mesh));
};
var newPlane2 = /* @__PURE__ */ bind11(/* @__PURE__ */ liftEffect5(/* @__PURE__ */ newPlaneGeometry(1)(1)(1)(1)))(function(geometry) {
  return bind11(liftEffect5(newMeshPhongMaterial({
    depthWrite: false
  })))(function(material) {
    return bind11(liftEffect5(newMesh(geometry)(material)))(function(mesh) {
      return bind11(ask5)(function(env) {
        return discard8(liftEffect5(addAnything(env.scene)(mesh)))(function() {
          return pure10({
            mesh,
            material
          });
        });
      });
    });
  });
});

// output/ZoneMap/index.js
var insert4 = /* @__PURE__ */ insert(ordInt);
var lookup6 = /* @__PURE__ */ lookup(ordInt);
var delete1 = /* @__PURE__ */ $$delete(ordInt);
var write3 = function(z) {
  return function(a) {
    return function(m) {
      return function __do2() {
        var m$prime = read(m)();
        return write(insert4(z)(a)(m$prime))(m)();
      };
    };
  };
};
var read3 = function(z) {
  return function(m) {
    return function __do2() {
      var m$prime = read(m)();
      return lookup6(z)(m$prime);
    };
  };
};
var $$new2 = /* @__PURE__ */ $$new(empty2);
var $$delete3 = function(z) {
  return function(m) {
    return function __do2() {
      var m$prime = read(m)();
      return write(delete1(z)(m$prime))(m)();
    };
  };
};
var count = function(m) {
  return function __do2() {
    var m$prime = read(m)();
    return size(m$prime);
  };
};

// output/RenderEngine/index.js
var monadReaderT6 = /* @__PURE__ */ monadReaderT(monadEffect);
var bindStateT8 = /* @__PURE__ */ bindStateT(monadReaderT6);
var bind13 = /* @__PURE__ */ bind(bindStateT8);
var applicativeStateT4 = /* @__PURE__ */ applicativeStateT(monadReaderT6);
var pure11 = /* @__PURE__ */ pure(applicativeStateT4);
var bind14 = /* @__PURE__ */ bind(bindEffect);
var catMaybes4 = /* @__PURE__ */ catMaybes(ordInt);
var map13 = /* @__PURE__ */ map(functorMap);
var unions2 = /* @__PURE__ */ unions(ordString)(foldableMap);
var lookup7 = /* @__PURE__ */ lookup(ordString);
var ask6 = /* @__PURE__ */ ask(/* @__PURE__ */ monadAskStateT(/* @__PURE__ */ monadAskReaderT(monadEffect)));
var discard9 = /* @__PURE__ */ discard(discardUnit);
var discard13 = /* @__PURE__ */ discard9(bindStateT8);
var updatePosition5 = /* @__PURE__ */ updatePosition();
var updateScale5 = /* @__PURE__ */ updateScale();
var updateRotation5 = /* @__PURE__ */ updateRotation();
var unwrap5 = /* @__PURE__ */ unwrap();
var diff3 = /* @__PURE__ */ diff(durationSeconds);
var setPosition5 = /* @__PURE__ */ setPosition2();
var toRational2 = /* @__PURE__ */ toRational(toRationalInt);
var pure14 = /* @__PURE__ */ pure(applicativeEffect);
var elem4 = /* @__PURE__ */ elem(foldableMap)(eqBoolean);
var setLightIntensity4 = /* @__PURE__ */ setLightIntensity2();
var when3 = /* @__PURE__ */ when(applicativeEffect);
var map14 = /* @__PURE__ */ map(/* @__PURE__ */ functorStateT(/* @__PURE__ */ functorReaderT(functorEffect)));
var monadStateStateT4 = /* @__PURE__ */ monadStateStateT(monadReaderT6);
var get4 = /* @__PURE__ */ get(monadStateStateT4);
var eq3 = /* @__PURE__ */ eq(eqElementType);
var modify_4 = /* @__PURE__ */ modify_(monadStateStateT4);
var traverseWithIndex2 = /* @__PURE__ */ traverseWithIndex(traversableWithIndexArray)(applicativeStateT4);
var traverse_3 = /* @__PURE__ */ traverse_(applicativeStateT4)(foldableArray);
var updateElement = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof ElementDancer) {
        return bind13(updateDancer(v)(v1)(v2.value0))(function($109) {
          return pure11(ElementDancer.create($109));
        });
      }
      ;
      if (v2 instanceof ElementPlane) {
        return bind13(updatePlane(v1)(v2.value0))(function($110) {
          return pure11(ElementPlane.create($110));
        });
      }
      ;
      if (v2 instanceof ElementAmbient) {
        return bind13(updateAmbient(v1)(v2.value0))(function($111) {
          return pure11(ElementAmbient.create($111));
        });
      }
      ;
      if (v2 instanceof ElementDirectional) {
        return bind13(updateDirectional(v1)(v2.value0))(function($112) {
          return pure11(ElementDirectional.create($112));
        });
      }
      ;
      if (v2 instanceof ElementHemisphere) {
        return bind13(updateHemisphere(v1)(v2.value0))(function($113) {
          return pure11(ElementHemisphere.create($113));
        });
      }
      ;
      if (v2 instanceof ElementPoint) {
        return bind13(updatePoint(v1)(v2.value0))(function($114) {
          return pure11(ElementPoint.create($114));
        });
      }
      ;
      if (v2 instanceof ElementRectArea) {
        return bind13(updateRectArea(v1)(v2.value0))(function($115) {
          return pure11(ElementRectArea.create($115));
        });
      }
      ;
      if (v2 instanceof ElementSpot) {
        return bind13(updateSpot(v1)(v2.value0))(function($116) {
          return pure11(ElementSpot.create($116));
        });
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 243, column 1 - line 243, column 57): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var setClearColor2 = function(re) {
  return function __do2() {
    var zs = read(re.programs)();
    var clearMaps = catMaybes4(map13(function(v) {
      return v.clearMap;
    })(zs));
    var cm = unions2(clearMaps);
    var c = function() {
      var v = lookup7("colour")(cm);
      if (v instanceof Just) {
        return valueToInt(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return 0;
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 177, column 11 - line 179, column 32): " + [v.constructor.name]);
    }();
    var a = function() {
      var v = lookup7("alpha")(cm);
      if (v instanceof Just) {
        return valueToNumber(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return 1;
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 180, column 11 - line 182, column 27): " + [v.constructor.name]);
    }();
    var rEnv = read(re.renderEnvironment)();
    return setClearColor(rEnv.renderer)(c)(a)();
  };
};
var runCamera = function(vm) {
  return bind13(ask6)(function(re) {
    return discard13(updatePosition5(vm)(re.camera))(function() {
      return discard13(updateScale5(vm)(re.camera))(function() {
        return updateRotation5(vm)(re.camera);
      });
    });
  });
};
var replaceAt = function(i) {
  return function(v) {
    return function(a) {
      if (i >= length(a)) {
        return snoc(a)(v);
      }
      ;
      if (otherwise) {
        return fromMaybe(a)(updateAt(i)(v)(a));
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 203, column 1 - line 203, column 54): " + [i.constructor.name, v.constructor.name, a.constructor.name]);
    };
  };
};
var removeElement = function(v) {
  if (v instanceof ElementDancer) {
    return removeDancer(v.value0);
  }
  ;
  if (v instanceof ElementPlane) {
    return removePlane(v.value0);
  }
  ;
  if (v instanceof ElementAmbient) {
    return removeAmbient(v.value0);
  }
  ;
  if (v instanceof ElementDirectional) {
    return removeDirectional(v.value0);
  }
  ;
  if (v instanceof ElementHemisphere) {
    return removeHemisphere(v.value0);
  }
  ;
  if (v instanceof ElementPoint) {
    return removePoint(v.value0);
  }
  ;
  if (v instanceof ElementRectArea) {
    return removeRectArea(v.value0);
  }
  ;
  if (v instanceof ElementSpot) {
    return removeSpot(v.value0);
  }
  ;
  throw new Error("Failed pattern match at RenderEngine (line 253, column 1 - line 253, column 35): " + [v.constructor.name]);
};
var preAnimate = function(re) {
  return function __do2() {
    var tNow = nowDateTime();
    var tPrev = read(re.prevTNow)();
    write(tNow)(re.prevTNow)();
    var envPrev = read(re.renderEnvironment)();
    var envNew = {
      delta: unwrap5(diff3(tNow)(tPrev)),
      nCycles: timeToCountNumber(envPrev.tempo)(tNow),
      cycleDur: 1 / toNumber3(envPrev.tempo.freq),
      camera: envPrev.camera,
      defaultLight: envPrev.defaultLight,
      renderer: envPrev.renderer,
      scene: envPrev.scene,
      tempo: envPrev.tempo
    };
    return write(envNew)(re.renderEnvironment)();
  };
};
var launch = function(cvs) {
  return function __do2() {
    log2("LocoMotion: launch...")();
    var scene = newScene();
    var defaultLight = newAmbientLight(16777215)(0)();
    addAnything(scene)(defaultLight)();
    var iWidth = windowInnerWidth();
    var iHeight = windowInnerHeight();
    var camera = newPerspectiveCamera(45)(iWidth / iHeight)(0.1)(100)();
    setPosition5(camera)(0)(1)(10)();
    var renderer = newWebGLRenderer({
      antialias: true,
      canvas: cvs,
      alpha: true
    })();
    setSize(renderer)(iWidth)(iHeight)(false)();
    setClearColor(renderer)(0)(1)();
    var tempo = newTempo(toRational2(1)(2))();
    var renderEnvironment = $$new({
      scene,
      camera,
      renderer,
      defaultLight,
      tempo,
      nCycles: 0,
      cycleDur: 2,
      delta: 0
    })();
    var programs = $$new2();
    var zoneStates = $$new2();
    var prevTNow = bind14(nowDateTime)($$new)();
    log2("LocoMotion: launch completed")();
    return {
      renderEnvironment,
      programs,
      zoneStates,
      prevTNow
    };
  };
};
var handleDefaultLighting = function(re) {
  return function __do2() {
    var env = read(re.renderEnvironment)();
    var allPrograms = read(re.programs)();
    var anyCustomLights = elem4(true)(map13(function(v) {
      return v.hasCustomLights;
    })(allPrograms));
    var intensity = function() {
      if (anyCustomLights) {
        return 0;
      }
      ;
      return 1;
    }();
    return setLightIntensity4(env.defaultLight)(intensity)();
  };
};
var postAnimate = function(re) {
  return function __do2() {
    var n = count(re.zoneStates)();
    return when3(n > 0)(function __do3() {
      handleDefaultLighting(re)();
      var iWidth = windowInnerWidth();
      var iHeight = windowInnerHeight();
      var rEnv = read(re.renderEnvironment)();
      setAspect(rEnv.camera)(iWidth / iHeight)();
      setSize(rEnv.renderer)(iWidth)(iHeight)(false)();
      setClearColor2(re)();
      return render(rEnv.renderer)(rEnv.scene)(rEnv.camera)();
    })();
  };
};
var evaluate = function(re) {
  return function(z) {
    return function(x) {
      var x$prime = parseProgram(x);
      if (x$prime instanceof Right) {
        return function __do2() {
          write3(z)(x$prime.value0)(re.programs)();
          return Nothing.value;
        };
      }
      ;
      if (x$prime instanceof Left) {
        return pure14(new Just(x$prime.value0));
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 94, column 3 - line 101, column 22): " + [x$prime.constructor.name]);
    };
  };
};
var createElement2 = function(v) {
  if (v instanceof Dancer) {
    return map14(ElementDancer.create)(newDancer);
  }
  ;
  if (v instanceof Plane) {
    return map14(ElementPlane.create)(newPlane2);
  }
  ;
  if (v instanceof Ambient) {
    return map14(ElementAmbient.create)(newAmbient);
  }
  ;
  if (v instanceof Directional) {
    return map14(ElementDirectional.create)(newDirectional);
  }
  ;
  if (v instanceof Hemisphere) {
    return map14(ElementHemisphere.create)(newHemisphere);
  }
  ;
  if (v instanceof Point) {
    return map14(ElementPoint.create)(newPoint);
  }
  ;
  if (v instanceof RectArea) {
    return map14(ElementRectArea.create)(newRectArea);
  }
  ;
  if (v instanceof Spot) {
    return map14(ElementSpot.create)(newSpot);
  }
  ;
  throw new Error("Failed pattern match at RenderEngine (line 233, column 1 - line 233, column 42): " + [v.constructor.name]);
};
var runElement = function(zone) {
  return function(i) {
    return function(v) {
      return bind13(get4)(function(s) {
        return bind13(function() {
          var v1 = index(s.elements)(i);
          if (v1 instanceof Nothing) {
            return bind13(createElement2(v.value0))(function(e) {
              return updateElement(zone)(v.value1)(e);
            });
          }
          ;
          if (v1 instanceof Just) {
            var v2 = eq3(v.value0)(elementType(v1.value0));
            if (v2) {
              return updateElement(zone)(v.value1)(v1.value0);
            }
            ;
            if (!v2) {
              return discard13(removeElement(v1.value0))(function() {
                return bind13(createElement2(v.value0))(function(e$prime) {
                  return updateElement(zone)(v.value1)(e$prime);
                });
              });
            }
            ;
            throw new Error("Failed pattern match at RenderEngine (line 225, column 7 - line 230, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at RenderEngine (line 220, column 11 - line 230, column 35): " + [v1.constructor.name]);
        }())(function(newE) {
          return modify_4(function(x) {
            var $97 = {};
            for (var $98 in x) {
              if ({}.hasOwnProperty.call(x, $98)) {
                $97[$98] = x[$98];
              }
              ;
            }
            ;
            $97.elements = replaceAt(i)(newE)(x.elements);
            return $97;
          });
        });
      });
    };
  };
};
var runElements = function(zone) {
  return function(xs) {
    return bind13(traverseWithIndex2(runElement(zone))(xs))(function() {
      var nElements = length(xs);
      return bind13(get4)(function(s) {
        return discard13(traverse_3(removeElement)(drop(nElements)(s.elements)))(function() {
          return modify_4(function(x) {
            var $102 = {};
            for (var $103 in x) {
              if ({}.hasOwnProperty.call(x, $103)) {
                $102[$103] = x[$103];
              }
              ;
            }
            ;
            $102.elements = take(nElements)(x.elements);
            return $102;
          });
        });
      });
    });
  };
};
var runProgram = function(zone) {
  return function(re) {
    return function(prog) {
      return function(zoneState) {
        return execR(re)(zoneState)(discard13(runElements(zone)(prog.elements))(function() {
          return runCamera(prog.cameraMap);
        }));
      };
    };
  };
};
var clearZone = function(re) {
  return function(z) {
    return function __do2() {
      $$delete3(z)(re.programs)();
      $$delete3(z)(re.zoneStates)();
      return log2("LocoMotion WARNING: clearZone is not properly implemented yet (needs to delete assets!)")();
    };
  };
};
var animateZone = function(re) {
  return function(z) {
    return function __do2() {
      var x = read3(z)(re.programs)();
      if (x instanceof Nothing) {
        log2("LocoMotion ERROR: animateZone called for zone with no program")();
        return unit;
      }
      ;
      if (x instanceof Just) {
        var y = read3(z)(re.zoneStates)();
        var zoneState = function() {
          if (y instanceof Just) {
            return y.value0;
          }
          ;
          if (y instanceof Nothing) {
            return defaultZoneState;
          }
          ;
          throw new Error("Failed pattern match at RenderEngine (line 135, column 23 - line 137, column 52): " + [y.constructor.name]);
        }();
        var rEnv = read(re.renderEnvironment)();
        var zoneState$prime = runProgram(z)(rEnv)(x.value0)(zoneState)();
        return write3(z)(zoneState$prime)(re.zoneStates)();
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 129, column 3 - line 140, column 47): " + [x.constructor.name]);
    };
  };
};

// output/Main/index.js
var setTempo = function(re) {
  return function(t) {
    return function __do2() {
      var rEnv = read(re.renderEnvironment)();
      return write({
        tempo: fromForeignTempo(t),
        camera: rEnv.camera,
        cycleDur: rEnv.cycleDur,
        defaultLight: rEnv.defaultLight,
        delta: rEnv.delta,
        nCycles: rEnv.nCycles,
        renderer: rEnv.renderer,
        scene: rEnv.scene
      })(re.renderEnvironment)();
    };
  };
};
var preAnimate2 = preAnimate;
var postAnimate2 = postAnimate;
var launch2 = launch;
var evaluate2 = function(re) {
  return function(zone) {
    return function(x) {
      return function __do2() {
        var y = evaluate(re)(zone)(x)();
        if (y instanceof Just) {
          return {
            success: false,
            error: y.value0
          };
        }
        ;
        if (y instanceof Nothing) {
          return {
            success: true,
            error: ""
          };
        }
        ;
        throw new Error("Failed pattern match at Main (line 26, column 3 - line 28, column 51): " + [y.constructor.name]);
      };
    };
  };
};
var clearZone2 = clearZone;
var animateZone2 = animateZone;
export {
  animateZone2 as animateZone,
  clearZone2 as clearZone,
  evaluate2 as evaluate,
  launch2 as launch,
  postAnimate2 as postAnimate,
  preAnimate2 as preAnimate,
  setTempo
};
