(function(l, i, v, e) { v = l.createElement(i); v.async = 1; v.src = '//' + (location.host || 'localhost').split(':')[0] + ':35729/livereload.js?snipver=1'; e = l.getElementsByTagName(i)[0]; e.parentNode.insertBefore(v, e)})(document, 'script');
var gyges = (function (exports) {
'use strict';

var out_of_memory = /* tuple */[
  "Out_of_memory",
  0
];

var sys_error = /* tuple */[
  "Sys_error",
  -1
];

var failure = /* tuple */[
  "Failure",
  -2
];

var invalid_argument = /* tuple */[
  "Invalid_argument",
  -3
];

var end_of_file = /* tuple */[
  "End_of_file",
  -4
];

var division_by_zero = /* tuple */[
  "Division_by_zero",
  -5
];

var not_found = /* tuple */[
  "Not_found",
  -6
];

var match_failure = /* tuple */[
  "Match_failure",
  -7
];

var stack_overflow = /* tuple */[
  "Stack_overflow",
  -8
];

var sys_blocked_io = /* tuple */[
  "Sys_blocked_io",
  -9
];

var assert_failure = /* tuple */[
  "Assert_failure",
  -10
];

var undefined_recursive_module = /* tuple */[
  "Undefined_recursive_module",
  -11
];

out_of_memory.tag = 248;

sys_error.tag = 248;

failure.tag = 248;

invalid_argument.tag = 248;

end_of_file.tag = 248;

division_by_zero.tag = 248;

not_found.tag = 248;

match_failure.tag = 248;

stack_overflow.tag = 248;

sys_blocked_io.tag = 248;

assert_failure.tag = 248;

undefined_recursive_module.tag = 248;


/*  Not a pure module */

function caml_array_sub(x, offset, len) {
  var result = new Array(len);
  var j = 0;
  var i = offset;
  while(j < len) {
    result[j] = x[i];
    j = j + 1 | 0;
    i = i + 1 | 0;
  }
  return result;
}

function caml_array_set(xs, index, newval) {
  if (index < 0 || index >= xs.length) {
    throw [
          invalid_argument,
          "index out of bounds"
        ];
  } else {
    xs[index] = newval;
    return /* () */0;
  }
}

function caml_array_get(xs, index) {
  if (index < 0 || index >= xs.length) {
    throw [
          invalid_argument,
          "index out of bounds"
        ];
  } else {
    return xs[index];
  }
}

function caml_make_vect(len, init) {
  var b = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    b[i] = init;
  }
  return b;
}


/* No side effect */

function app(_f, _args) {
  while(true) {
    var args = _args;
    var f = _f;
    var arity = f.length;
    var arity$1 = arity ? arity : 1;
    var len = args.length;
    var d = arity$1 - len | 0;
    if (d) {
      if (d < 0) {
        _args = caml_array_sub(args, arity$1, -d | 0);
        _f = f.apply(null, caml_array_sub(args, 0, arity$1));
        continue ;
        
      } else {
        return (function(f,args){
        return function (x) {
          return app(f, args.concat(/* array */[x]));
        }
        }(f,args));
      }
    } else {
      return f.apply(null, args);
    }
  }
}

function curry_1(o, a0, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[a0]);
  } else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return o(a0);
      case 2 : 
          return (function (param) {
              return o(a0, param);
            });
      case 3 : 
          return (function (param, param$1) {
              return o(a0, param, param$1);
            });
      case 4 : 
          return (function (param, param$1, param$2) {
              return o(a0, param, param$1, param$2);
            });
      case 5 : 
          return (function (param, param$1, param$2, param$3) {
              return o(a0, param, param$1, param$2, param$3);
            });
      case 6 : 
          return (function (param, param$1, param$2, param$3, param$4) {
              return o(a0, param, param$1, param$2, param$3, param$4);
            });
      case 7 : 
          return (function (param, param$1, param$2, param$3, param$4, param$5) {
              return o(a0, param, param$1, param$2, param$3, param$4, param$5);
            });
      
    }
  }
}

function _1(o, a0) {
  var arity = o.length;
  if (arity === 1) {
    return o(a0);
  } else {
    return curry_1(o, a0, arity);
  }
}

function curry_2(o, a0, a1, arity) {
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1
              ]);
  } else {
    switch (arity) {
      case 0 : 
      case 1 : 
          return app(o(a0), /* array */[a1]);
      case 2 : 
          return o(a0, a1);
      case 3 : 
          return (function (param) {
              return o(a0, a1, param);
            });
      case 4 : 
          return (function (param, param$1) {
              return o(a0, a1, param, param$1);
            });
      case 5 : 
          return (function (param, param$1, param$2) {
              return o(a0, a1, param, param$1, param$2);
            });
      case 6 : 
          return (function (param, param$1, param$2, param$3) {
              return o(a0, a1, param, param$1, param$2, param$3);
            });
      case 7 : 
          return (function (param, param$1, param$2, param$3, param$4) {
              return o(a0, a1, param, param$1, param$2, param$3, param$4);
            });
      
    }
  }
}

function _2(o, a0, a1) {
  var arity = o.length;
  if (arity === 2) {
    return o(a0, a1);
  } else {
    return curry_2(o, a0, a1, arity);
  }
}

function curry_3(o, a0, a1, a2, arity) {
  var exit = 0;
  if (arity > 7 || arity < 0) {
    return app(o, /* array */[
                a0,
                a1,
                a2
              ]);
  } else {
    switch (arity) {
      case 0 : 
      case 1 : 
          exit = 1;
          break;
      case 2 : 
          return app(o(a0, a1), /* array */[a2]);
      case 3 : 
          return o(a0, a1, a2);
      case 4 : 
          return (function (param) {
              return o(a0, a1, a2, param);
            });
      case 5 : 
          return (function (param, param$1) {
              return o(a0, a1, a2, param, param$1);
            });
      case 6 : 
          return (function (param, param$1, param$2) {
              return o(a0, a1, a2, param, param$1, param$2);
            });
      case 7 : 
          return (function (param, param$1, param$2, param$3) {
              return o(a0, a1, a2, param, param$1, param$2, param$3);
            });
      
    }
  }
  if (exit === 1) {
    return app(o(a0), /* array */[
                a1,
                a2
              ]);
  }
  
}

function _3(o, a0, a1, a2) {
  var arity = o.length;
  if (arity === 3) {
    return o(a0, a1, a2);
  } else {
    return curry_3(o, a0, a1, a2, arity);
  }
}


/* No side effect */

function __(tag, block) {
  block.tag = tag;
  return block;
}


/* No side effect */

function caml_int_compare(x, y) {
  if (x < y) {
    return -1;
  } else if (x === y) {
    return 0;
  } else {
    return 1;
  }
}

function caml_compare(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === b) {
      return 0;
    } else {
      var a_type = typeof a;
      var b_type = typeof b;
      if (a_type === "string") {
        var x = a;
        var y = b;
        if (x < y) {
          return -1;
        } else if (x === y) {
          return 0;
        } else {
          return 1;
        }
      } else {
        var is_a_number = +(a_type === "number");
        var is_b_number = +(b_type === "number");
        if (is_a_number !== 0) {
          if (is_b_number !== 0) {
            return caml_int_compare(a, b);
          } else {
            return -1;
          }
        } else if (is_b_number !== 0) {
          return 1;
        } else if (a_type === "boolean" || a_type === "undefined" || a === null) {
          var x$1 = a;
          var y$1 = b;
          if (x$1 === y$1) {
            return 0;
          } else if (x$1 < y$1) {
            return -1;
          } else {
            return 1;
          }
        } else if (a_type === "function" || b_type === "function") {
          throw [
                invalid_argument,
                "compare: functional value"
              ];
        } else {
          var tag_a = a.tag | 0;
          var tag_b = b.tag | 0;
          if (tag_a === 250) {
            _a = a[0];
            continue ;
            
          } else if (tag_b === 250) {
            _b = b[0];
            continue ;
            
          } else if (tag_a === 248) {
            return caml_int_compare(a[1], b[1]);
          } else if (tag_a === 251) {
            throw [
                  invalid_argument,
                  "equal: abstract value"
                ];
          } else if (tag_a !== tag_b) {
            if (tag_a < tag_b) {
              return -1;
            } else {
              return 1;
            }
          } else {
            var len_a = a.length | 0;
            var len_b = b.length | 0;
            if (len_a === len_b) {
              var a$1 = a;
              var b$1 = b;
              var _i = 0;
              var same_length = len_a;
              while(true) {
                var i = _i;
                if (i === same_length) {
                  return 0;
                } else {
                  var res = caml_compare(a$1[i], b$1[i]);
                  if (res !== 0) {
                    return res;
                  } else {
                    _i = i + 1 | 0;
                    continue ;
                    
                  }
                }
              }
            } else if (len_a < len_b) {
              var a$2 = a;
              var b$2 = b;
              var _i$1 = 0;
              var short_length = len_a;
              while(true) {
                var i$1 = _i$1;
                if (i$1 === short_length) {
                  return -1;
                } else {
                  var res$1 = caml_compare(a$2[i$1], b$2[i$1]);
                  if (res$1 !== 0) {
                    return res$1;
                  } else {
                    _i$1 = i$1 + 1 | 0;
                    continue ;
                    
                  }
                }
              }
            } else {
              var a$3 = a;
              var b$3 = b;
              var _i$2 = 0;
              var short_length$1 = len_b;
              while(true) {
                var i$2 = _i$2;
                if (i$2 === short_length$1) {
                  return 1;
                } else {
                  var res$2 = caml_compare(a$3[i$2], b$3[i$2]);
                  if (res$2 !== 0) {
                    return res$2;
                  } else {
                    _i$2 = i$2 + 1 | 0;
                    continue ;
                    
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function caml_equal(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === b) {
      return /* true */1;
    } else {
      var a_type = typeof a;
      if (a_type === "string" || a_type === "number" || a_type === "boolean" || a_type === "undefined" || a === null) {
        return /* false */0;
      } else {
        var b_type = typeof b;
        if (a_type === "function" || b_type === "function") {
          throw [
                invalid_argument,
                "equal: functional value"
              ];
        } else if (b_type === "number" || b_type === "undefined" || b === null) {
          return /* false */0;
        } else {
          var tag_a = a.tag | 0;
          var tag_b = b.tag | 0;
          if (tag_a === 250) {
            _a = a[0];
            continue ;
            
          } else if (tag_b === 250) {
            _b = b[0];
            continue ;
            
          } else if (tag_a === 248) {
            return +(a[1] === b[1]);
          } else if (tag_a === 251) {
            throw [
                  invalid_argument,
                  "equal: abstract value"
                ];
          } else if (tag_a !== tag_b) {
            return /* false */0;
          } else {
            var len_a = a.length | 0;
            var len_b = b.length | 0;
            if (len_a === len_b) {
              var a$1 = a;
              var b$1 = b;
              var _i = 0;
              var same_length = len_a;
              while(true) {
                var i = _i;
                if (i === same_length) {
                  return /* true */1;
                } else if (caml_equal(a$1[i], b$1[i])) {
                  _i = i + 1 | 0;
                  continue ;
                  
                } else {
                  return /* false */0;
                }
              }
            } else {
              return /* false */0;
            }
          }
        }
      }
    }
  }
}

function caml_greaterequal(a, b) {
  return +(caml_compare(a, b) >= 0);
}

function caml_lessequal(a, b) {
  return +(caml_compare(a, b) <= 0);
}


/* No side effect */

/* stdin Not a pure module */

/* No side effect */

var imul = ( Math.imul || function (x,y) {
  y |= 0; return ((((x >> 16) * y) << 16) + (x & 0xffff) * y)|0; 
}
);


/* imul Not a pure module */

/* repeat Not a pure module */

/* two_ptr_32_dbl Not a pure module */

/* float_of_string Not a pure module */

function caml_create_string(len) {
  if (len < 0) {
    throw [
          invalid_argument,
          "String.create"
        ];
  } else {
    return new Array(len);
  }
}

function caml_fill_string(s, i, l, c) {
  if (l > 0) {
    for(var k = i ,k_finish = (l + i | 0) - 1 | 0; k <= k_finish; ++k){
      s[k] = c;
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function caml_blit_string(s1, i1, s2, i2, len) {
  if (len > 0) {
    var off1 = s1.length - i1 | 0;
    if (len <= off1) {
      for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
        s2[i2 + i | 0] = s1.charCodeAt(i1 + i | 0);
      }
      return /* () */0;
    } else {
      for(var i$1 = 0 ,i_finish$1 = off1 - 1 | 0; i$1 <= i_finish$1; ++i$1){
        s2[i2 + i$1 | 0] = s1.charCodeAt(i1 + i$1 | 0);
      }
      for(var i$2 = off1 ,i_finish$2 = len - 1 | 0; i$2 <= i_finish$2; ++i$2){
        s2[i2 + i$2 | 0] = /* "\000" */0;
      }
      return /* () */0;
    }
  } else {
    return 0;
  }
}

function caml_blit_bytes(s1, i1, s2, i2, len) {
  if (len > 0) {
    if (s1 === s2) {
      var s1$1 = s1;
      var i1$1 = i1;
      var i2$1 = i2;
      var len$1 = len;
      if (i1$1 < i2$1) {
        var range_a = (s1$1.length - i2$1 | 0) - 1 | 0;
        var range_b = len$1 - 1 | 0;
        var range = range_a > range_b ? range_b : range_a;
        for(var j = range; j >= 0; --j){
          s1$1[i2$1 + j | 0] = s1$1[i1$1 + j | 0];
        }
        return /* () */0;
      } else if (i1$1 > i2$1) {
        var range_a$1 = (s1$1.length - i1$1 | 0) - 1 | 0;
        var range_b$1 = len$1 - 1 | 0;
        var range$1 = range_a$1 > range_b$1 ? range_b$1 : range_a$1;
        for(var k = 0; k <= range$1; ++k){
          s1$1[i2$1 + k | 0] = s1$1[i1$1 + k | 0];
        }
        return /* () */0;
      } else {
        return 0;
      }
    } else {
      var off1 = s1.length - i1 | 0;
      if (len <= off1) {
        for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
          s2[i2 + i | 0] = s1[i1 + i | 0];
        }
        return /* () */0;
      } else {
        for(var i$1 = 0 ,i_finish$1 = off1 - 1 | 0; i$1 <= i_finish$1; ++i$1){
          s2[i2 + i$1 | 0] = s1[i1 + i$1 | 0];
        }
        for(var i$2 = off1 ,i_finish$2 = len - 1 | 0; i$2 <= i_finish$2; ++i$2){
          s2[i2 + i$2 | 0] = /* "\000" */0;
        }
        return /* () */0;
      }
    }
  } else {
    return 0;
  }
}

function bytes_to_string(a) {
  var bytes = a;
  var i = 0;
  var len = a.length;
  var s = "";
  var s_len = len;
  if (i === 0 && len <= 4096 && len === bytes.length) {
    return String.fromCharCode.apply(null,bytes);
  } else {
    var offset = 0;
    while(s_len > 0) {
      var next = s_len < 1024 ? s_len : 1024;
      var tmp_bytes = new Array(next);
      caml_blit_bytes(bytes, offset, tmp_bytes, 0, next);
      s = s + String.fromCharCode.apply(null,tmp_bytes);
      s_len = s_len - next | 0;
      offset = offset + next | 0;
    }
    return s;
  }
}

function get(s, i) {
  if (i < 0 || i >= s.length) {
    throw [
          invalid_argument,
          "index out of bounds"
        ];
  } else {
    return s.charCodeAt(i);
  }
}


/* No side effect */

var id = [0];

function get_id() {
  id[0] += 1;
  return id[0];
}

function create(str) {
  var v_001 = get_id(/* () */0);
  var v = /* tuple */[
    str,
    v_001
  ];
  v.tag = 248;
  return v;
}


/* No side effect */

/* not_implemented Not a pure module */

/* No side effect */

var Exit = create("Pervasives.Exit");

function min(x, y) {
  if (caml_lessequal(x, y)) {
    return x;
  } else {
    return y;
  }
}

function max(x, y) {
  if (caml_greaterequal(x, y)) {
    return x;
  } else {
    return y;
  }
}

function string_of_int(param) {
  return "" + param;
}

function $at(l1, l2) {
  if (l1) {
    return /* :: */[
            l1[0],
            $at(l1[1], l2)
          ];
  } else {
    return l2;
  }
}


/* No side effect */

function length(l) {
  var _len = 0;
  var _param = l;
  while(true) {
    var param = _param;
    var len = _len;
    if (param) {
      _param = param[1];
      _len = len + 1 | 0;
      continue ;
      
    } else {
      return len;
    }
  }
}

function hd(param) {
  if (param) {
    return param[0];
  } else {
    throw [
          failure,
          "hd"
        ];
  }
}

function tl(param) {
  if (param) {
    return param[1];
  } else {
    throw [
          failure,
          "tl"
        ];
  }
}

function rev_append(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      _l2 = /* :: */[
        l1[0],
        l2
      ];
      _l1 = l1[1];
      continue ;
      
    } else {
      return l2;
    }
  }
}

function rev(l) {
  return rev_append(l, /* [] */0);
}

function flatten(param) {
  if (param) {
    return $at(param[0], flatten(param[1]));
  } else {
    return /* [] */0;
  }
}

function map(f, param) {
  if (param) {
    var r = _1(f, param[0]);
    return /* :: */[
            r,
            map(f, param[1])
          ];
  } else {
    return /* [] */0;
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      _1(f, param[0]);
      _param = param[1];
      continue ;
      
    } else {
      return /* () */0;
    }
  }
}

function fold_left(f, _accu, _l) {
  while(true) {
    var l = _l;
    var accu = _accu;
    if (l) {
      _l = l[1];
      _accu = _2(f, accu, l[0]);
      continue ;
      
    } else {
      return accu;
    }
  }
}

function fold_left2(f, _accu, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    var accu = _accu;
    if (l1) {
      if (l2) {
        _l2 = l2[1];
        _l1 = l1[1];
        _accu = _3(f, accu, l1[0], l2[0]);
        continue ;
        
      } else {
        throw [
              invalid_argument,
              "List.fold_left2"
            ];
      }
    } else if (l2) {
      throw [
            invalid_argument,
            "List.fold_left2"
          ];
    } else {
      return accu;
    }
  }
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (_1(p, param[0])) {
        _param = param[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  }
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (_1(p, param[0])) {
        return /* true */1;
      } else {
        _param = param[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  }
}

function find_all(p) {
  return (function (param) {
      var _accu = /* [] */0;
      var _param = param;
      while(true) {
        var param$1 = _param;
        var accu = _accu;
        if (param$1) {
          var l = param$1[1];
          var x = param$1[0];
          if (_1(p, x)) {
            _param = l;
            _accu = /* :: */[
              x,
              accu
            ];
            continue ;
            
          } else {
            _param = l;
            continue ;
            
          }
        } else {
          return rev_append(accu, /* [] */0);
        }
      }
    });
}

function chop(_k, _l) {
  while(true) {
    var l = _l;
    var k = _k;
    if (k) {
      if (l) {
        _l = l[1];
        _k = k - 1 | 0;
        continue ;
        
      } else {
        throw [
              assert_failure,
              [
                "list.ml",
                223,
                11
              ]
            ];
      }
    } else {
      return l;
    }
  }
}

function sort_uniq(cmp, l) {
  var sort = function (n, l) {
    var exit$$1 = 0;
    if (n !== 2) {
      if (n !== 3) {
        exit$$1 = 1;
      } else if (l) {
        var match = l[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var x3 = match$1[0];
            var x2 = match[0];
            var x1 = l[0];
            var c = _2(cmp, x1, x2);
            if (c) {
              if (c < 0) {
                var c$1 = _2(cmp, x2, x3);
                if (c$1) {
                  if (c$1 < 0) {
                    return /* :: */[
                            x1,
                            /* :: */[
                              x2,
                              /* :: */[
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  } else {
                    var c$2 = _2(cmp, x1, x3);
                    if (c$2) {
                      if (c$2 < 0) {
                        return /* :: */[
                                x1,
                                /* :: */[
                                  x3,
                                  /* :: */[
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      } else {
                        return /* :: */[
                                x3,
                                /* :: */[
                                  x1,
                                  /* :: */[
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    } else {
                      return /* :: */[
                              x1,
                              /* :: */[
                                x2,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                } else {
                  return /* :: */[
                          x1,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              } else {
                var c$3 = _2(cmp, x1, x3);
                if (c$3) {
                  if (c$3 < 0) {
                    return /* :: */[
                            x2,
                            /* :: */[
                              x1,
                              /* :: */[
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  } else {
                    var c$4 = _2(cmp, x2, x3);
                    if (c$4) {
                      if (c$4 < 0) {
                        return /* :: */[
                                x2,
                                /* :: */[
                                  x3,
                                  /* :: */[
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      } else {
                        return /* :: */[
                                x3,
                                /* :: */[
                                  x2,
                                  /* :: */[
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    } else {
                      return /* :: */[
                              x2,
                              /* :: */[
                                x1,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                } else {
                  return /* :: */[
                          x2,
                          /* :: */[
                            x1,
                            /* [] */0
                          ]
                        ];
                }
              }
            } else {
              var c$5 = _2(cmp, x2, x3);
              if (c$5) {
                if (c$5 < 0) {
                  return /* :: */[
                          x2,
                          /* :: */[
                            x3,
                            /* [] */0
                          ]
                        ];
                } else {
                  return /* :: */[
                          x3,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              } else {
                return /* :: */[
                        x2,
                        /* [] */0
                      ];
              }
            }
          } else {
            exit$$1 = 1;
          }
        } else {
          exit$$1 = 1;
        }
      } else {
        exit$$1 = 1;
      }
    } else if (l) {
      var match$2 = l[1];
      if (match$2) {
        var x2$1 = match$2[0];
        var x1$1 = l[0];
        var c$6 = _2(cmp, x1$1, x2$1);
        if (c$6) {
          if (c$6 < 0) {
            return /* :: */[
                    x1$1,
                    /* :: */[
                      x2$1,
                      /* [] */0
                    ]
                  ];
          } else {
            return /* :: */[
                    x2$1,
                    /* :: */[
                      x1$1,
                      /* [] */0
                    ]
                  ];
          }
        } else {
          return /* :: */[
                  x1$1,
                  /* [] */0
                ];
        }
      } else {
        exit$$1 = 1;
      }
    } else {
      exit$$1 = 1;
    }
    if (exit$$1 === 1) {
      var n1 = (n >> 1);
      var n2 = n - n1 | 0;
      var l2 = chop(n1, l);
      var s1 = rev_sort(n1, l);
      var s2 = rev_sort(n2, l2);
      var _l1 = s1;
      var _l2 = s2;
      var _accu = /* [] */0;
      while(true) {
        var accu = _accu;
        var l2$1 = _l2;
        var l1 = _l1;
        if (l1) {
          if (l2$1) {
            var t2 = l2$1[1];
            var h2 = l2$1[0];
            var t1 = l1[1];
            var h1 = l1[0];
            var c$7 = _2(cmp, h1, h2);
            if (c$7) {
              if (c$7 > 0) {
                _accu = /* :: */[
                  h1,
                  accu
                ];
                _l1 = t1;
                continue ;
                
              } else {
                _accu = /* :: */[
                  h2,
                  accu
                ];
                _l2 = t2;
                continue ;
                
              }
            } else {
              _accu = /* :: */[
                h1,
                accu
              ];
              _l2 = t2;
              _l1 = t1;
              continue ;
              
            }
          } else {
            return rev_append(l1, accu);
          }
        } else {
          return rev_append(l2$1, accu);
        }
      }
    }
    
  };
  var rev_sort = function (n, l) {
    var exit$$1 = 0;
    if (n !== 2) {
      if (n !== 3) {
        exit$$1 = 1;
      } else if (l) {
        var match = l[1];
        if (match) {
          var match$1 = match[1];
          if (match$1) {
            var x3 = match$1[0];
            var x2 = match[0];
            var x1 = l[0];
            var c = _2(cmp, x1, x2);
            if (c) {
              if (c > 0) {
                var c$1 = _2(cmp, x2, x3);
                if (c$1) {
                  if (c$1 > 0) {
                    return /* :: */[
                            x1,
                            /* :: */[
                              x2,
                              /* :: */[
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  } else {
                    var c$2 = _2(cmp, x1, x3);
                    if (c$2) {
                      if (c$2 > 0) {
                        return /* :: */[
                                x1,
                                /* :: */[
                                  x3,
                                  /* :: */[
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      } else {
                        return /* :: */[
                                x3,
                                /* :: */[
                                  x1,
                                  /* :: */[
                                    x2,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    } else {
                      return /* :: */[
                              x1,
                              /* :: */[
                                x2,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                } else {
                  return /* :: */[
                          x1,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              } else {
                var c$3 = _2(cmp, x1, x3);
                if (c$3) {
                  if (c$3 > 0) {
                    return /* :: */[
                            x2,
                            /* :: */[
                              x1,
                              /* :: */[
                                x3,
                                /* [] */0
                              ]
                            ]
                          ];
                  } else {
                    var c$4 = _2(cmp, x2, x3);
                    if (c$4) {
                      if (c$4 > 0) {
                        return /* :: */[
                                x2,
                                /* :: */[
                                  x3,
                                  /* :: */[
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      } else {
                        return /* :: */[
                                x3,
                                /* :: */[
                                  x2,
                                  /* :: */[
                                    x1,
                                    /* [] */0
                                  ]
                                ]
                              ];
                      }
                    } else {
                      return /* :: */[
                              x2,
                              /* :: */[
                                x1,
                                /* [] */0
                              ]
                            ];
                    }
                  }
                } else {
                  return /* :: */[
                          x2,
                          /* :: */[
                            x1,
                            /* [] */0
                          ]
                        ];
                }
              }
            } else {
              var c$5 = _2(cmp, x2, x3);
              if (c$5) {
                if (c$5 > 0) {
                  return /* :: */[
                          x2,
                          /* :: */[
                            x3,
                            /* [] */0
                          ]
                        ];
                } else {
                  return /* :: */[
                          x3,
                          /* :: */[
                            x2,
                            /* [] */0
                          ]
                        ];
                }
              } else {
                return /* :: */[
                        x2,
                        /* [] */0
                      ];
              }
            }
          } else {
            exit$$1 = 1;
          }
        } else {
          exit$$1 = 1;
        }
      } else {
        exit$$1 = 1;
      }
    } else if (l) {
      var match$2 = l[1];
      if (match$2) {
        var x2$1 = match$2[0];
        var x1$1 = l[0];
        var c$6 = _2(cmp, x1$1, x2$1);
        if (c$6) {
          if (c$6 > 0) {
            return /* :: */[
                    x1$1,
                    /* :: */[
                      x2$1,
                      /* [] */0
                    ]
                  ];
          } else {
            return /* :: */[
                    x2$1,
                    /* :: */[
                      x1$1,
                      /* [] */0
                    ]
                  ];
          }
        } else {
          return /* :: */[
                  x1$1,
                  /* [] */0
                ];
        }
      } else {
        exit$$1 = 1;
      }
    } else {
      exit$$1 = 1;
    }
    if (exit$$1 === 1) {
      var n1 = (n >> 1);
      var n2 = n - n1 | 0;
      var l2 = chop(n1, l);
      var s1 = sort(n1, l);
      var s2 = sort(n2, l2);
      var _l1 = s1;
      var _l2 = s2;
      var _accu = /* [] */0;
      while(true) {
        var accu = _accu;
        var l2$1 = _l2;
        var l1 = _l1;
        if (l1) {
          if (l2$1) {
            var t2 = l2$1[1];
            var h2 = l2$1[0];
            var t1 = l1[1];
            var h1 = l1[0];
            var c$7 = _2(cmp, h1, h2);
            if (c$7) {
              if (c$7 < 0) {
                _accu = /* :: */[
                  h1,
                  accu
                ];
                _l1 = t1;
                continue ;
                
              } else {
                _accu = /* :: */[
                  h2,
                  accu
                ];
                _l2 = t2;
                continue ;
                
              }
            } else {
              _accu = /* :: */[
                h1,
                accu
              ];
              _l2 = t2;
              _l1 = t1;
              continue ;
              
            }
          } else {
            return rev_append(l1, accu);
          }
        } else {
          return rev_append(l2$1, accu);
        }
      }
    }
    
  };
  var len = length(l);
  if (len < 2) {
    return l;
  } else {
    return sort(len, l);
  }
}

var concat = flatten;

var filter = find_all;


/* No side effect */

var $$Error = create("Js_exn.Error");


/* No side effect */

function to_list(a) {
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    } else {
      _res = /* :: */[
        a[i],
        res
      ];
      _i = i - 1 | 0;
      continue ;
      
    }
  }
}

var Bottom = create("Array.Bottom");


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function compare_case(c1, c2) {
  if (typeof c1 === "number") {
    if (c1 !== 0) {
      if (typeof c2 === "number" && c2 !== 0) {
        return 0;
      } else {
        return -1;
      }
    } else if (typeof c2 === "number") {
      if (c2 !== 0) {
        return 1;
      } else {
        return 0;
      }
    } else {
      return -1;
    }
  } else if (typeof c2 === "number") {
    return 1;
  } else {
    return (c1[1] - c2[1] | 0) + imul(6, c1[0] - c2[0] | 0) | 0;
  }
}

function init_jeu$1() {
  return /* [] */0;
}

function pion_dans_case(_partie, $$case) {
  while(true) {
    var partie = _partie;
    if (partie) {
      var hd$$1 = partie[0];
      if (caml_equal(hd$$1[0], $$case)) {
        return hd$$1[1];
      } else {
        _partie = partie[1];
        continue ;
        
      }
    } else {
      return /* Vide */0;
    }
  }
}

function mise_a_jour(partie, depl) {
  var c1 = depl[0];
  var p1 = pion_dans_case(partie, c1);
  return /* :: */[
          /* tuple */[
            c1,
            /* Vide */0
          ],
          /* :: */[
            /* tuple */[
              depl[1],
              p1
            ],
            partie
          ]
        ];
}

function meme_chemin(d1, d2) {
  var t = d2[1];
  var z = d2[0];
  var y = d1[1];
  var x = d1[0];
  if (caml_equal(x, z) && caml_equal(y, t) || caml_equal(x, t) && caml_equal(y, z)) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function par_paire(l) {
  if (l) {
    var match = l[1];
    if (match) {
      var y = match[0];
      return /* :: */[
              /* tuple */[
                l[0],
                y
              ],
              par_paire(/* :: */[
                    y,
                    match[1]
                  ])
            ];
    } else {
      return /* [] */0;
    }
  } else {
    return /* [] */0;
  }
}

function pas_recoupe(l) {
  if (l) {
    if (l[1]) {
      var lp = par_paire(l);
      return for_all((function (x) {
                    return 1 - meme_chemin(x, hd(lp));
                  }), tl(lp));
    } else {
      return /* true */1;
    }
  } else {
    return /* true */1;
  }
}

function voisins_bords(pos) {
  var yp = pos[1];
  var xp = pos[0];
  return filter((function (param) {
                  var y = param[1];
                  var x = param[0];
                  if (x > 0 && x < 7 && y > 0) {
                    return +(y < 7);
                  } else {
                    return /* false */0;
                  }
                }))(/* :: */[
              /* tuple */[
                xp - 1 | 0,
                yp
              ],
              /* :: */[
                /* tuple */[
                  xp + 1 | 0,
                  yp
                ],
                /* :: */[
                  /* tuple */[
                    xp,
                    yp - 1 | 0
                  ],
                  /* :: */[
                    /* tuple */[
                      xp,
                      yp + 1 | 0
                    ],
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

function voisins(pos) {
  var lambd = function (param) {
    return map((function (param) {
                  return /* Case */[
                          param[0],
                          param[1]
                        ];
                }), param);
  };
  if (typeof pos === "number") {
    if (pos !== 0) {
      return map((function (i) {
                    return /* Case */[
                            i,
                            1
                          ];
                  }), /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* :: */[
                        4,
                        /* :: */[
                          5,
                          /* :: */[
                            6,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]);
    } else {
      return map((function (i) {
                    return /* Case */[
                            i,
                            6
                          ];
                  }), /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* :: */[
                        4,
                        /* :: */[
                          5,
                          /* :: */[
                            6,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]);
    }
  } else {
    var y = pos[1];
    var x = pos[0];
    if (y !== 1) {
      if (y !== 6) {
        return lambd(voisins_bords(/* tuple */[
                        x,
                        y
                      ]));
      } else {
        return /* :: */[
                /* CaseNord */0,
                lambd(voisins_bords(/* tuple */[
                          x,
                          6
                        ]))
              ];
      }
    } else {
      return /* :: */[
              /* CaseSud */1,
              lambd(voisins_bords(/* tuple */[
                        x,
                        1
                      ]))
            ];
    }
  }
}

function int_de_pos(p) {
  if (p) {
    return p[0];
  } else {
    return 0;
  }
}

function chemins(partie, n, lchemin) {
  if (n !== 0) {
    if (n !== 1) {
      var dernier = hd(lchemin);
      var lvoisins = filter((function (p) {
                if (pion_dans_case(partie, p) === /* Vide */0 && p !== /* CaseNord */0) {
                  return +(p !== /* CaseSud */1);
                } else {
                  return /* false */0;
                }
              }))(voisins(dernier));
      var param = filter(pas_recoupe)(map((function (x) {
                  return /* :: */[
                          x,
                          lchemin
                        ];
                }), lvoisins));
      return concat(map((function (lch) {
                        return chemins(partie, n - 1 | 0, lch);
                      }), param));
    } else {
      var dernier$1 = hd(lchemin);
      var lvoisins$1 = voisins(dernier$1);
      var param$1 = filter(pas_recoupe)(map((function (x) {
                  return /* :: */[
                          x,
                          lchemin
                        ];
                }), lvoisins$1));
      return concat(map((function (lch) {
                        var dern = hd(lch);
                        return chemins(partie, int_de_pos(pion_dans_case(partie, dern)), lch);
                      }), param$1));
    }
  } else {
    return /* :: */[
            lchemin,
            /* [] */0
          ];
  }
}

function depl_possibles(jeu, $$case) {
  var match = pion_dans_case(jeu, $$case);
  if (match) {
    var liste_chemins = chemins(jeu, match[0], /* :: */[
          $$case,
          /* [] */0
        ]);
    return sort_uniq(compare_case, map(hd, liste_chemins));
  } else {
    return /* [] */0;
  }
}

function departs_possibles(jeu, joueur) {
  var premiere_ligne = function (_i) {
    while(true) {
      var i = _i;
      var pion_de_ligne = filter((function (x) {
                return +(x !== /* Vide */0);
              }))(map((function(i){
              return function (x) {
                return pion_dans_case(jeu, /* Case */[
                            x,
                            i
                          ]);
              }
              }(i)), /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
                      4,
                      /* :: */[
                        5,
                        /* :: */[
                          6,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]));
      if (pion_de_ligne) {
        return i;
      } else {
        _i = joueur !== 0 ? i + 1 | 0 : i - 1 | 0;
        continue ;
        
      }
    }
  };
  var i = joueur !== 0 ? 1 : 6;
  var ligne = premiere_ligne(i);
  return filter((function (x) {
                  return +(pion_dans_case(jeu, x) !== /* Vide */0);
                }))(map((function (x) {
                    return /* Case */[
                            x,
                            ligne
                          ];
                  }), /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* :: */[
                        4,
                        /* :: */[
                          5,
                          /* :: */[
                            6,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]));
}

var encours$1 = map((function (param) {
        var match = param[0];
        return /* tuple */[
                /* Case */[
                  match[0],
                  match[1]
                ],
                /* Val */[param[1]]
              ];
      }), /* :: */[
      /* tuple */[
        /* tuple */[
          1,
          1
        ],
        3
      ],
      /* :: */[
        /* tuple */[
          /* tuple */[
            3,
            1
          ],
          1
        ],
        /* :: */[
          /* tuple */[
            /* tuple */[
              4,
              1
            ],
            3
          ],
          /* :: */[
            /* tuple */[
              /* tuple */[
                6,
                1
              ],
              1
            ],
            /* :: */[
              /* tuple */[
                /* tuple */[
                  4,
                  2
                ],
                2
              ],
              /* :: */[
                /* tuple */[
                  /* tuple */[
                    3,
                    3
                  ],
                  2
                ],
                /* :: */[
                  /* tuple */[
                    /* tuple */[
                      5,
                      3
                    ],
                    3
                  ],
                  /* :: */[
                    /* tuple */[
                      /* tuple */[
                        3,
                        4
                      ],
                      3
                    ],
                    /* :: */[
                      /* tuple */[
                        /* tuple */[
                          1,
                          6
                        ],
                        1
                      ],
                      /* :: */[
                        /* tuple */[
                          /* tuple */[
                            3,
                            6
                          ],
                          2
                        ],
                        /* :: */[
                          /* tuple */[
                            /* tuple */[
                              4,
                              6
                            ],
                            1
                          ],
                          /* :: */[
                            /* tuple */[
                              /* tuple */[
                                5,
                                6
                              ],
                              2
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);


/* encours Not a pure module */

/* No side effect */

function make$1(n, c) {
  var s = caml_create_string(n);
  caml_fill_string(s, 0, n, c);
  return s;
}


/* No side effect */

function make(n, c) {
  return bytes_to_string(make$1(n, c));
}

function concat$2(sep, l) {
  if (l) {
    var hd$$1 = l[0];
    var num = [0];
    var len = [0];
    iter((function (s) {
            num[0] = num[0] + 1 | 0;
            len[0] = len[0] + s.length | 0;
            return /* () */0;
          }), l);
    var r = caml_create_string(len[0] + imul(sep.length, num[0] - 1 | 0) | 0);
    caml_blit_string(hd$$1, 0, r, 0, hd$$1.length);
    var pos = [hd$$1.length];
    iter((function (s) {
            caml_blit_string(sep, 0, r, pos[0], sep.length);
            pos[0] = pos[0] + sep.length | 0;
            caml_blit_string(s, 0, r, pos[0], s.length);
            pos[0] = pos[0] + s.length | 0;
            return /* () */0;
          }), l[1]);
    return bytes_to_string(r);
  } else {
    return "";
  }
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function polyfills() {
  ((
  // remove polyfill
  (function() {
    if (!('remove' in Element.prototype)) {
      Element.prototype.remove = function() {
        if (this.parentNode) {
          this.parentNode.removeChild(this);
        }
      };
    }
  }())
  ));
  ((
  // requestAnimationFrame polyfill
  (function() {
      var lastTime = 0;
      var vendors = ['ms', 'moz', 'webkit', 'o'];
      for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
          window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
          window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame']
                                     || window[vendors[x]+'CancelRequestAnimationFrame'];
      }

      if (!window.requestAnimationFrame)
          window.requestAnimationFrame = function(callback, element) {
              var currTime = new Date().getTime();
              var timeToCall = Math.max(0, 16 - (currTime - lastTime));
              var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                timeToCall);
              lastTime = currTime + timeToCall;
              return id;
          };

      if (!window.cancelAnimationFrame)
          window.cancelAnimationFrame = function(id) {
              clearTimeout(id);
          };
  }())
  ));
  return /* () */0;
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function setStyle(n, key, value) {
  n.style[key] = value;
  return /* () */0;
}

function setStyleProperty(n, $staropt$star, key, value) {
  var priority = $staropt$star ? $staropt$star[0] : /* false */0;
  var style = n.style;
  var match = style.setProperty;
  if (match !== undefined) {
    return style.setProperty(key, value, priority ? "important" : null);
  } else {
    return setStyle(n, key, value);
  }
}

function insertBefore(n, child, refNode) {
  return n.insertBefore(child, refNode);
}

function setAttributeNsOptional(n, namespace, key, value) {
  if (namespace === "") {
    return n.setAttribute(key, value);
  } else {
    return n.setAttributeNS(namespace, key, value);
  }
}

function removeAttributeNsOptional(n, namespace, key) {
  if (namespace === "") {
    return n.removeAttribute(key);
  } else {
    return n.removeAttributeNS(namespace, key);
  }
}

function addEventListener(n, typ, listener, options) {
  return n.addEventListener(typ, listener, options);
}

function removeEventListener(n, typ, listener, options) {
  return n.removeEventListener(typ, listener, options);
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function createElementNsOptional(namespace, tagName) {
  if (namespace === "") {
    return document.createElement(tagName);
  } else {
    return document.createElementNS(namespace, tagName);
  }
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
var noNode = /* CommentNode */__(0, [""]);

function fullnode(namespace, tagName, key, unique, props, vdoms) {
  return /* Node */__(2, [
            namespace,
            tagName,
            key,
            unique,
            props,
            vdoms
          ]);
}

function onMsg(name, msg) {
  return /* Event */__(3, [
            name,
            /* EventHandlerMsg */__(1, [msg]),
            [/* None */0]
          ]);
}

function style(key, value) {
  return /* Style */__(4, [/* :: */[
              /* tuple */[
                key,
                value
              ],
              /* [] */0
            ]]);
}

function renderToHtmlString(_param) {
  while(true) {
    var param = _param;
    switch (param.tag | 0) {
      case 0 : 
          return "<!-- " + (param[0] + " -->");
      case 1 : 
          return param[0];
      case 2 : 
          var tagName = param[1];
          var namespace = param[0];
          return concat$2("", /* :: */[
                      "<",
                      /* :: */[
                        namespace,
                        /* :: */[
                          namespace === "" ? "" : ":",
                          /* :: */[
                            tagName,
                            /* :: */[
                              concat$2("", map((function (p) {
                                          var param = p;
                                          if (typeof param === "number") {
                                            return "";
                                          } else {
                                            switch (param.tag | 0) {
                                              case 0 : 
                                                  return concat$2("", /* :: */[
                                                              " ",
                                                              /* :: */[
                                                                param[0],
                                                                /* :: */[
                                                                  "=\"",
                                                                  /* :: */[
                                                                    param[1],
                                                                    /* :: */[
                                                                      "\"",
                                                                      /* [] */0
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]);
                                              case 1 : 
                                                  return concat$2("", /* :: */[
                                                              " ",
                                                              /* :: */[
                                                                param[1],
                                                                /* :: */[
                                                                  "=\"",
                                                                  /* :: */[
                                                                    param[2],
                                                                    /* :: */[
                                                                      "\"",
                                                                      /* [] */0
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]);
                                              case 2 : 
                                                  return concat$2("", /* :: */[
                                                              " data-",
                                                              /* :: */[
                                                                param[0],
                                                                /* :: */[
                                                                  "=\"",
                                                                  /* :: */[
                                                                    param[1],
                                                                    /* :: */[
                                                                      "\"",
                                                                      /* [] */0
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]);
                                              case 3 : 
                                                  return "";
                                              case 4 : 
                                                  return concat$2("", /* :: */[
                                                              " style=\"",
                                                              /* :: */[
                                                                concat$2(";", map((function (param) {
                                                                            return concat$2("", /* :: */[
                                                                                        param[0],
                                                                                        /* :: */[
                                                                                          ":",
                                                                                          /* :: */[
                                                                                            param[1],
                                                                                            /* :: */[
                                                                                              ";",
                                                                                              /* [] */0
                                                                                            ]
                                                                                          ]
                                                                                        ]
                                                                                      ]);
                                                                          }), param[0])),
                                                                /* :: */[
                                                                  "\"",
                                                                  /* [] */0
                                                                ]
                                                              ]
                                                            ]);
                                              
                                            }
                                          }
                                        }), param[4])),
                              /* :: */[
                                ">",
                                /* :: */[
                                  concat$2("", map(renderToHtmlString, param[5])),
                                  /* :: */[
                                    "</",
                                    /* :: */[
                                      tagName,
                                      /* :: */[
                                        ">",
                                        /* [] */0
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]);
      case 3 : 
          _param = _1(param[1], /* () */0);
          continue ;
          case 4 : 
          _param = param[1];
          continue ;
          
    }
  }
}

function eventHandler(callbacks, cb) {
  return (function (ev) {
      var match = _1(cb[0], ev);
      if (match) {
        return _1(callbacks[0][/* enqueue */0], match[0]);
      } else {
        return /* () */0;
      }
    });
}

function eventHandler_GetCB(param) {
  if (param.tag) {
    var msg = param[0];
    return (function () {
        return /* Some */[msg];
      });
  } else {
    return param[1];
  }
}

function compareEventHandlerTypes(left, param) {
  if (param.tag) {
    if (!left.tag || !caml_equal(param[0], left[0])) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  } else if (!left.tag && param[0] === left[0]) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function eventHandler_Register(callbacks, elem, name, handlerType) {
  var cb = [eventHandler_GetCB(handlerType)];
  var handler = eventHandler(callbacks, cb);
  addEventListener(elem, name, handler, /* false */0);
  return /* Some */[/* record */[
            /* handler */handler,
            /* cb */cb
          ]];
}

function eventHandler_Unregister(elem, name, param) {
  if (param) {
    removeEventListener(elem, name, param[0][/* handler */0], /* false */0);
    return /* None */0;
  } else {
    return /* None */0;
  }
}

function eventHandler_Mutate(callbacks, elem, oldName, newName, oldHandlerType, newHandlerType, oldCache, newCache) {
  var match = oldCache[0];
  if (match) {
    if (oldName === newName) {
      newCache[0] = oldCache[0];
      if (compareEventHandlerTypes(oldHandlerType, newHandlerType)) {
        return /* () */0;
      } else {
        var cb = eventHandler_GetCB(newHandlerType);
        match[0][/* cb */1][0] = cb;
        return /* () */0;
      }
    } else {
      oldCache[0] = eventHandler_Unregister(elem, oldName, oldCache[0]);
      newCache[0] = eventHandler_Register(callbacks, elem, newName, newHandlerType);
      return /* () */0;
    }
  } else {
    newCache[0] = eventHandler_Register(callbacks, elem, newName, newHandlerType);
    return /* () */0;
  }
}

function patchVNodesOnElems_PropertiesApply_Add(callbacks, elem, _, param) {
  if (typeof param === "number") {
    return /* () */0;
  } else {
    switch (param.tag | 0) {
      case 0 : 
          elem[param[0]] = param[1];
          return /* () */0;
      case 1 : 
          return setAttributeNsOptional(elem, param[0], param[1], param[2]);
      case 2 : 
          console.log(/* tuple */[
                "TODO:  Add Data Unhandled",
                param[0],
                param[1]
              ]);
          throw [
                failure,
                "TODO:  Add Data Unhandled"
              ];
      case 3 : 
          param[2][0] = eventHandler_Register(callbacks, elem, param[0], param[1]);
          return /* () */0;
      case 4 : 
          return fold_left((function (_, param) {
                        return setStyleProperty(elem, /* None */0, param[0], param[1]);
                      }), /* () */0, param[0]);
      
    }
  }
}

function patchVNodesOnElems_PropertiesApply_Remove(_, elem, _$1, param) {
  if (typeof param === "number") {
    return /* () */0;
  } else {
    switch (param.tag | 0) {
      case 0 : 
          elem[param[0]] = undefined;
          return /* () */0;
      case 1 : 
          return removeAttributeNsOptional(elem, param[0], param[1]);
      case 2 : 
          console.log(/* tuple */[
                "TODO:  Remove Data Unhandled",
                param[0],
                param[1]
              ]);
          throw [
                failure,
                "TODO:  Remove Data Unhandled"
              ];
      case 3 : 
          var cache = param[2];
          cache[0] = eventHandler_Unregister(elem, param[0], cache[0]);
          return /* () */0;
      case 4 : 
          return fold_left((function (_, param) {
                        return setStyleProperty(elem, /* None */0, param[0], null);
                      }), /* () */0, param[0]);
      
    }
  }
}

function patchVNodesOnElems_PropertiesApply_RemoveAdd(callbacks, elem, idx, oldProp, newProp) {
  patchVNodesOnElems_PropertiesApply_Remove(callbacks, elem, idx, oldProp);
  patchVNodesOnElems_PropertiesApply_Add(callbacks, elem, idx, newProp);
  return /* () */0;
}

function patchVNodesOnElems_PropertiesApply_Mutate(_, elem, _$1, oldProp, _newProp) {
  if (typeof _newProp === "number") {
    throw [
          failure,
          "This should never be called as all entries through NoProp are gated."
        ];
  } else {
    switch (_newProp.tag | 0) {
      case 0 : 
          elem[_newProp[0]] = _newProp[1];
          return /* () */0;
      case 1 : 
          return setAttributeNsOptional(elem, _newProp[0], _newProp[1], _newProp[2]);
      case 2 : 
          console.log(/* tuple */[
                "TODO:  Mutate Data Unhandled",
                _newProp[0],
                _newProp[1]
              ]);
          throw [
                failure,
                "TODO:  Mutate Data Unhandled"
              ];
      case 3 : 
          throw [
                failure,
                "This will never be called because it is gated"
              ];
      case 4 : 
          if (typeof oldProp === "number") {
            throw [
                  failure,
                  "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"
                ];
          } else if (oldProp.tag === 4) {
            return fold_left2((function (_, param, param$1) {
                          var nv = param$1[1];
                          var nk = param$1[0];
                          var ok = param[0];
                          if (ok === nk) {
                            if (param[1] === nv) {
                              return /* () */0;
                            } else {
                              return setStyleProperty(elem, /* None */0, nk, nv);
                            }
                          } else {
                            setStyleProperty(elem, /* None */0, ok, null);
                            return setStyleProperty(elem, /* None */0, nk, nv);
                          }
                        }), /* () */0, oldProp[0], _newProp[0]);
          } else {
            throw [
                  failure,
                  "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"
                ];
          }
          break;
      
    }
  }
}

function patchVNodesOnElems_PropertiesApply(callbacks, elem, _idx, _oldProperties, _newProperties) {
  while(true) {
    var newProperties = _newProperties;
    var oldProperties = _oldProperties;
    var idx = _idx;
    if (oldProperties) {
      var _oldProp = oldProperties[0];
      var exit = 0;
      if (newProperties) {
        if (typeof _oldProp === "number") {
          if (typeof newProperties[0] === "number") {
            _newProperties = newProperties[1];
            _oldProperties = oldProperties[1];
            _idx = idx + 1 | 0;
            continue ;
            
          } else {
            exit = 1;
          }
        } else {
          switch (_oldProp.tag | 0) {
            case 0 : 
                var newProp = newProperties[0];
                if (typeof newProp === "number") {
                  exit = 1;
                } else if (newProp.tag) {
                  exit = 1;
                } else {
                  if (!(_oldProp[0] === newProp[0] && _oldProp[1] === newProp[1])) {
                    patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp);
                  }
                  _newProperties = newProperties[1];
                  _oldProperties = oldProperties[1];
                  _idx = idx + 1 | 0;
                  continue ;
                  
                }
                break;
            case 1 : 
                var newProp$1 = newProperties[0];
                if (typeof newProp$1 === "number") {
                  exit = 1;
                } else if (newProp$1.tag === 1) {
                  if (!(_oldProp[0] === newProp$1[0] && _oldProp[1] === newProp$1[1] && _oldProp[2] === newProp$1[2])) {
                    patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp$1);
                  }
                  _newProperties = newProperties[1];
                  _oldProperties = oldProperties[1];
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else {
                  exit = 1;
                }
                break;
            case 2 : 
                var newProp$2 = newProperties[0];
                if (typeof newProp$2 === "number") {
                  exit = 1;
                } else if (newProp$2.tag === 2) {
                  if (!(_oldProp[0] === newProp$2[0] && _oldProp[1] === newProp$2[1])) {
                    patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp$2);
                  }
                  _newProperties = newProperties[1];
                  _oldProperties = oldProperties[1];
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else {
                  exit = 1;
                }
                break;
            case 3 : 
                var _newProp = newProperties[0];
                if (typeof _newProp === "number") {
                  exit = 1;
                } else if (_newProp.tag === 3) {
                  eventHandler_Mutate(callbacks, elem, _oldProp[0], _newProp[0], _oldProp[1], _newProp[1], _oldProp[2], _newProp[2]);
                  _newProperties = newProperties[1];
                  _oldProperties = oldProperties[1];
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else {
                  exit = 1;
                }
                break;
            case 4 : 
                var newProp$3 = newProperties[0];
                if (typeof newProp$3 === "number") {
                  exit = 1;
                } else if (newProp$3.tag === 4) {
                  if (!caml_equal(_oldProp[0], newProp$3[0])) {
                    patchVNodesOnElems_PropertiesApply_Mutate(callbacks, elem, idx, _oldProp, newProp$3);
                  }
                  _newProperties = newProperties[1];
                  _oldProperties = oldProperties[1];
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else {
                  exit = 1;
                }
                break;
            
          }
        }
      } else {
        return /* false */0;
      }
      if (exit === 1) {
        patchVNodesOnElems_PropertiesApply_RemoveAdd(callbacks, elem, idx, _oldProp, newProperties[0]);
        _newProperties = newProperties[1];
        _oldProperties = oldProperties[1];
        _idx = idx + 1 | 0;
        continue ;
        
      }
      
    } else if (newProperties) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  }
}

function patchVNodesOnElems_Properties(callbacks, elem, oldProperties, newProperties) {
  return patchVNodesOnElems_PropertiesApply(callbacks, elem, 0, oldProperties, newProperties);
}

function patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, param) {
  if (param.tag === 2) {
    var newProperties = param[4];
    var oldChild = caml_array_get(elems, idx);
    var newChild = createElementNsOptional(param[0], param[1]);
    var match = patchVNodesOnElems_Properties(callbacks, newChild, map((function () {
                return /* NoProp */0;
              }), newProperties), newProperties);
    if (match !== 0) {
      var childChildren = newChild.childNodes;
      patchVNodesOnElems(callbacks, newChild, childChildren, 0, /* [] */0, param[5]);
      insertBefore(elem, newChild, oldChild);
      elem.removeChild(oldChild);
      return /* () */0;
    } else {
      throw [
            match_failure,
            [
              "/home/sylvain/Documents/devel/gyges/node_modules/bucklescript-tea/src/vdom.ml",
              319,
              30
            ]
          ];
    }
  } else {
    throw [
          failure,
          "Node replacement should never be passed anything but a node itself"
        ];
  }
}

function patchVNodesOnElems_CreateElement(_callbacks, _param) {
  while(true) {
    var param = _param;
    var callbacks = _callbacks;
    switch (param.tag | 0) {
      case 0 : 
          var text = param[0];
          return document.createComment(text);
      case 1 : 
          var text$1 = param[0];
          return document.createTextNode(text$1);
      case 2 : 
          var newProperties = param[4];
          var newChild = createElementNsOptional(param[0], param[1]);
          var match = patchVNodesOnElems_Properties(callbacks, newChild, map((function () {
                      return /* NoProp */0;
                    }), newProperties), newProperties);
          if (match !== 0) {
            var childChildren = newChild.childNodes;
            patchVNodesOnElems(callbacks, newChild, childChildren, 0, /* [] */0, param[5]);
            return newChild;
          } else {
            throw [
                  match_failure,
                  [
                    "/home/sylvain/Documents/devel/gyges/node_modules/bucklescript-tea/src/vdom.ml",
                    333,
                    30
                  ]
                ];
          }
          break;
      case 3 : 
          var vdom = _1(param[1], /* () */0);
          param[2][0] = vdom;
          _param = vdom;
          continue ;
          case 4 : 
          _param = param[1];
          _callbacks = _1(param[0], callbacks);
          continue ;
          
    }
  }
}

function patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode) {
  if (oldNode.tag === 2) {
    if (newNode.tag === 2) {
      if (oldNode[3] !== newNode[3] || oldNode[1] !== newNode[1]) {
        return patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode);
      } else {
        var child = caml_array_get(elems, idx);
        var childChildren = child.childNodes;
        if (!patchVNodesOnElems_Properties(callbacks, child, oldNode[4], newNode[4])) {
          console.log("VDom:  Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure.  This is a massive inefficiency until this issue is resolved.");
          patchVNodesOnElems_ReplaceNode(callbacks, elem, elems, idx, newNode);
        }
        return patchVNodesOnElems(callbacks, child, childChildren, 0, oldNode[5], newNode[5]);
      }
    } else {
      throw [
            failure,
            "Non-node passed to patchVNodesOnElems_MutateNode"
          ];
    }
  } else {
    throw [
          failure,
          "Non-node passed to patchVNodesOnElems_MutateNode"
        ];
  }
}

function patchVNodesOnElems(callbacks, elem, elems, _idx, _oldVNodes, _newVNodes) {
  while(true) {
    var newVNodes = _newVNodes;
    var oldVNodes = _oldVNodes;
    var idx = _idx;
    if (oldVNodes) {
      var oldNode = oldVNodes[0];
      var exit = 0;
      switch (oldNode.tag | 0) {
        case 0 : 
            if (newVNodes) {
              var match = newVNodes[0];
              if (match.tag) {
                exit = 1;
              } else if (oldNode[0] === match[0]) {
                _newVNodes = newVNodes[1];
                _oldVNodes = oldVNodes[1];
                _idx = idx + 1 | 0;
                continue ;
                
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
            }
            break;
        case 1 : 
            if (newVNodes) {
              var match$1 = newVNodes[0];
              if (match$1.tag === 1) {
                var newText = match$1[0];
                if (oldNode[0] !== newText) {
                  var child = caml_array_get(elems, idx);
                  child.nodeValue = newText;
                }
                _newVNodes = newVNodes[1];
                _oldVNodes = oldVNodes[1];
                _idx = idx + 1 | 0;
                continue ;
                
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
            }
            break;
        case 2 : 
            if (newVNodes) {
              var newNode = newVNodes[0];
              if (newNode.tag === 2) {
                var newRest = newVNodes[1];
                var newKey = newNode[2];
                var newTagName = newNode[1];
                var newNamespace = newNode[0];
                var oldRest = oldVNodes[1];
                var oldKey = oldNode[2];
                var oldTagName = oldNode[1];
                var oldNamespace = oldNode[0];
                if (oldKey === newKey && oldKey !== "") {
                  _newVNodes = newRest;
                  _oldVNodes = oldRest;
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else if (oldKey === "" || newKey === "") {
                  patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode);
                  _newVNodes = newRest;
                  _oldVNodes = oldRest;
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else {
                  var exit$1 = 0;
                  var exit$2 = 0;
                  if (oldRest) {
                    var match$2 = oldRest[0];
                    if (match$2.tag === 2) {
                      var olderRest = oldRest[1];
                      var olderKey = match$2[2];
                      var olderTagName = match$2[1];
                      var olderNamespace = match$2[0];
                      var exit$3 = 0;
                      if (newRest) {
                        var match$3 = newRest[0];
                        if (match$3.tag === 2) {
                          if (olderNamespace === newNamespace && olderTagName === newTagName && olderKey === newKey && oldNamespace === match$3[0] && oldTagName === match$3[1] && oldKey === match$3[2]) {
                            var firstChild$$1 = caml_array_get(elems, idx);
                            var secondChild = caml_array_get(elems, idx + 1 | 0);
                            elem.removeChild(secondChild);
                            insertBefore(elem, secondChild, firstChild$$1);
                            _newVNodes = newRest[1];
                            _oldVNodes = olderRest;
                            _idx = idx + 2 | 0;
                            continue ;
                            
                          } else {
                            exit$3 = 4;
                          }
                        } else {
                          exit$3 = 4;
                        }
                      } else {
                        exit$3 = 4;
                      }
                      if (exit$3 === 4) {
                        if (olderNamespace === newNamespace && olderTagName === newTagName && olderKey === newKey) {
                          var oldChild = caml_array_get(elems, idx);
                          elem.removeChild(oldChild);
                          _newVNodes = newRest;
                          _oldVNodes = olderRest;
                          _idx = idx + 1 | 0;
                          continue ;
                          
                        } else {
                          exit$2 = 3;
                        }
                      }
                      
                    } else {
                      exit$2 = 3;
                    }
                  } else {
                    exit$2 = 3;
                  }
                  if (exit$2 === 3) {
                    if (newRest) {
                      var match$4 = newRest[0];
                      if (match$4.tag === 2) {
                        if (oldNamespace === match$4[0] && oldTagName === match$4[1] && oldKey === match$4[2]) {
                          var oldChild$1 = caml_array_get(elems, idx);
                          var newChild = patchVNodesOnElems_CreateElement(callbacks, newNode);
                          insertBefore(elem, newChild, oldChild$1);
                          _newVNodes = newRest;
                          _idx = idx + 1 | 0;
                          continue ;
                          
                        } else {
                          exit$1 = 2;
                        }
                      } else {
                        exit$1 = 2;
                      }
                    } else {
                      exit$1 = 2;
                    }
                  }
                  if (exit$1 === 2) {
                    patchVNodesOnElems_MutateNode(callbacks, elem, elems, idx, oldNode, newNode);
                    _newVNodes = newRest;
                    _oldVNodes = oldRest;
                    _idx = idx + 1 | 0;
                    continue ;
                    
                  }
                  
                }
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
            }
            break;
        case 3 : 
            if (newVNodes) {
              var match$5 = newVNodes[0];
              if (match$5.tag === 3) {
                var newRest$1 = newVNodes[1];
                var newCache = match$5[2];
                var newGen = match$5[1];
                var newKey$1 = match$5[0];
                var oldRest$1 = oldVNodes[1];
                var oldCache = oldNode[2];
                var oldKey$1 = oldNode[0];
                if (oldKey$1 === newKey$1) {
                  newCache[0] = oldCache[0];
                  _newVNodes = newRest$1;
                  _oldVNodes = oldRest$1;
                  _idx = idx + 1 | 0;
                  continue ;
                  
                } else {
                  var exit$4 = 0;
                  var exit$5 = 0;
                  if (oldRest$1) {
                    var match$6 = oldRest$1[0];
                    if (match$6.tag === 3) {
                      var olderRest$1 = oldRest$1[1];
                      var olderKey$1 = match$6[0];
                      var exit$6 = 0;
                      if (newRest$1) {
                        var match$7 = newRest$1[0];
                        if (match$7.tag === 3) {
                          if (olderKey$1 === newKey$1 && oldKey$1 === match$7[0]) {
                            var firstChild$1 = caml_array_get(elems, idx);
                            var secondChild$1 = caml_array_get(elems, idx + 1 | 0);
                            elem.removeChild(secondChild$1);
                            insertBefore(elem, secondChild$1, firstChild$1);
                            _newVNodes = newRest$1[1];
                            _oldVNodes = olderRest$1;
                            _idx = idx + 2 | 0;
                            continue ;
                            
                          } else {
                            exit$6 = 4;
                          }
                        } else {
                          exit$6 = 4;
                        }
                      } else {
                        exit$6 = 4;
                      }
                      if (exit$6 === 4) {
                        if (olderKey$1 === newKey$1) {
                          var oldChild$2 = caml_array_get(elems, idx);
                          elem.removeChild(oldChild$2);
                          var oldVdom = match$6[2][0];
                          newCache[0] = oldVdom;
                          _newVNodes = newRest$1;
                          _oldVNodes = olderRest$1;
                          _idx = idx + 1 | 0;
                          continue ;
                          
                        } else {
                          exit$5 = 3;
                        }
                      }
                      
                    } else {
                      exit$5 = 3;
                    }
                  } else {
                    exit$5 = 3;
                  }
                  if (exit$5 === 3) {
                    if (newRest$1) {
                      var match$8 = newRest$1[0];
                      if (match$8.tag === 3) {
                        if (match$8[0] === oldKey$1) {
                          var oldChild$3 = caml_array_get(elems, idx);
                          var newVdom = _1(newGen, /* () */0);
                          newCache[0] = newVdom;
                          var newChild$1 = patchVNodesOnElems_CreateElement(callbacks, newVdom);
                          insertBefore(elem, newChild$1, oldChild$3);
                          _newVNodes = newRest$1;
                          _idx = idx + 1 | 0;
                          continue ;
                          
                        } else {
                          exit$4 = 2;
                        }
                      } else {
                        exit$4 = 2;
                      }
                    } else {
                      exit$4 = 2;
                    }
                  }
                  if (exit$4 === 2) {
                    var oldVdom$1 = oldCache[0];
                    var newVdom$1 = _1(newGen, /* () */0);
                    newCache[0] = newVdom$1;
                    _newVNodes = /* :: */[
                      newVdom$1,
                      newRest$1
                    ];
                    _oldVNodes = /* :: */[
                      oldVdom$1,
                      oldRest$1
                    ];
                    continue ;
                    
                  }
                  
                }
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
            }
            break;
        case 4 : 
            _oldVNodes = /* :: */[
              oldNode[1],
              oldVNodes[1]
            ];
            continue ;
            
      }
      if (exit === 1) {
        var oldRest$2 = oldVNodes[1];
        if (newVNodes) {
          var newNode$1 = newVNodes[0];
          if (newNode$1.tag === 4) {
            patchVNodesOnElems(_1(newNode$1[0], callbacks), elem, elems, idx, /* :: */[
                  oldNode,
                  /* [] */0
                ], /* :: */[
                  newNode$1[1],
                  /* [] */0
                ]);
            _newVNodes = newVNodes[1];
            _oldVNodes = oldRest$2;
            _idx = idx + 1 | 0;
            continue ;
            
          } else {
            var oldChild$4 = caml_array_get(elems, idx);
            var newChild$2 = patchVNodesOnElems_CreateElement(callbacks, newNode$1);
            insertBefore(elem, newChild$2, oldChild$4);
            elem.removeChild(oldChild$4);
            _newVNodes = newVNodes[1];
            _oldVNodes = oldRest$2;
            _idx = idx + 1 | 0;
            continue ;
            
          }
        } else {
          var child$1 = caml_array_get(elems, idx);
          elem.removeChild(child$1);
          _newVNodes = /* [] */0;
          _oldVNodes = oldRest$2;
          continue ;
          
        }
      }
      
    } else if (newVNodes) {
      var newChild$3 = patchVNodesOnElems_CreateElement(callbacks, newVNodes[0]);
      elem.appendChild(newChild$3);
      _newVNodes = newVNodes[1];
      _oldVNodes = /* [] */0;
      _idx = idx + 1 | 0;
      continue ;
      
    } else {
      return /* () */0;
    }
  }
}

function patchVNodesIntoElement(callbacks, elem, oldVNodes, newVNodes) {
  var elems = elem.childNodes;
  patchVNodesOnElems(callbacks, elem, elems, 0, oldVNodes, newVNodes);
  return newVNodes;
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function run(callbacks, param) {
  if (typeof param === "number") {
    return /* () */0;
  } else {
    switch (param.tag | 0) {
      case 1 : 
          return fold_left((function (_, cmd) {
                        return run(callbacks, cmd);
                      }), /* () */0, param[0]);
      case 0 : 
      case 2 : 
          return _1(param[0], callbacks);
      
    }
  }
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function run$1(oldCallbacks, newCallbacks, oldSub, newSub) {
  var enable = function (_callbacks, _param) {
    while(true) {
      var param = _param;
      var callbacks = _callbacks;
      if (typeof param === "number") {
        return /* () */0;
      } else {
        switch (param.tag | 0) {
          case 0 : 
              var subs = param[0];
              if (subs) {
                return iter((function(callbacks){
                          return function (param) {
                            return enable(callbacks, param);
                          }
                          }(callbacks)), subs);
              } else {
                return /* () */0;
              }
          case 1 : 
              param[2][0] = /* Some */[_1(param[1], callbacks)];
              return /* () */0;
          case 2 : 
              var subCallbacks = _1(param[0], callbacks);
              _param = param[1];
              _callbacks = subCallbacks;
              continue ;
              
        }
      }
    }
  };
  var disable = function (_callbacks, _param) {
    while(true) {
      var param = _param;
      var callbacks = _callbacks;
      if (typeof param === "number") {
        return /* () */0;
      } else {
        switch (param.tag | 0) {
          case 0 : 
              var subs = param[0];
              if (subs) {
                return iter((function(callbacks){
                          return function (param) {
                            return disable(callbacks, param);
                          }
                          }(callbacks)), subs);
              } else {
                return /* () */0;
              }
          case 1 : 
              var diCB = param[2];
              var match = diCB[0];
              if (match) {
                diCB[0] = /* None */0;
                return _1(match[0], /* () */0);
              } else {
                return /* () */0;
              }
          case 2 : 
              var subCallbacks = _1(param[0], callbacks);
              _param = param[1];
              _callbacks = subCallbacks;
              continue ;
              
        }
      }
    }
  };
  var exit = 0;
  if (typeof oldSub === "number") {
    if (typeof newSub === "number") {
      return newSub;
    } else {
      exit = 1;
    }
  } else {
    switch (oldSub.tag | 0) {
      case 0 : 
          if (typeof newSub === "number") {
            exit = 1;
          } else if (newSub.tag) {
            exit = 1;
          } else {
            var aux = function (_oldList, _newList) {
              while(true) {
                var newList = _newList;
                var oldList = _oldList;
                if (oldList) {
                  var oldRest = oldList[1];
                  var oldSubSub = oldList[0];
                  if (newList) {
                    run$1(oldCallbacks, newCallbacks, oldSubSub, newList[0]);
                    _newList = newList[1];
                    _oldList = oldRest;
                    continue ;
                    
                  } else {
                    disable(oldCallbacks, oldSubSub);
                    _newList = /* [] */0;
                    _oldList = oldRest;
                    continue ;
                    
                  }
                } else if (newList) {
                  enable(newCallbacks, newList[0]);
                  _newList = newList[1];
                  _oldList = /* [] */0;
                  continue ;
                  
                } else {
                  return /* () */0;
                }
              }
            };
            aux(oldSub[0], newSub[0]);
            return newSub;
          }
          break;
      case 1 : 
          if (typeof newSub === "number") {
            exit = 1;
          } else if (newSub.tag === 1) {
            if (oldSub[0] === newSub[0]) {
              newSub[2][0] = oldSub[2][0];
              return newSub;
            } else {
              exit = 1;
            }
          } else {
            exit = 1;
          }
          break;
      case 2 : 
          if (typeof newSub === "number") {
            exit = 1;
          } else if (newSub.tag === 2) {
            var olderCallbacks = _1(oldSub[0], oldCallbacks);
            var newerCallbacks = _1(newSub[0], newCallbacks);
            run$1(olderCallbacks, newerCallbacks, oldSub[1], newSub[1]);
            return newSub;
          } else {
            exit = 1;
          }
          break;
      
    }
  }
  if (exit === 1) {
    disable(oldCallbacks, oldSub);
    enable(newCallbacks, newSub);
    return newSub;
  }
  
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function programStateWrapper(initModel, pump, shutdown) {
  var model = [initModel];
  var callbacks = [/* record */[/* enqueue */(function () {
          console.log("INVALID enqueue CALL!");
          return /* () */0;
        })]];
  var pumperInterface = _1(pump, callbacks);
  var pending = [/* None */0];
  var handler = function (msg$$1) {
    var match = pending[0];
    if (match) {
      pending[0] = /* Some */[/* :: */[
          msg$$1,
          match[0]
        ]];
      return /* () */0;
    } else {
      pending[0] = /* Some */[/* [] */0];
      var newModel = _2(pumperInterface[/* handleMsg */2], model[0], msg$$1);
      model[0] = newModel;
      var match$1 = pending[0];
      if (match$1) {
        var msgs = match$1[0];
        if (msgs) {
          pending[0] = /* None */0;
          return iter(handler, rev(msgs));
        } else {
          pending[0] = /* None */0;
          return /* () */0;
        }
      } else {
        throw [
              failure,
              "INVALID message queue state, should never be None during message processing!"
            ];
      }
    }
  };
  var finalizedCBs = /* record */[/* enqueue */handler];
  callbacks[0] = finalizedCBs;
  var pi_requestShutdown = function () {
    callbacks[0] = /* record */[/* enqueue */(function () {
          console.log("INVALID message enqueued when shut down");
          return /* () */0;
        })];
    var cmd = _1(shutdown, model[0]);
    _1(pumperInterface[/* shutdown */3], cmd);
    return /* () */0;
  };
  var render_string = function () {
    return _1(pumperInterface[/* render_string */1], model[0]);
  };
  _1(pumperInterface[/* startup */0], /* () */0);
  return {
          pushMsg: handler,
          shutdown: pi_requestShutdown,
          getHtmlString: render_string
        };
}

function programLoop(update, view, subscriptions, initModel, initCmd, param) {
  if (param) {
    var parentNode = param[0];
    return (function (callbacks) {
        var priorRenderedVdom = [/* [] */0];
        var latestModel = [initModel];
        var nextFrameID = [/* None */0];
        var doRender = function () {
          var match = nextFrameID[0];
          if (match) {
            var newVdom_000 = _1(view, latestModel[0]);
            var newVdom = /* :: */[
              newVdom_000,
              /* [] */0
            ];
            var justRenderedVdom = patchVNodesIntoElement(callbacks, parentNode, priorRenderedVdom[0], newVdom);
            priorRenderedVdom[0] = justRenderedVdom;
            nextFrameID[0] = /* None */0;
            return /* () */0;
          } else {
            return /* () */0;
          }
        };
        var scheduleRender = function () {
          var match = nextFrameID[0];
          if (match) {
            return /* () */0;
          } else {
            nextFrameID[0] = /* Some */[-1];
            return doRender(16);
          }
        };
        var clearPnode = function () {
          while(parentNode.childNodes.length > 0) {
            var match = parentNode.firstChild;
            if (match !== null) {
              parentNode.removeChild(match);
            }
            
          }
          return /* () */0;
        };
        var oldSub = [/* NoSub */0];
        var handleSubscriptionChange = function (model) {
          var newSub = _1(subscriptions, model);
          oldSub[0] = run$1(callbacks, callbacks, oldSub[0], newSub);
          return /* () */0;
        };
        var handlerStartup = function () {
          clearPnode(/* () */0);
          run(callbacks, initCmd);
          handleSubscriptionChange(latestModel[0]);
          nextFrameID[0] = /* Some */[-1];
          doRender(16);
          return /* () */0;
        };
        var render_string = function (model) {
          return renderToHtmlString(_1(view, model));
        };
        var handler = function (model, msg$$1) {
          var match = _2(update, model, msg$$1);
          var newModel = match[0];
          latestModel[0] = newModel;
          run(callbacks, match[1]);
          scheduleRender(/* () */0);
          handleSubscriptionChange(newModel);
          return newModel;
        };
        var handlerShutdown = function (cmd) {
          nextFrameID[0] = /* None */0;
          run(callbacks, cmd);
          oldSub[0] = run$1(callbacks, callbacks, oldSub[0], /* NoSub */0);
          priorRenderedVdom[0] = /* [] */0;
          clearPnode(/* () */0);
          return /* () */0;
        };
        return /* record */[
                /* startup */handlerStartup,
                /* render_string */render_string,
                /* handleMsg */handler,
                /* shutdown */handlerShutdown
              ];
      });
  } else {
    return (function (callbacks) {
        var oldSub = [/* NoSub */0];
        var handleSubscriptionChange = function (model) {
          var newSub = _1(subscriptions, model);
          oldSub[0] = run$1(callbacks, callbacks, oldSub[0], newSub);
          return /* () */0;
        };
        return /* record */[
                /* startup */(function () {
                    run(callbacks, initCmd);
                    handleSubscriptionChange(initModel);
                    return /* () */0;
                  }),
                /* render_string */(function (model) {
                    return renderToHtmlString(_1(view, model));
                  }),
                /* handleMsg */(function (model, msg$$1) {
                    var match = _2(update, model, msg$$1);
                    var newModel = match[0];
                    run(callbacks, match[1]);
                    handleSubscriptionChange(newModel);
                    return newModel;
                  }),
                /* shutdown */(function (cmd) {
                    run(callbacks, cmd);
                    oldSub[0] = run$1(callbacks, callbacks, oldSub[0], /* NoSub */0);
                    return /* () */0;
                  })
              ];
      });
  }
}

function program(param, pnode, flags) {
  polyfills(/* () */0);
  var match = _1(param[/* init */0], flags);
  var initModel = match[0];
  var opnode = (pnode == null) ? /* None */0 : [pnode];
  var pumpInterface = programLoop(param[/* update */1], param[/* view */2], param[/* subscriptions */3], initModel, match[1], opnode);
  return programStateWrapper(initModel, pumpInterface, param[/* shutdown */4]);
}

function standardProgram(param, pnode, args) {
  return program(/* record */[
              /* init */param[/* init */0],
              /* update */param[/* update */1],
              /* view */param[/* view */2],
              /* subscriptions */param[/* subscriptions */3],
              /* shutdown */(function () {
                  return /* NoCmd */0;
                })
            ], pnode, args);
}

function beginnerProgram(param, pnode, _) {
  var update = param[/* update */1];
  var model = param[/* model */0];
  return standardProgram(/* record */[
              /* init */(function () {
                  return /* tuple */[
                          model,
                          /* NoCmd */0
                        ];
                }),
              /* update */(function (model, msg$$1) {
                  return /* tuple */[
                          _2(update, model, msg$$1),
                          /* NoCmd */0
                        ];
                }),
              /* view */param[/* view */2],
              /* subscriptions */(function () {
                  return /* NoSub */0;
                })
            ], pnode, /* () */0);
}


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function text$1(str) {
  return /* Text */__(1, [str]);
}

function div$2($staropt$star, $staropt$star$1, props, nodes) {
  var key = $staropt$star ? $staropt$star[0] : "";
  var unique = $staropt$star$1 ? $staropt$star$1[0] : "";
  return fullnode("", "div", key, unique, props, nodes);
}

function span($staropt$star, $staropt$star$1, props, nodes) {
  var key = $staropt$star ? $staropt$star[0] : "";
  var unique = $staropt$star$1 ? $staropt$star$1[0] : "";
  return fullnode("", "span", key, unique, props, nodes);
}

function p($staropt$star, $staropt$star$1, props, nodes) {
  var key = $staropt$star ? $staropt$star[0] : "";
  var unique = $staropt$star$1 ? $staropt$star$1[0] : "";
  return fullnode("", "p", key, unique, props, nodes);
}

function pre($staropt$star, $staropt$star$1, props, nodes) {
  var key = $staropt$star ? $staropt$star[0] : "";
  var unique = $staropt$star$1 ? $staropt$star$1[0] : "";
  return fullnode("", "pre", key, unique, props, nodes);
}

function h3($staropt$star, $staropt$star$1, props, nodes) {
  var key = $staropt$star ? $staropt$star[0] : "";
  var unique = $staropt$star$1 ? $staropt$star$1[0] : "";
  return fullnode("", "h3", key, unique, props, nodes);
}

function button($staropt$star, $staropt$star$1, props, nodes) {
  var key = $staropt$star ? $staropt$star[0] : "";
  var unique = $staropt$star$1 ? $staropt$star$1[0] : "";
  return fullnode("", "button", key, unique, props, nodes);
}

function class$prime(name) {
  return /* RawProp */__(0, [
            "className",
            name
          ]);
}

var style$2 = style;

function styles$1(s) {
  return /* Style */__(4, [s]);
}

function onClick(msg) {
  return onMsg("click", msg);
}

var noNode$1 = noNode;


/* No side effect */

// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
function choix(param_0) {
  return /* Choix */__(1, [param_0]);
}

var original = /* array */[
  "●",
  "○",
  "◎",
  "🞋"
];

var defaut = /* array */[
  "٠",
  "١",
  "٢",
  "٣"
];

var chiffre = /* array */[
  ".",
  "1",
  "2",
  "3"
];

function init_jeu() {
  return /* [] */0;
}

function ajout_cle(f, cle, valeur, x) {
  if (caml_equal(x, cle)) {
    return valeur;
  } else {
    return _1(f, x);
  }
}

function cle_ajout(cle, valeur, f) {
  return (function (param) {
      return ajout_cle(f, cle, valeur, param);
    });
}

function ajout(f, p$$1) {
  var v = p$$1[1];
  var k = p$$1[0];
  return (function (param) {
      return ajout_cle(f, k, v, param);
    });
}

var encours = map((function (param) {
        var match = param[0];
        return /* tuple */[
                /* Case */[
                  match[0],
                  match[1]
                ],
                /* Val */[param[1]]
              ];
      }), /* :: */[
      /* tuple */[
        /* tuple */[
          1,
          1
        ],
        3
      ],
      /* :: */[
        /* tuple */[
          /* tuple */[
            3,
            1
          ],
          1
        ],
        /* :: */[
          /* tuple */[
            /* tuple */[
              4,
              1
            ],
            3
          ],
          /* :: */[
            /* tuple */[
              /* tuple */[
                6,
                1
              ],
              1
            ],
            /* :: */[
              /* tuple */[
                /* tuple */[
                  4,
                  2
                ],
                2
              ],
              /* :: */[
                /* tuple */[
                  /* tuple */[
                    3,
                    3
                  ],
                  2
                ],
                /* :: */[
                  /* tuple */[
                    /* tuple */[
                      5,
                      3
                    ],
                    3
                  ],
                  /* :: */[
                    /* tuple */[
                      /* tuple */[
                        3,
                        4
                      ],
                      3
                    ],
                    /* :: */[
                      /* tuple */[
                        /* tuple */[
                          1,
                          6
                        ],
                        1
                      ],
                      /* :: */[
                        /* tuple */[
                          /* tuple */[
                            3,
                            6
                          ],
                          2
                        ],
                        /* :: */[
                          /* tuple */[
                            /* tuple */[
                              4,
                              6
                            ],
                            1
                          ],
                          /* :: */[
                            /* tuple */[
                              /* tuple */[
                                5,
                                6
                              ],
                              2
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

function pions(config, p$$1) {
  var parray = config.split("");
  if (p$$1) {
    return caml_array_get(parray, p$$1[0]);
  } else {
    return caml_array_get(parray, 0);
  }
}

function string_vers_array(str) {
  var n = str.length;
  var rep = caml_make_vect(n, "");
  for(var i$$1 = 0; i$$1 <= n; ++i$$1){
    caml_array_set(rep, i$$1, make(1, get(str, i$$1)));
  }
  return rep;
}

function init() {
  return /* record */[
          /* partie */encours,
          /* joueur : Sud */1,
          /* pions */original,
          /* desc : false */0,
          /* choixprem : None */0,
          /* choixdeux : None */0,
          /* message */""
        ];
}

function case_vers_string($$case) {
  if ($$case) {
    var $$case$1 = $$case[0];
    if (typeof $$case$1 === "number") {
      if ($$case$1 !== 0) {
        return "case Sud";
      } else {
        return "case Nord";
      }
    } else {
      return " case (" + (String($$case$1[0]) + ("," + (String($$case$1[1]) + ")")));
    }
  } else {
    return "aucune case sélectionnée";
  }
}

function update(model, param) {
  if (typeof param === "number") {
    if (param) {
      var x = model[/* pions */2];
      var nou = caml_equal(x, original) ? defaut : original;
      var newrecord = model.slice();
      newrecord[/* pions */2] = nou;
      return newrecord;
    } else {
      var newrecord$1 = model.slice();
      newrecord$1[/* desc */3] = 1 - model[/* desc */3];
      return newrecord$1;
    }
  } else if (param.tag) {
    var $$case = param[0];
    var match = model[/* choixprem */4];
    if (match) {
      var prem = match[0];
      if (caml_equal(/* Some */[$$case], model[/* choixprem */4])) {
        var newrecord$2 = model.slice();
        newrecord$2[/* choixprem */4] = /* None */0;
        return newrecord$2;
      } else if (exists((function (x) {
                return caml_equal(x, $$case);
              }), depl_possibles(model[/* partie */0], prem))) {
        if (typeof $$case === "number") {
          if ($$case !== 0) {
            return /* record */[
                    /* partie */init_jeu$1(/* () */0),
                    /* joueur */model[/* joueur */1],
                    /* pions */model[/* pions */2],
                    /* desc */model[/* desc */3],
                    /* choixprem : None */0,
                    /* choixdeux */model[/* choixdeux */5],
                    /* message */"Nord a gagné"
                  ];
          } else {
            return /* record */[
                    /* partie */init_jeu$1(/* () */0),
                    /* joueur */model[/* joueur */1],
                    /* pions */model[/* pions */2],
                    /* desc */model[/* desc */3],
                    /* choixprem : None */0,
                    /* choixdeux */model[/* choixdeux */5],
                    /* message */"Sud a gagné"
                  ];
          }
        } else {
          return /* record */[
                  /* partie */mise_a_jour(model[/* partie */0], /* tuple */[
                        prem,
                        $$case
                      ]),
                  /* joueur */model[/* joueur */1] === /* Sud */1 ? /* Nord */0 : /* Sud */1,
                  /* pions */model[/* pions */2],
                  /* desc */model[/* desc */3],
                  /* choixprem : None */0,
                  /* choixdeux : Some */[$$case],
                  /* message */"deuxieme choix: " + case_vers_string(/* Some */[$$case])
                ];
        }
      } else {
        var newrecord$3 = model.slice();
        newrecord$3[/* message */6] = "Case inacessible";
        return newrecord$3;
      }
    } else if (exists((function (x) {
              return caml_equal(x, $$case);
            }), departs_possibles(model[/* partie */0], model[/* joueur */1]))) {
      return /* record */[
              /* partie */model[/* partie */0],
              /* joueur */model[/* joueur */1],
              /* pions */model[/* pions */2],
              /* desc */model[/* desc */3],
              /* choixprem : Some */[$$case],
              /* choixdeux */model[/* choixdeux */5],
              /* message */"premier choix: " + case_vers_string(/* Some */[$$case])
            ];
    } else {
      var newrecord$4 = model.slice();
      newrecord$4[/* message */6] = "Choix invalide";
      return newrecord$4;
    }
  } else {
    var newrecord$5 = model.slice();
    newrecord$5[/* pions */2] = string_vers_array(param[0]);
    return newrecord$5;
  }
}

function view_button(title, msg) {
  return button(/* None */0, /* None */0, /* :: */[
              onClick(msg),
              /* [] */0
            ], /* :: */[
              text$1(title),
              /* [] */0
            ]);
}

var ascii_titre = "\n         _    _        _          _              _           _        \n        /\\ \\ /\\ \\     /\\_\\       /\\ \\           /\\ \\        / /\\      \n       /  \\ \\\\ \\ \\   / / /      /  \\ \\         /  \\ \\      / /  \\     \n      / /\\ \\_\\\\ \\ \\_/ / /      / /\\ \\_\\       / /\\ \\ \\    / / /\\ \\__  \n     / / /\\/_/ \\ \\___/ /      / / /\\/_/      / / /\\ \\_\\  / / /\\ \\___\\ \n    / / / ______\\ \\ \\_/      / / / ______   / /_/_ \\/_/  \\ \\ \\ \\/___/ \n   / / / /\\_____\\\\ \\ \\      / / / /\\_____\\ / /____/\\      \\ \\ \\       \n  / / /  \\/____ / \\ \\ \\    / / /  \\/____ // /\\____\\/  _    \\ \\ \\      \n / / /_____/ / /   \\ \\ \\  / / /_____/ / // / /______ /_/\\__/ / /      \n/ / /______\\/ /     \\ \\_\\/ / /______\\/ // / /_______\\\\ \\/___/ /       \n\\/___________/       \\/_/\\/___________/ \\/__________/ \\_____\\/        ";

var style_item = styles$1(/* :: */[
      /* tuple */[
        "padding",
        "5px"
      ],
      /* [] */0
    ]);

var menu = div$2(/* None */0, /* None */0, /* :: */[
      style$2("margin", "20px"),
      /* [] */0
    ], /* :: */[
      span(/* None */0, /* None */0, /* :: */[
            style_item,
            /* [] */0
          ], /* :: */[
            text$1("Description"),
            /* [] */0
          ]),
      /* :: */[
        span(/* None */0, /* None */0, /* :: */[
              style_item,
              /* [] */0
            ], /* :: */[
              text$1("Règles"),
              /* [] */0
            ]),
        /* :: */[
          span(/* None */0, /* None */0, /* :: */[
                style_item,
                /* [] */0
              ], /* :: */[
                text$1("Config"),
                /* [] */0
              ]),
          /* [] */0
        ]
      ]
    ]);

function donne_position(cote, param) {
  var intervalle = cote / 8 | 0;
  return /* tuple */[
          imul(intervalle, param[0]),
          imul(intervalle, 7 - param[1] | 0)
        ];
}

var max_size = max(window.innerWidth, window.innerHeight);

function val_de_pion(p$$1) {
  if (p$$1) {
    return p$$1[0];
  } else {
    return 0;
  }
}

function vers_pix(n) {
  return string_of_int(n) + "px";
}

function grille_liste(m, n) {
  var range = function (i$$1, j) {
    var match = j - i$$1 | 0;
    if (match !== 0) {
      return /* :: */[
              i$$1,
              range(i$$1 + 1 | 0, j)
            ];
    } else {
      return /* [] */0;
    }
  };
  return flatten(map((function (y) {
                    return map((function (x) {
                                  return /* tuple */[
                                          x,
                                          y
                                        ];
                                }), range(m, n + 1 | 0));
                  }), range(m, n + 1 | 0)));
}

function vue_plateau(model) {
  var jeu = model[/* partie */0];
  var str = model[/* pions */2];
  var prem = model[/* choixprem */4];
  var cote = min(min(window.innerWidth, window.innerHeight) / 2 | 0, 600);
  var carre = grille_liste(1, 6);
  var intervalle = cote / 8 | 0;
  var fait_case = function (x, y, $$case) {
    return span(/* None */0, /* None */0, /* :: */[
                onClick(/* Choix */__(1, [$$case])),
                /* :: */[
                  caml_equal(prem, /* Some */[$$case]) ? class$prime("case-active") : class$prime("case-inactive"),
                  /* :: */[
                    styles$1(/* :: */[
                          /* tuple */[
                            "left",
                            string_of_int(x) + "px"
                          ],
                          /* :: */[
                            /* tuple */[
                              "top",
                              string_of_int(y) + "px"
                            ],
                            /* :: */[
                              /* tuple */[
                                "width",
                                string_of_int(intervalle) + "px"
                              ],
                              /* :: */[
                                /* tuple */[
                                  "height",
                                  string_of_int(intervalle) + "px"
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "font-size",
                                    string_of_int(imul(intervalle, 3) / 4 | 0) + "px"
                                  ],
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ]),
                    /* [] */0
                  ]
                ]
              ], /* :: */[
                text$1(caml_array_get(str, val_de_pion(pion_dans_case(jeu, $$case)))),
                /* [] */0
              ]);
  };
  var appel = function (param) {
    var y = param[1];
    var x = param[0];
    var match = donne_position(cote, /* tuple */[
          x,
          y
        ]);
    return fait_case(match[0], match[1], /* Case */[
                x,
                y
              ]);
  };
  var nord = fait_case((cote - intervalle | 0) / 2 | 0, 0, /* CaseNord */0);
  var sud = fait_case((cote - intervalle | 0) / 2 | 0, cote - intervalle | 0, /* CaseSud */1);
  return div$2(/* None */0, /* None */0, /* :: */[
              class$prime("plateau"),
              /* :: */[
                styles$1(/* :: */[
                      /* tuple */[
                        "width",
                        string_of_int(cote) + "px"
                      ],
                      /* :: */[
                        /* tuple */[
                          "height",
                          string_of_int(cote) + "px"
                        ],
                        /* [] */0
                      ]
                    ]),
                /* [] */0
              ]
            ], /* :: */[
              div$2(/* None */0, /* None */0, /* :: */[
                    class$prime("bord-plateau"),
                    /* [] */0
                  ], /* :: */[
                    text$1(""),
                    /* [] */0
                  ]),
              /* :: */[
                nord,
                /* :: */[
                  sud,
                  map(appel, carre)
                ]
              ]
            ]);
}

function view(model) {
  return div$2(/* None */0, /* None */0, /* :: */[
              styles$1(/* :: */[
                    /* tuple */[
                      "max-width",
                      "800px"
                    ],
                    /* :: */[
                      /* tuple */[
                        "margin",
                        "auto"
                      ],
                      /* :: */[
                        /* tuple */[
                          "background-color",
                          "#ccc"
                        ],
                        /* [] */0
                      ]
                    ]
                  ]),
              /* [] */0
            ], /* :: */[
              h3(/* None */0, /* None */0, /* :: */[
                    onClick(/* ToggleDesc */0),
                    /* :: */[
                      styles$1(/* :: */[
                            /* tuple */[
                              "text-align",
                              "center"
                            ],
                            /* [] */0
                          ]),
                      /* [] */0
                    ]
                  ], /* :: */[
                    pre(/* None */0, /* None */0, /* :: */[
                          styles$1(/* :: */[
                                /* tuple */[
                                  "margin",
                                  "auto"
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "font-size",
                                    "8px"
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "font-weight",
                                      "bold"
                                    ],
                                    /* [] */0
                                  ]
                                ]
                              ]),
                          /* [] */0
                        ], /* :: */[
                          text$1(ascii_titre),
                          /* [] */0
                        ]),
                    /* :: */[
                      menu,
                      /* [] */0
                    ]
                  ]),
              /* :: */[
                model[/* desc */3] ? p(/* None */0, /* None */0, /* [] */0, /* :: */[
                        text$1("Description de Gyges"),
                        /* [] */0
                      ]) : noNode$1,
                /* :: */[
                  span(/* None */0, /* None */0, /* :: */[
                        style$2("font_size", "20px"),
                        /* [] */0
                      ], /* :: */[
                        text$1(concat$2(" ", to_list(model[/* pions */2]))),
                        /* [] */0
                      ]),
                  /* :: */[
                    button(/* None */0, /* None */0, /* :: */[
                          onClick(/* ChangePions */1),
                          /* [] */0
                        ], /* :: */[
                          text$1("Change les pions"),
                          /* [] */0
                        ]),
                    /* :: */[
                      vue_plateau(model),
                      /* :: */[
                        p(/* None */0, /* None */0, /* [] */0, /* :: */[
                              text$1(model[/* message */6]),
                              /* [] */0
                            ]),
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var partial_arg_000 = /* model : record */[
  /* partie */encours,
  /* joueur : Sud */1,
  /* pions */original,
  /* desc : false */0,
  /* choixprem : None */0,
  /* choixdeux : None */0,
  /* message */""
];

var partial_arg = /* record */[
  partial_arg_000,
  /* update */update,
  /* view */view
];

function main(param, param$1) {
  return beginnerProgram(partial_arg, param, param$1);
}

var toggleDesc = /* ToggleDesc */0;

var changePions = /* ChangePions */1;

var test = /* [] */0;


/* encours Not a pure module */

exports.toggleDesc = toggleDesc;
exports.changePions = changePions;
exports.choix = choix;
exports.original = original;
exports.defaut = defaut;
exports.chiffre = chiffre;
exports.init_jeu = init_jeu;
exports.test = test;
exports.ajout_cle = ajout_cle;
exports.cle_ajout = cle_ajout;
exports.ajout = ajout;
exports.encours = encours;
exports.pions = pions;
exports.string_vers_array = string_vers_array;
exports.init = init;
exports.case_vers_string = case_vers_string;
exports.update = update;
exports.view_button = view_button;
exports.ascii_titre = ascii_titre;
exports.style_item = style_item;
exports.menu = menu;
exports.donne_position = donne_position;
exports.max_size = max_size;
exports.val_de_pion = val_de_pion;
exports.vers_pix = vers_pix;
exports.grille_liste = grille_liste;
exports.vue_plateau = vue_plateau;
exports.view = view;
exports.main = main;

return exports;

}({}));
