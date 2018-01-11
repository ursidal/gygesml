// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

import * as List       from "bs-platform/lib/es6/list.js";
import * as Curry      from "bs-platform/lib/es6/curry.js";
import * as Caml_obj   from "bs-platform/lib/es6/caml_obj.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";

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
    return (c1[1] - c2[1] | 0) + Caml_int32.imul(6, c1[0] - c2[0] | 0) | 0;
  }
}

function $pipe$great(x, f) {
  return Curry._1(f, x);
}

function init_jeu() {
  return /* [] */0;
}

function pion_dans_case(_partie, $$case) {
  while(true) {
    var partie = _partie;
    if (partie) {
      var hd = partie[0];
      if (Caml_obj.caml_equal(hd[0], $$case)) {
        return hd[1];
      } else {
        _partie = partie[1];
        continue ;
        
      }
    } else {
      return /* Vide */0;
    }
  };
}

function ajout_cle(f, cle, valeur, x) {
  if (Caml_obj.caml_equal(x, cle)) {
    return valeur;
  } else {
    return Curry._1(f, x);
  }
}

function cle_ajout(cle, valeur, f) {
  return (function (param) {
      return ajout_cle(f, cle, valeur, param);
    });
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
  if (Caml_obj.caml_equal(x, z) && Caml_obj.caml_equal(y, t) || Caml_obj.caml_equal(x, t) && Caml_obj.caml_equal(y, z)) {
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
      return List.for_all((function (x) {
                    return 1 - meme_chemin(x, List.hd(lp));
                  }), List.tl(lp));
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
  return List.filter((function (param) {
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
    return List.map((function (param) {
                  return /* Case */[
                          param[0],
                          param[1]
                        ];
                }), param);
  };
  if (typeof pos === "number") {
    if (pos !== 0) {
      return List.map((function (i) {
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
      return List.map((function (i) {
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
      var dernier = List.hd(lchemin);
      var lvoisins = List.filter((function (p) {
                if (pion_dans_case(partie, p) === /* Vide */0 && p !== /* CaseNord */0) {
                  return +(p !== /* CaseSud */1);
                } else {
                  return /* false */0;
                }
              }))(voisins(dernier));
      var param = List.filter(pas_recoupe)(List.map((function (x) {
                  return /* :: */[
                          x,
                          lchemin
                        ];
                }), lvoisins));
      return List.concat(List.map((function (lch) {
                        return chemins(partie, n - 1 | 0, lch);
                      }), param));
    } else {
      var dernier$1 = List.hd(lchemin);
      var lvoisins$1 = voisins(dernier$1);
      var param$1 = List.filter(pas_recoupe)(List.map((function (x) {
                  return /* :: */[
                          x,
                          lchemin
                        ];
                }), lvoisins$1));
      return List.concat(List.map((function (lch) {
                        var dern = List.hd(lch);
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
    return List.sort_uniq(compare_case, List.map(List.hd, liste_chemins));
  } else {
    return /* [] */0;
  }
}

function departs_possibles(jeu, joueur) {
  var premiere_ligne = function (_i) {
    while(true) {
      var i = _i;
      var pion_de_ligne = List.filter((function (x) {
                return +(x !== /* Vide */0);
              }))(List.map((function(i){
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
    };
  };
  var i = joueur !== 0 ? 1 : 6;
  var ligne = premiere_ligne(i);
  return List.filter((function (x) {
                  return +(pion_dans_case(jeu, x) !== /* Vide */0);
                }))(List.map((function (x) {
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

function ajout(f, p) {
  var v = p[1];
  var k = p[0];
  return (function (param) {
      return ajout_cle(f, k, v, param);
    });
}

var encours = List.map((function (param) {
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

var test = /* [] */0;

export {
  compare_case      ,
  $pipe$great       ,
  init_jeu          ,
  pion_dans_case    ,
  ajout_cle         ,
  cle_ajout         ,
  mise_a_jour       ,
  meme_chemin       ,
  par_paire         ,
  pas_recoupe       ,
  voisins_bords     ,
  voisins           ,
  int_de_pos        ,
  chemins           ,
  depl_possibles    ,
  departs_possibles ,
  test              ,
  ajout             ,
  encours           ,
  
}
/* encours Not a pure module */
