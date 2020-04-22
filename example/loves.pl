:- use_module(library(gv)).
:- use_module(library(yall)).

run :-
  gv_export('loves.svg', [Out]>>format(Out, "John -- Mary [label=loves]", [])).
