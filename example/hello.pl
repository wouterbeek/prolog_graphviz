:- use_module(library(graph/gv)).
:- use_module(library(yall)).

run :-
  gv_view([Out]>>format(Out, "x [label=<Hello,<BR/>world!>]\n", [])).
