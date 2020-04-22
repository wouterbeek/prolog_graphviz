:- use_module(library(gv)).
:- use_module(library(yall)).

run :-
  gv_view([Out]>>format(Out, "x [label=<Hello,<BR/>world!>]\n", [])).
