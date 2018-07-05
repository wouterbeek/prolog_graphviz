:- use_module(library(graph/graph_export)).
:- use_module(library(yall)).

run :-
  view_graph([Out]>>format(Out, "x [label=<Hello,<BR/>world!>]\n", [])).
