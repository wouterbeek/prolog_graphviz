:- use_module(library(graph/graph_export)).
:- use_module(library(yall)).

run :-
  export_graph(
    'loves.svg',
    [Out]>>format(Out, "John -- Mary [label=loves]", [])
  ).
