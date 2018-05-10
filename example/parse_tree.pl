:- use_module(library(apply)).
:- use_module(library(graph/graph_export)).
:- use_module(library(yall)).

export_tree(Tree) :-
  export_graph(
    'parse-tree.svg',
    {Tree}/[Out]>>export_tree(Out, Tree, _),
    [format(svg)]
  ).

export_tree(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  gv_id(Id),
  gv_node_id(Out, Id, [label(Op)]),
  maplist(export_tree(Out), Trees, Ids),
  maplist(gv_edge_id(Out, Id), Ids).
