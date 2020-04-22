:- use_module(library(apply)).
:- use_module(library(gv)).
:- use_module(library(yall)).

run :-
  export_tree(s(np(det(the),n(cat)),vp(v(loves),np(det(the),n(dog))))).

export_tree(Tree) :-
  gv_export('parse_tree.svg', {Tree}/[Out]>>export_tree(Out, Tree, _)).

export_tree(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  dot_id(Id),
  dot_node_id(Out, Id, [label(Op)]),
  maplist(export_tree(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).
