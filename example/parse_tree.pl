:- module(parse_tree, [export/0, export/1, view/0, view/1]).

/** <module> Parse tree example

*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(gv)).
:- use_module(library(term_ext)).

export :-
  tree(Tree),
  export(Tree).

export(Tree) :-
  gv_export('parse_tree.svg', {Tree}/[Out]>>export_tree_(Out, Tree, _)).

tree(s(np(det(the),n(cat)),vp(v(loves),np(det(the),n(dog))))).

view :-
  tree(Tree),
  view(Tree).

view(Tree) :-
  gv_view({Tree}/[Out]>>export_tree_(Out, Tree, _)).

export_tree_(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  ascii_id(Id),
  dot_node_id(Out, Id, [label(Op)]),
  maplist(export_tree_(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).
