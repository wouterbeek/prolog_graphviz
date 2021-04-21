/** <application> Parse tree example

~~~sh
$ swipl -s parse_tree.pl -g export -t halt
$ swipl -s parse_tree.pl -g view -t halt
~~~

*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(gv)).
:- use_module(library(term_ext)).



%! export is det.
%! export(+Tree:tree) is det.

export :-
  example_(Tree),
  export(Tree).


export(Tree) :-
  gv_export('parse_tree.svg', {Tree}/[Out0]>>export_tree_(Out0, Tree, _)).



%! view is det.
%! view(+Tree:tree) is det.

view :-
  example_(Tree),
  view(Tree).


view(Tree) :-
  gv_view({Tree}/[Out0]>>export_tree_(Out0, Tree, _)).



% GENERICS %

example_(s(np(det(the),n(cat)),vp(v(loves),np(det(the),n(dog))))).

export_tree_(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  ascii_id(Id),
  dot_node_id(Out, Id, options{label: Op}),
  maplist(export_tree_(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).
