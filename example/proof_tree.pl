/** <example> Example of exporting a proof tree

This example file shows a simple use case for library ~graph_export~,
in which the user wants to view a proof tree, without first writing
the export to file (~view_graph/[1,2]~).

Notice that the exported graph is *directed*.  This is indicated by
the ~directed(true)~ option that is passed to ~view_graph/2~, and by
the use of ~dot_arc/3~ instead of ~dot_edge/3~.

Notice that no DOT ID is generated explicitly, instead, the Prolog
terms themselves are used in order to characterize the nodes.

*/

:- use_module(library(apply)).
:- use_module(library(graph/graph_export)).
:- use_module(library(yall)).

test :-
  view_proof(
    t(rdfs(3),isa(class,class),[t(axiom(rdfs),range(range,class),[]),
                                t(axiom(rdfs),range(subClassOf,class),[])])
  ).

view_proof(Proof) :-
  view_graph({Proof}/[Out]>>export_proof(Out, Proof), [directed(true)]).

export_proof(Out, Tree) :-
  Tree = t(Rule,Concl,Prems),
  dot_node(Out, Concl),
  dot_node(Out, Tree, [label(Rule)]),
  dot_arc(Out, Concl, Tree),
  maplist(export_subproof(Out, Tree), Prems).
  
export_subproof(Out, Node, Tree) :-
  Tree = t(_Rule,Concl,_Prems),
  dot_node(Out, Concl),
  dot_arc(Out, Node, Concl),
  export_proof(Out, Tree).
