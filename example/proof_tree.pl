:- use_module(library(apply)).
:- use_module(library(graph/graph_export)).
:- use_module(library(yall)).

run :-
  view_proof(
    t(rdfs(3),≡(class,class),[t(axiom(rdfs),range(range,class),[]),
                              t(axiom(rdfs),range(⊆,class),[])])
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
