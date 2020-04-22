:- use_module(library(apply)).
:- use_module(library(gv)).
:- use_module(library(yall)).

run :-
  view_proof(
    t(rdfs(3),≡(class,class),[t(axiom(rdfs),range(range,class),[]),
                              t(axiom(rdfs),range(⊆,class),[])])
  ).

view_proof(Proof) :-
  gv_view({Proof}/[Out]>>export_proof_(Out, Proof), [directed(true)]).

export_proof_(Out, Tree) :-
  Tree = t(Rule,Concl,SubTrees),
  dot_node(Out, Concl),
  dot_node(Out, Tree, [label(Rule)]),
  dot_arc(Out, Concl, Tree),
  maplist(export_subproof_(Out, Tree), SubTrees).

export_subproof_(Out, Tree, SubTree) :-
  SubTree = t(_,Prem,_),
  dot_node(Out, Prem),
  dot_arc(Out, Tree, Prem),
  export_proof_(Out, SubTree).
