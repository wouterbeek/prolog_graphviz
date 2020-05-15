:- module(proof_tree, [export/0, export/1, view/0, view/1]).

/** <module> Proof tree example

*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(gv)).

export :-
  proof(Proof),
  export(Proof).

export(Proof) :-
  gv_export('proof_tree.svg', {Proof}/[Out]>>export_proof_(Out, Proof), [directed(true)]).

proof(t(rdfs(3), ∈(class,class), [t(axiom(rdfs), range(range,class), []),
                                  t(axiom(rdfs), range(⊆,class), [])])).

view :-
  proof(Proof),
  view(Proof).

view(Proof) :-
  gv_view({Proof}/[Out]>>export_proof_(Out, Proof), [directed(true)]).

export_proof_(Out, Proof) :-
  Proof = t(Rule,Concl,SubProofs),
  dot_node(Out, Concl),
  dot_node(Out, Proof, [label(Rule)]),
  dot_arc(Out, Concl, Proof),
  maplist(export_subproof_(Out, Proof), SubProofs).

export_subproof_(Out, Proof, SubProof) :-
  SubProof = t(_,Prem,_),
  dot_node(Out, Prem),
  dot_arc(Out, Proof, Prem),
  export_proof_(Out, SubProof).
