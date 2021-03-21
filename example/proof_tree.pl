/** <application> Proof tree example

~~~sh
$ swipl -s proof_tree.pl -g export -t halt
$ swipl -s proof_tree.pl -g view -t halt
~~~

*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- use_module(library(gv)).



%! export is det.
%! export(+Proof:tree) is det.

export :-
  example_(Proof),
  export(Proof).


export(Proof) :-
  gv_export(
    'proof_tree.svg',
    {Proof}/[Out]>>export_proof_(Out, Proof),
    options{directed: true}
  ).



%! view is det.
%! view(+Proof:tree) is det.

view :-
  example_(Proof),
  view(Proof).


view(Proof) :-
  gv_view(
    {Proof}/[Out]>>export_proof_(Out, Proof),
    options{directed: true}
  ).



% GENERICS %

export_proof_(Out, Proof) :-
  Proof = tree(Rule,Concl,SubProofs),
  dot_node(Out, Concl),
  dot_node(Out, Proof, [label(Rule)]),
  dot_arc(Out, Concl, Proof),
  maplist(export_subproof_(Out, Proof), SubProofs).

export_subproof_(Out, Proof, SubProof) :-
  SubProof = tree(_,Prem,_),
  dot_node(Out, Prem),
  dot_arc(Out, Proof, Prem),
  export_proof_(Out, SubProof).

example_(tree(rdfs(3),
              ∈(class,class),
              [tree(axiom(rdfs), range(range,class), []),
               tree(axiom(rdfs), range(⊆,class), [])])).
