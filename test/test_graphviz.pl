/** <test> Tests for the GraphViz library

*/

:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(process)).
:- use_module(library(yall)).

:- use_module(library(gv)).
:- use_module(library(os_ext)).
:- use_module(library(term_ext)).

:- begin_tests(gv).

:- meta_predicate
    test_gv_export(+, 1),
    test_gv_export(+, 1, +).

test(hello, [cleanup(delete_file(File))]) :-
  File = 'hello.pdf',
  test_gv_export(
    File,
    [Out0]>>format(Out0, "x [label=<Hello,<BR/>world!>,shape=diamond];\n", [])
  ).



test(hello2, [cleanup(delete_file(File))]) :-
  File = 'hello2.pdf',
  test_gv_export(
    File,
    [Out0]>>
      dot_node(
        Out0,
        hello,
        options{label: ["Hello,","world!"], shape: diamond}
      )
  ).



test(arc, [cleanup(delete_file(File))]) :-
  File = 'arc.svg',
  test_gv_export(File, [Out0]>>format(Out0, "John -- Mary [label=arc]", [])).



test(parse_tree, [cleanup(delete_file(File))]) :-
  File = 'parse_tree.svg',
  Tree = s(np(det(the),n(cat)),vp(v(knows),np(det(the),n(dog)))),
  test_gv_export(File, {Tree}/[Out0]>>export_tree_(Out0, Tree, _)).

export_tree_(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  ascii_id(Id),
  dot_node_id(Out, Id, options{label: Op}),
  maplist(export_tree_(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).



test(proof_tree, [cleanup(delete_file(File))]) :-
  File = 'proof_tree.svg',
  Proof = tree(rdfs(3),≡(class,class),[tree(axiom(rdfs),range(range,class),[]),
                                        tree(axiom(rdfs),range(⊆,class),[])]),
  test_gv_export(
    File,
    {Proof}/[Out0]>>export_proof_(Out0, Proof),
    options{directed: true}
  ).

export_proof_(Out, Tree) :-
  Tree = tree(Rule,Concl,SubTrees),
  dot_node(Out, Concl),
  dot_node(Out, Tree, options{color: green, label: Rule}),
  dot_arc(Out, Concl, Tree),
  maplist(export_subproof_(Out, Tree), SubTrees).

export_subproof_(Out, Tree, SubTree) :-
  SubTree = tree(_,Prem,_),
  dot_node(Out, Prem),
  dot_arc(Out, Tree, Prem),
  export_proof_(Out, SubTree).





% HELPERS %

%! test_gv_export(+File:atom, :Goal_1) is det.
%! test_gv_export(+File:atom, :Goal_1, +Options:list(compound)) is det.

test_gv_export(File, Goal_1) :-
  test_gv_export(File, Goal_1, options{}).


test_gv_export(File, Goal_1, Options) :-
  gv_export(File, Goal_1, Options),
  open_file(File).

:- end_tests(gv).
