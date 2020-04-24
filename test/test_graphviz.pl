:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(process)).
:- use_module(library(yall)).

:- use_module(library(gv)).
:- use_module(library(os_ext)).

:- meta_predicate
    test_gv_export(+, 1),
    test_gv_export(+, 1, +).

:- begin_tests(gv).

test(hello, [cleanup(delete_file(File))]) :-
  File = 'hello.pdf',
  test_gv_export(
    File,
    [Out]>>format(Out, "x [label=<Hello,<BR/>world!>,shape=diamond];\n", [])
  ).



test(hello2, [cleanup(delete_file(File))]) :-
  File = 'hello2.pdf',
  test_gv_export(
    File,
    [Out]>>dot_node(Out, hello, [label(["Hello,","world!"]),shape(diamond)])
  ).



test(loves, [cleanup(delete_file(File))]) :-
  File = 'loves.svg',
  test_gv_export(File, [Out]>>format(Out, "John -- Mary [label=loves]", [])).



test(parse_tree, [cleanup(delete_file(File))]) :-
  File = 'parse_tree.svg',
  Tree = s(np(det(the),n(cat)),vp(v(loves),np(det(the),n(dog)))),
  test_gv_export(File, {Tree}/[Out]>>export_tree_(Out, Tree, _)).

export_tree_(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  dot_id(Id),
  dot_node_id(Out, Id, [label(Op)]),
  maplist(export_tree_(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).



test(proof_tree, [cleanup(delete_file(File))]) :-
  File = 'proof_tree.svg',
  Proof = t(rdfs(3),≡(class,class),[t(axiom(rdfs),range(range,class),[]),
                                    t(axiom(rdfs),range(⊆,class),[])]),
  test_gv_export(
    File,
    {Proof}/[Out]>>export_proof_(Out, Proof),
    [directed(true)]
  ).

export_proof_(Out, Tree) :-
  Tree = t(Rule,Concl,SubTrees),
  dot_node(Out, Concl),
  dot_node(Out, Tree, [color(green),label(Rule)]),
  dot_arc(Out, Concl, Tree),
  maplist(export_subproof_(Out, Tree), SubTrees).

export_subproof_(Out, Tree, SubTree) :-
  SubTree = t(_,Prem,_),
  dot_node(Out, Prem),
  dot_arc(Out, Tree, Prem),
  export_proof_(Out, SubTree).





% HELPERS %

%! test_gv_export(+File:atom, :Goal_1) is det.
%! test_gv_export(+File:atom, :Goal_1, +Options:list(compound)) is det.

test_gv_export(File, Goal_1) :-
  test_gv_export(File, Goal_1, []).


test_gv_export(File, Goal_1, Options) :-
  gv_export(File, Goal_1, Options),
  open_file(File).

:- end_tests(gv).
