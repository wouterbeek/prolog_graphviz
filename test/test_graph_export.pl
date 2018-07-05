:- begin_tests(graph_export).

:- use_module(library(apply)).
:- use_module(library(graph/graph_export)).
:- use_module(library(plunit)).
:- use_module(library(yall)).



test(hello, [cleanup(delete_file(File))]) :-
  File = 'hello.svg',
  export_graph(
    File,
    [Out]>>format(Out, "x [label=<Hello,<BR/>world!>]\n", [])
  ).



test(hello2, [cleanup(delete_file(File))]) :-
  File = 'hello2.svg',
  export_graph(
    File,
    [Out]>>dot_node(Out, hello, [label(["Hello,","world!"])])
  ).



test(loves, [cleanup(delete_file(File))]) :-
  File = 'loves.svg',
  export_graph(
    File,
    [Out]>>format(Out, "John -- Mary [label=loves]", [])
  ).



test(parse_tree, [cleanup(delete_file(File))]) :-
  File = 'parse_tree.svg',
  export_tree(File, s(np(det(the),n(cat)),vp(v(loves),np(det(the),n(dog))))).

export_tree(File, Tree) :-
  export_graph(File, {Tree}/[Out]>>export_tree_(Out, Tree, _)).

export_tree_(Out, Tree, Id) :-
  Tree =.. [Op|Trees],
  dot_id(Id),
  dot_node_id(Out, Id, [label(Op)]),
  maplist(export_tree_(Out), Trees, Ids),
  maplist(dot_edge_id(Out, Id), Ids).



test(proof_tree, [cleanup(delete_file(File))]) :-
  File = 'proof_tree.svg',
  export_proof(
    File,
    t(rdfs(3),≡(class,class),[t(axiom(rdfs),range(range,class),[]),
                              t(axiom(rdfs),range(⊆,class),[])])
  ).

export_proof(File, Proof) :-
  export_graph(
    File,
    {Proof}/[Out]>>export_proof_(Out, Proof),
    [directed(true)]
  ).

export_proof_(Out, Tree) :-
  Tree = t(Rule,Concl,Prems),
  dot_node(Out, Concl),
  dot_node(Out, Tree, [label(Rule)]),
  dot_arc(Out, Concl, Tree),
  maplist(export_subproof_(Out, Tree), Prems).

export_subproof_(Out, Node, Tree) :-
  Tree = t(_Rule,Concl,_Prems),
  dot_node(Out, Concl),
  dot_arc(Out, Node, Concl),
  export_proof_(Out, Tree).

:- end_tests(graph_export).
