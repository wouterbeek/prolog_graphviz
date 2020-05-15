:- module(hello, [export/0, view/0]).

/** <module> Hello, world! for GraphViz

*/

:- use_module(library(yall)).

:- use_module(library(gv)).

dot("x [label=<Hello,<BR/>world!>,shape=diamond];\n").

export :-
  dot(String),
  gv_export('hello.svg', {String}/[Out]>>format(Out, String, [])).

view :-
  dot(String),
  gv_view({String}/[Out]>>format(Out, String, [])).
