:- module(loves, [export/0, view/0]).

/** <module> John loves Mary example

*/

:- use_module(library(yall)).

:- use_module(library(gv)).

dot("John -- Mary [label=loves];\n").

export :-
  dot(String),
  gv_export('loves.svg', {String}/[Out]>>format(Out, String, [])).

view :-
  dot(String),
  gv_view({String}/[Out]>>format(Out, String, [])).
