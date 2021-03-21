/** <application> “Hello, world!” GraphViz example

~~~sh
$ swipl -s hello.pl -g export -t halt
$ swipl -s hello.pl -g view -t halt
~~~

*/

:- use_module(library(yall)).

:- use_module(library(gv)).



%! export is det.

export :-
  example_(String),
  gv_export('hello.svg', {String}/[Out]>>format(Out, String, [])).



%! view is det.

view :-
  example_(String),
  gv_view({String}/[Out]>>format(Out, String, [])).



% GENERICS %

example_("x [label=<Hello,<BR/>world!>,shape=diamond];\n").
