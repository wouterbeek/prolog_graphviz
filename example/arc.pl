/** <application> Example of a simple arc

~~~sh
$ swipl -s arc.pl -g export -t halt
$ swipl -s arc.pl -g view -t halt
~~~

*/

:- use_module(library(yall)).

:- use_module(library(gv)).



%! export is det.

export :-
  example_(String),
  gv_export('loves.svg', {String}/[Out]>>format(Out, String, [])).



%! view is det.

view :-
  example_(String),
  gv_view({String}/[Out]>>format(Out, String, [])).



% GENERICS %

example_("John -- Mary [label=loves];\n").
