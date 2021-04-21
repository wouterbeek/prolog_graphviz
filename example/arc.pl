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
  gv_export('loves.svg', {String}/[Out0]>>format(Out0, String, [])).



%! view is det.

view :-
  example_(String),
  gv_view({String}/[Out0]>>format(Out0, String, [])).



% GENERICS %

example_("John -- Mary [label=loves];\n").
