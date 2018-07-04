:- module(
  graph_export,
  [
  % GRAPHVIZ EXPORT/VIEW
    export_graph/2,     % +File, :Goal_1
    export_graph/3,     % +File, :Goal_1, +Options
    view_graph/1,       % :Goal_1
    view_graph/2,       % :Goal_1, +Options
  % DOT PRIMITIVES
    dot_arc/3,          % +Out, +FromTerm, +ToTerm
    dot_arc/4,          % +Out, +FromTerm, +ToTerm. +Options
    dot_arc_id/3,       % +Out, +FromId, +ToId
    dot_arc_id/4,       % +Out, +FromId, +ToId. +Options
    dot_edge/3,         % +Out, +FromTerm, +ToTerm
    dot_edge/4,         % +Out, +FromTerm, +ToTerm. +Options
    dot_edge_id/3,      % +Out, +FromId, +ToId
    dot_edge_id/4,      % +Out, +FromId, +ToId. +Options
    dot_html_replace/2, % +Unescaped, -Escaped
    dot_id/1,           % -Id
    dot_id/2,           % +Term, -Id
    dot_node/2,         % +Out, +Term
    dot_node/3,         % +Out, +Term, +Options
    dot_node_id/2,      % +Out, +Id
    dot_node_id/3,      % +Out, +Id, +Options
  % GRAPHVIZ FORMATS/METHODS
    gv_format/1,        % ?Format
    gv_format_type/2,   % ?Format, ?Type
    gv_method/1         % ?Method
  ]
).

/** <module> Graph export

Support for exporting graphs using the GraphViz DOT format.

---

@author Wouter Beek
@see https://www.graphviz.org
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- meta_predicate
    call_must_be(1, +),
    export_graph(+, 1),
    export_graph(+, 1, +),
    view_graph(1),
    view_graph(1, +),
    write_graph(+, 1, +).

:- setting(default_export_format, atom, svg,
           "The default format that is used when exporting a graph using GraphViz.").
:- setting(default_method, atom, dot,
           "The default method that is used when creating a GraphViz visualization.").
:- setting(default_view_format, atom, gtk,
           "The default format that is used when viewing a graph using GraphViz.").





% GRAPHVIZ EXPORT/VIEW %

%! export_graph(+File:atom, :Goal_1) is det.
%! export_graph(+File:atom, :Goal_1, +Options:list(compound)) is det.
%
% @arg File is the name of the file to which the graph export is
%      written.
%
% @arg Goal_1 is a unary goal that takes a Prolog output stream that
%      receives DOT formatted messages.
%
% @arg Options is a list that may include any of the following
%      options:
%
%      * directed(+boolean)
%
%        Whether the graph is directed (`true`) or undirected
%        (`false`, default).
%
%      * format(+atom)
%
%        The format that is used to store the output in.  Both binary
%        and text output formats are supported.  See
%        `gv_format_type(-Format, Type), memberchk(Type,
%        [binary,text])` for possible values.  The default value is
%        stored in setting `default_export_format`.
%
%      * method(+atom)
%
%        The method that is used by GraphViz to calculate the graph
%        layout.  See `gv_method(-Method)` for possible values.  The
%        default value is stored in setting `default_method`.
%
%      * name(+atom)
%
%        The name of the graph.  Default is `noname`.
%
%      * overlap(+boolean)
%
%        Whether or not nodes are allowed to overlap.  Default is
%        `false`.

export_graph(File, Goal_1) :-
  export_graph(File, Goal_1, []).


export_graph(File, Goal_1, Options) :-
  export_format_option(Format, Type, Options),
  method_option(Method, Options),
  setup_call_cleanup(
    open(File, write, Out, [type(Type)]),
    setup_call_cleanup(
      (
        % Open a GraphViz input and a GraphViz output stream.  The
        % input stream expects statments in the DOT language.  The
        % output stream is in the specified Format.
        process_create(
          path(Method),
          ['-T',Format],
          [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
        ),
        % Binary and text streams are treated differently.
        set_stream(ProcOut, type(Type))
      ),
      (
        call_cleanup(
          write_graph(ProcIn, Goal_1, Options),
          close(ProcIn)
        ),
        copy_stream_data(ProcOut, Out)
      ),
      close(ProcOut)
    ),
    close(Out)
  ).

export_format_option(Format, Type, Options) :-
  (   option(format(Format), Options)
  ->  true
  ;   setting(default_export_format, Format)
  ),
  call_must_be(export_format, Format),
  gv_format_type(Format, Type).

export_format(Format) :-
  gv_format_type(Format, Type),
  memberchk(Type, [binary,text]).

method_option(Method, Options) :-
  (   option(method(Method), Options)
  ->  true
  ;   setting(default_method, Method)
  ),
  call_must_be(gv_method, Method).

write_graph(Out, Goal_1, Options) :-
  option(directed(Directed), Options, false),
  must_be(boolean, Directed),
  graph_type(Directed, Type),
  option(name(Name), Options, noname),
  must_be(atom, Name),
  format_debug(dot, Out, "~a ~a {", [Type,Name]),
  write_graph_attributes(Out, Options),
  call(Goal_1, Out),
  format_debug(dot, Out, "}").

graph_type(false, graph).
graph_type(true, digraph).

write_graph_attributes(Out, Options) :-
  option(overlap(Overlap), Options, false),
  must_be(boolean, Overlap),
  format_debug(dot, Out, "  graph [overlap=~a];", [Overlap]).



%! view_graph(:Goal_1) is det.
%! view_graph(:Goal_1, +Options:list(compound)) is det.
%
% Generate a GraphViz graph visualization and open the result in a
% viewer application.
%
% @arg Goal_1 is a unary goal that takes a Prolog output stream that
%      receives DOT formatted messages.
%
% @arg Options is a list that may include any of the options defined
%      for export_graph/3.

view_graph(Goal_1) :-
  view_graph(Goal_1, []).


view_graph(Goal_1, Options) :-
  view_format_option(Format, Options),
  method_option(Method, Options),
  setup_call_cleanup(
    process_create(path(Method), ['-T',Format], [stdin(pipe(ProcIn))]),
    write_graph(ProcIn, Goal_1, Options),
    close(ProcIn)
  ).

view_format_option(Format, Options) :-
  (   option(format(Format), Options)
  ->  true
  ;   setting(default_view_format, Format)
  ),
  call_must_be(view_format, Format).

view_format(Format) :-
  gv_format_type(Format, viewer).





% DOT PRIMITIVES %

%! dot_arc(+Out:stream, +FromTerm:term, +ToTerm:term) is det.
%! dot_arc(+Out:stream, +FromTerm:term, +ToTerm:term, +Options:list(compound)) is det.
%
% Emits an arc (directed edge) from one Prolog term to another in the
% DOT language.
%
% Since Prolog terms cannot be used as DOT IDs, dot_arc/[3,4] and
% automatically creates compatible DOT IDs under the hood.  The same
% Prolog term is always denoted by the same DOT ID.
%
% @see Most of the time, the use of Prolog terms instead of DOT IDs is
%      preferable.  However, there are legitimate use cases where the
%      programmer would like to generate and use the DOT IDs herself.
%      For these purposes, dot_arc_id/[3,4] can be used -- in
%      combination with dot_id/2 -- instead.

dot_arc(Out, FromTerm, ToTerm) :-
  dot_arc(Out, FromTerm, ToTerm, []).


dot_arc(Out, FromTerm, ToTerm, Options) :-
  maplist(dot_id, [FromTerm,ToTerm], [FromId,ToId]),
  dot_arc_id(Out, FromId, ToId, Options).



%! dot_arc_id(+Out:stream, +FromId:atom, +ToId:atom) is det.
%! dot_arc_id(+Out:stream, +FromId:atom, +ToId:atom, +Options:list(compound)) is det.
%
% Emits a directed edge or arc from one DOT ID to another in the DOT
% language.
%
% @see dot_arc/[3,4] allows arcs to be asserted between Prolog terms.

dot_arc_id(Out, FromId, ToId) :-
  dot_arc_id(Out, FromId, ToId, []).


dot_arc_id(Out, FromId, ToId, Options) :-
  dot_attributes(Options, Str),
  format_debug(dot, Out, "  ~a -> ~a~s;", [FromId,ToId,Str]).



%! dot_attributes(+Options:list(compound), -String:string) is det.

dot_attributes([], "") :- !.
dot_attributes(Options, Str) :-
  maplist(dot_attribute, Options, Strs),
  atomics_to_string(Strs, ",", Str0),
  format(string(Str), " [~s]", [Str0]).

dot_attribute(Option, Str) :-
  Option =.. [Name,Value],
  dot_attribute(Name, Value, Str).

% Multi-line label
dot_attribute(label, Values, Str) :-
  is_list(Values), !,
  maplist(dot_label_string, Values, Strs),
  atomics_to_string(Strs, "<BR/>", Str0),
  format(string(Str), "label=<~s>", [Str0]).
% Single-line label
dot_attribute(label, Value, Str) :- !,
  dot_attribute(label, [Value], Str).
% other attributes
dot_attribute(Name, Value, Str) :-
  format(string(Str), "~a=\"~s\"", [Name,Value]).

dot_label_string(Str, Str) :-
  string(Str), !.
dot_label_string(Term, Str) :-
  format(string(Str), "~w", [Term]).



%! dot_edge(+Out:stream, +FromTerm:term, +ToTerm:term) is det.
%! dot_edge(+Out:stream, +FromTerm:term, +ToTerm:term, +Options:list(compound)) is det.
%
% Emits an edge between two Prolog terms in the DOT language.
%
% Since Prolog terms cannot be immediate used as DOT IDs,
% dot_edge/[3,4] and dot_node/[2,3] automatically create compatible DOT
% IDs under the hood.  When the same Prolog term is given to these
% predicates, it is guaranteed that the DOT ID will also be the same.
%
% @see Most of the time, the use of Prolog terms instead of DOT ID is
%      preferable.  However, there are legitimate use cases where the
%      programmer would like to generate and use the DOT IDs
%      themselves.  For these purposes, dot_edge_id/[3,4] can be used
%      -- in combination with dot_id/2 -- instead.

dot_edge(Out, FromTerm, ToTerm) :-
  dot_edge(Out, FromTerm, ToTerm, []).


dot_edge(Out, FromTerm, ToTerm, Options) :-
  maplist(dot_id, [FromTerm,ToTerm], [FromId,ToId]),
  dot_edge_id(Out, FromId, ToId, Options).



%! dot_edge_id(+Out:stream, +FromId:atom, +ToId:atom) is det.
%! dot_edge_id(+Out:stream, +FromId:atom, +ToId:atom, +Options:list(compound)) is det.
%
% Emits an edge between two DOT IDs in the DOT language.
%
% @see dot_edge/[3,4] allows edges to be asserted between Prolog terms.

dot_edge_id(Out, FromId, ToId) :-
  dot_edge_id(Out, FromId, ToId, []).


dot_edge_id(Out, FromId, ToId, Options) :-
  dot_attributes(Options, Str),
  format_debug(dot, Out, "  ~a -- ~a~s;", [FromId,ToId,Str]).



%! dot_html_replace(+Unescaped:string, -Escaped:string) is det.
%
% Replaces the following characters that are not allowed to occur in
% DOT HTML labels with HTML elements: left and right angle bracket,
% ampersand.

dot_html_replace(Str1, Str2) :-
  string_codes(Str1, Cs1),
  phrase(html_replace, Cs1, Cs2),
  string_codes(Str2, Cs2).

html_replace, "&lt;" --> "<", !, html_replace.
html_replace, "&gt;" --> ">", !, html_replace.
html_replace, "&amp;" --> "&", !, html_replace.
html_replace, [C] --> [C], !, html_replace.
html_replace --> "".



%! dot_id(-Id:atom) is det.

dot_id(Id) :-
  uuid(Id0, [format(integer)]),
  atom_concat(n, Id0, Id).



%! dot_id(+Term:term, -Id:atom) is det.
%
% Create a DOT ID that can be used to represent a Prolog term in the
% DOT language.  When the same Prolog term is supplied, the DOT ID is
% also the same.

dot_id(Term, Id) :-
  term_to_atom(Term, Atom),
  % DOT IDs cannot contain all characters allowed in Prolog terms.
  % Also, Prolog terms can have arbitrary length.  For these reasons,
  % we calculate the MD5 hash of a serialization of the Prolog term.
  md5_hash(Atom, Hash, []),
  % DOT IDs must start with an ASCII letter.  Since an MD5 hash may
  % start with a decimal digit, an specific ASCII letter is prefixed.
  atomic_concat(n, Hash, Id).



%! dot_node(+Out:stream, +Term:term) is det.
%! dot_node(+Out:stream, +Term:term, +Options:list(compound)) is det.
%
% @arg Out is a handle to an output stream.
%
% @arg is a Prolog term.
%
% @arg Options is a list of compound terms, each of which denotes a
% GraphViz attribute.  The following attributes are supported:
%
%   * label(+or([string,list(string)]))
%
%     Allows either a single string or a list of strings to be used as
%     a node label.  The strings are allowed to contain Unicode
%     characters and a limited set of HTML tags for markup purposes
%     (see `README.org`).  Regular DOT labels do not allow such
%     flexibility, so strings supplied through this option are
%     exported as DOT HTML labels.
%
%   * Other options are written as regular DOT attributes
%     (`KEY="VALUE"`).  See `README.org` for an overview of supported
%     GraphViz attributes.
%
% @see Most of the time, the use of Prolog terms instead of DOT ID is
%      preferable.  However, there are legitimate use cases where the
%      programmer would like to generate and use the DOT IDs
%      themselves.  For these purposes, dot_node_id/[2,3] can be used --
%      in combination with dot_id/2 -- instead.

dot_node(Out, Term) :-
  dot_node(Out, Term, [label(Term)]).


dot_node(Out, Term, Options) :-
  dot_id(Term, Id),
  dot_node_id(Out, Id, Options).



%! dot_node_id(+Out:stream, +Id:atom) is det.
%! dot_node_id(+Out:stream, +Id:atom, +Options:list(compound)) is det.
%
% @see dot_node/[2,3] allows nodes to be asserted for Prolog terms.

dot_node_id(Out, Id) :-
  dot_node_id(Out, Id, []).


dot_node_id(Out, Id, Options) :-
  dot_attributes(Options, Str),
  format_debug(dot, Out, "  ~a~s;", [Id,Str]).





% GRAPHVIZ FORMATS/METHODS %

%! gv_format(?Format:atom) is nondet.

gv_format(Format) :-
  gv_format_type(Format, _).



%! gv_format_type(?Format:atom, ?Type:oneof([binary,text,viewer])) is nondet.
%
% GraphViz provides two types of export: binary (e.g., `jpeg`) and
% text (e.g., `svg`).  The third type `viewer` is use to directly open
% GraphViz output in a viewer application (without storing the result
% to a file).

gv_format_type(bmp,         binary).
gv_format_type(canon,       text).   % DOT, pretty-printed, no layout.
gv_format_type(cgimage,     binary). % CGImage, a drawable image object in Core
                                     % Graphics (the low-level procedural
                                     % drawing API for iOS and Mac OS X).
gv_format_type(cmap,        text).   % Client-side imagemap (deprecated).
gv_format_type(cmapx,       text).   % Server-side and client-side imagemap.
gv_format_type(cmapx_np,    text).   % Server-side and client-side imagemap.
gv_format_type(dot,         text).   % JSON version of `-Txdot' without layout.
gv_format_type(dot_json,    text).   % JSON version of `-Tdot' without layout.
gv_format_type(eps,         binary).
gv_format_type(exr,         binary).
gv_format_type(fig,         text).
gv_format_type(gd,          text).
gv_format_type(gd2,         binary).
gv_format_type(gif,         binary).
gv_format_type(gtk,         viewer).
gv_format_type(gv,          text).   % Same as `dot'.
gv_format_type(ico,         binary).
gv_format_type(imap,        text).   % Server-side and client-side imagemap.
gv_format_type(imap_np,     text).   % Same as `imap'.
gv_format_type(ismap,       text).   % Server-side and client-side imagemap
                                     % (deprecated).
gv_format_type(jp2,         binary).
gv_format_type(jpe,         binary). % Same as `jpeg'.
gv_format_type(jpeg,        binary).
gv_format_type(jpg,         binary). % Same as `jpeg'.
gv_format_type(json,        text).   % JSON version of `-Tdot'.
gv_format_type(json0,       text).   % JSON version of `-Txdot'.
gv_format_type(pct,         binary).
gv_format_type(pdf,         binary).
gv_format_type(pic,         text).
gv_format_type(pict,        text).   % Same as `pic'.
gv_format_type(plain,       text).
gv_format_type('plain-ext', text).   % Same as `plain'.
gv_format_type(png,         binary).
gv_format_type(pov,         binary).
gv_format_type(ps,          binary).
gv_format_type(ps2,         binary). % PostScript output with PDF notations.
gv_format_type(psd,         binary).
gv_format_type(sgi,         binary).
gv_format_type(svg,         text).
gv_format_type(svgz,        binary).
gv_format_type(tga,         binary).
gv_format_type(tif,         binary).
gv_format_type(tiff,        binary).
gv_format_type(tk,          text).
gv_format_type(vdx,         text).
gv_format_type(vml,         text).
gv_format_type(vmlz,        binary).
gv_format_type(vrml,        text).
gv_format_type(wbmp,        binary).
gv_format_type(webp,        binary).
gv_format_type(x11,         none).
gv_format_type(xdot,        text).
gv_format_type(xdot_json,   text).
gv_format_type('xdot1.2',   text).
gv_format_type('xdot1.4',   text).
gv_format_type(xlib,        none).



%! gv_method(?Method:atom) is nondet.
%
% Layout methods supported by GraphViz.

gv_method(circo).
gv_method(dot).
gv_method(fdp).
gv_method(neato).
gv_method(osage).
gv_method(sfdp).
gv_method(twopi).





% HELPERS %

%! call_must_be(:Goal_1, +Term:term) is det.

call_must_be(Goal_1, Term) :-
  findall(Atom, call(Goal_1, Atom), Atoms),
  must_be(oneof(Atoms), Term).



%! format_debug(+Flag:term, +Out:stream, +Pattern:string) is det.
%! format_debug(+Flag:term, +Out:stream, +Pattern:string, +Arguments:list(term)) is det.
%
% Allows a line of text to be written to an output stream and --
% optionally -- to a debug stream as well.
%
% Pattern and Arguments are used to compose a line of text.  The
% newline character is automatically added at the end.
%
% Debug information is displayed by calling `debug(Flag)` (see library
% debug).  Flag can be an atom or a compound term.

format_debug(Flag, Out, Pattern) :-
  format_debug(Flag, Out, Pattern, []).


format_debug(Flag, Out, Pattern, Args) :-
  string_concat(Pattern, "\n", PatternNewline),
  format(Out, PatternNewline, Args),
  debug(Flag, Pattern, Args).
