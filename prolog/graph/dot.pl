:- module(
  dot,
  [
    dot_arc/3,          % +Out, +FromTerm, +ToTerm
    dot_arc/4,          % +Out, +FromTerm, +ToTerm. +Options
    dot_arc_id/3,       % +Out, +FromId, +ToId
    dot_arc_id/4,       % +Out, +FromId, +ToId. +Options
    dot_edge/3,         % +Out, +FromTerm, +ToTerm
    dot_edge/4,         % +Out, +FromTerm, +ToTerm. +Options
    dot_edge_id/3,      % +Out, +FromId, +ToId
    dot_edge_id/4,      % +Out, +FromId, +ToId. +Options
    dot_graph/2,        % +Out, :Goal_1
    dot_graph/3,        % +Out, :Goal_1, +Options
    dot_html_replace/2, % +Unescaped, -Escaped
    dot_id/1,           % -Id
    dot_id/2,           % +Term, -Id
    dot_node/2,         % +Out, +Term
    dot_node/3,         % +Out, +Term, +Options
    dot_node_id/2,      % +Out, +Id
    dot_node_id/3       % +Out, +Id, +Options
  ]
).

/* <module> DOT serialization

@author Wouter Beek
@see https://www.graphviz.org
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(debug_ext)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).

:- meta_predicate
    dot_graph(+, 1),
    dot_graph(+, 1, +).





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
dot_attribute(label, Values, Attr) :-
  is_list(Values), !,
  maplist([Value,Line]>>format(string(Line), "~w", [Value]), Values, Lines),
  atomics_to_string(Lines, "<BR/>", Unescaped),
  dot_html_replace(Unescaped, Escaped),
  format(string(Attr), "label=<~s>", [Escaped]).
% Single-line label
dot_attribute(label, Value, Attr) :- !,
  dot_attribute(label, [Value], Attr).
% other attributes
dot_attribute(Name, Value, Attr) :-
  format(string(Attr), "~a=\"~s\"", [Name,Value]).



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



%! dot_graph(+Out:stream, :Goal_1) is det.
%! dot_graph(+Out:stream, :Goal_1, +Options:list(compound)) is det.
%
% The following options are supported:
%
%   * directed(+boolean)
%
%     Whether the graph is directed (`true`) or undirected (`false`,
%     default).
%
%   * name(+atom)
%
%   The name of the graph.  Default is `noname`.

dot_graph(Out, Goal_1) :-
  dot_graph(Out, Goal_1, []).


dot_graph(Out, Goal_1, Options) :-
  option(directed(Directed), Options, false),
  must_be(boolean, Directed),
  dot_graph_type(Directed, Type),
  option(name(Name), Options, noname),
  must_be(atom, Name),
  format_debug(dot, Out, "~a ~a {", [Type,Name]),
  dot_graph_attributes(Out, Options),
  call(Goal_1, Out),
  format_debug(dot, Out, "}").

dot_graph_attributes(Out, Options) :-
  option(overlap(Overlap), Options), !,
  must_be(boolean, Overlap),
  format_debug(dot, Out, "  graph [overlap=~a];", [Overlap]).
dot_graph_attributes(_, _).

dot_graph_type(false, graph).
dot_graph_type(true, digraph).



%! dot_html_replace(+Unescaped:string, -Escaped:string) is det.
%
% Replaces the following characters that are not allowed to occur in
% DOT HTML labels with HTML elements: left and right angle bracket,
% ampersand.

dot_html_replace(Str1, Str2) :-
  string_phrase(html_replace, Str1, Str2).

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
  % DOT IDs cannot contain all characters allowed in Prolog terms.
  % Also, Prolog terms can have arbitrary length.  For these reasons,
  % we calculate the MD5 hash of a serialization of the Prolog term.
  md5(Term, Hash),
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
