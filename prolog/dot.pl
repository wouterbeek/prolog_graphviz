:- module(
  dot,
  [
    dot_arc/3,            % +Out, +FromTerm, +ToTerm
    dot_arc/4,            % +Out, +FromTerm, +ToTerm. +Options
    dot_arc_id/3,         % +Out, +FromId, +ToId
    dot_arc_id/4,         % +Out, +FromId, +ToId. +Options
    dot_cluster/3,        % +Out, +Term, :Goal_1
    dot_cluster/4,        % +Out, +Term, :Goal_1, +Options
    dot_cluster_arc/3,    % +Out, +FromTerm, +ToTerm
    dot_cluster_arc/4,    % +Out, +FromTerm, +ToTerm, +Options
    dot_cluster_arc_id/3, % +Out, +FromTerm, +ToTerm
    dot_cluster_arc_id/4, % +Out, +FromTerm, +ToTerm, +Options
    dot_cluster_id/3,     % +Out, +Id, :Goal_1
    dot_cluster_id/4,     % +Out, +Id, :Goal_1, +Options
    dot_edge/3,           % +Out, +FromTerm, +ToTerm
    dot_edge/4,           % +Out, +FromTerm, +ToTerm. +Options
    dot_edge_id/3,        % +Out, +FromId, +ToId
    dot_edge_id/4,        % +Out, +FromId, +ToId. +Options
    dot_graph/2,          % +Out, :Goal_1
    dot_graph/3,          % +Out, :Goal_1, +Options
    dot_html_replace/2,   % +Unescaped, -Escaped
    dot_id/1,             % -Id
    dot_id/2,             % +Term, -Id
    dot_node/2,           % +Out, +Term
    dot_node/3,           % +Out, +Term, +Options
    dot_node_id/2,        % +Out, +Id
    dot_node_id/3         % +Out, +Id, +Options
  ]
).

/* <module> DOT serialization

@see https://www.graphviz.org

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(debug_ext)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).

:- use_module(dot_html).

:- meta_predicate
    dot_graph(+, 1),
    dot_graph(+, 1, +),
    dot_cluster(+, +, 1),
    dot_cluster(+, +, 1, +),
    dot_cluster_id(+, +, 1),
    dot_cluster_id(+, +, 1, +).





%! dot_arc(+Out:stream, +FromTerm:term, +ToTerm:term) is det.
%! dot_arc(+Out:stream, +FromTerm:term, +ToTerm:term, +Options:dict) is det.
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
  dot_arc(Out, FromTerm, ToTerm, options{}).


dot_arc(Out, FromTerm, ToTerm, Options) :-
  maplist(dot_id, [FromTerm,ToTerm], [FromId,ToId]),
  dot_arc_id(Out, FromId, ToId, Options).



%! dot_arc_id(+Out:stream, +FromId:atom, +ToId:atom) is det.
%! dot_arc_id(+Out:stream, +FromId:atom, +ToId:atom, +Options:dict) is det.
%
% Emits a directed edge or arc from one DOT ID to another in the DOT
% language.
%
% @see dot_arc/[3,4] allows arcs to be asserted between Prolog terms.

dot_arc_id(Out, FromId, ToId) :-
  dot_arc_id(Out, FromId, ToId, options{}).


dot_arc_id(Out, FromId, ToId, Options) :-
  dot_attributes(Options, String),
  format_debug(dot, Out, "    ~a -> ~a~s;", [FromId,ToId,String]).



%! dot_attribute(+Pair:pair(atom,term), -String:string) is semidet.

dot_attribute(Name-Value, String) :-
  dot_attribute_(Name, Value, String).

% HTML-like label
dot_attribute_(html, Spec, Attr) :- !,
  string_phrase(("label=<",dot_html(Spec),">"), Attr).
% multi-line label
dot_attribute_(label, Values, Attr) :-
  is_list(Values), !,
  maplist([Value,Line]>>format(string(Line), "~w", [Value]), Values, Lines1),
  maplist(dot_html_replace, Lines1, Lines2),
  atomics_to_string(Lines2, "<BR/>", Lines3),
  format(string(Attr), "label=<~s>", [Lines3]).
% single-line label
dot_attribute_(label, Value, Attr) :- !,
  dot_attribute_(label, [Value], Attr).
% Another DOT attribute.
dot_attribute_(Key, Value, Attr) :-
  dot_attribute_(Key), !,
  format(string(Attr), "~a=\"~w\"", [Key,Value]).

dot_attribute_(arrowhead).
dot_attribute_(charset).
dot_attribute_(colorscheme).
dot_attribute_(compound).
dot_attribute_(lhead).
dot_attribute_(ltail).
dot_attribute_(shape).



%! dot_attributes(+Options:dict, -String:string) is det.

dot_attributes(options{}, "") :- !.
dot_attributes(Options, String) :-
  dict_pairs(Options, Pairs),
  findall(
    String,
    (
      member(Pair, Pairs),
      dot_attribute(Pair, String)
    ),
    Strings
  ),
  atomics_to_string(Strings, ",", String0),
  format(string(String), " [~s]", [String0]).



%! dot_cluster(+Out:stream, +Term:term, :Goal_1) is det.
%! dot_cluster(+Out:stream, +Term:term, :Goal_1, +Options:dict) is det.

dot_cluster(Out, Term, Goal_1) :-
  dot_cluster(Out, Term, Goal_1, options{label: Term}).


dot_cluster(Out, Term, Goal_1, Options) :-
  dot_id(Term, Id),
  dot_cluster_id(Out, Id, Goal_1, Options).



%! dot_cluster_arc(+Out:stream, +FromTerm:term, +ToTerm:term) is det.
%! dot_cluster_arc(+Out:stream, +FromTerm:term, +ToTerm:term, +Options:dict) is det.

dot_cluster_arc(Out, FromTerm, ToTerm) :-
  dot_cluster_arc(Out, FromTerm, ToTerm, options{}).


dot_cluster_arc(Out, FromTerm, ToTerm, Options) :-
  maplist(dot_id, [FromTerm,ToTerm], [FromId,ToId]),
  dot_cluster_arc_id(Out, FromId, ToId, Options).



%! dot_cluster_arc_id(+Out:stream, +FromId:atom, +ToId:atom) is det.
%! dot_cluster_arc_id(+Out:stream, +FromId:atom, +ToId:atom, +Options:dict) is det.

dot_cluster_arc_id(Out, FromId, ToId) :-
  dot_cluster_arc_id(Out, FromId, ToId, options{}).


dot_cluster_arc_id(Out, FromId0, ToId0, Options0) :-
  maplist(atom_concat(cluster_), [FromId0,ToId0], [LTail,LHead]),
  maplist(atom_concat(dummy_), [FromId0,ToId0], [FromId,ToId]),
  merge_dicts(options{lhead: LHead, ltail: LTail}, Options0, Options),
  dot_arc_id(Out, FromId, ToId, Options).



%! dot_cluster_id(+Out:stream, +Id:atom, :Goal_1) is det.
%! dot_cluster_id(+Out:stream, +Id:atom, :Goal_1, +Options:dict) is det.

dot_cluster_id(Out, Id, Goal_1) :-
  dot_cluster_id(Out, Id, Goal_1, options{}).


dot_cluster_id(Out, Id, Goal_1, Options) :-
  format_debug(dot, Out, "  subgraph cluster_~a {", [Id]),
  dot_attributes(Options, String),
  format_debug(dot, Out, "    graph~s;", [String]),
  format_debug(dot, Out, '    dummy_~a [label="",shape="none"];', [Id]),
  call(Goal_1, Out),
  format_debug(dot, Out, "  }").



%! dot_edge(+Out:stream, +FromTerm:term, +ToTerm:term) is det.
%! dot_edge(+Out:stream, +FromTerm:term, +ToTerm:term, +Options:dict) is det.
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
  dot_edge(Out, FromTerm, ToTerm, options{}).


dot_edge(Out, FromTerm, ToTerm, Options) :-
  maplist(dot_id, [FromTerm,ToTerm], [FromId,ToId]),
  dot_edge_id(Out, FromId, ToId, Options).



%! dot_edge_id(+Out:stream, +FromId:atom, +ToId:atom) is det.
%! dot_edge_id(+Out:stream, +FromId:atom, +ToId:atom, +Options:dict) is det.
%
% Emits an edge between two DOT IDs in the DOT language.
%
% @see dot_edge/[3,4] allows edges to be asserted between Prolog terms.

dot_edge_id(Out, FromId, ToId) :-
  dot_edge_id(Out, FromId, ToId, options{}).


dot_edge_id(Out, FromId, ToId, Options) :-
  dot_attributes(Options, String),
  format_debug(dot, Out, "    ~a -- ~a~s;", [FromId,ToId,String]).



%! dot_graph(+Out:stream, :Goal_1) is det.
%! dot_graph(+Out:stream, :Goal_1, +Options:dict) is det.
%
% @arg Options The following options are supported:
%
%   * directed(+boolean)
%
%     Whether the graph is directed (`true`) or undirected (`false`,
%     default).
%
%   * name(+string)
%
%     The name of the graph.  Default is `"noname"`.
%
%   * overlap(+boolean)
%
%     Whether or not nodes are allowed to overlap.  Default is
%    `false`.
%
%   * strict(+boolean)
%
%     Value `true' indicates that the graph is strict, i.e., has no
%     self-arcs and has not multi-edges.  Default is `false'.
%
%     This can only be used in combination with option
%     `directed(true)', and throws an exception otherwise.

dot_graph(Out, Goal_1) :-
  dot_graph(Out, Goal_1, options{}).


dot_graph(Out, Goal_1, Options0) :-
  % Set default option values.
  merge_dicts(
    Options0,
    options{directed: false, name: "noname", overlap: false, strict: false},
    Options1
  ),
  % Typecheck all option values.
  dict_select(directed, Options1, Options2, Directed),
  must_be(boolean, Directed),
  dict_select(name, Options2, Options3, Name),
  must_be(string, Name),
  dict_select(overlap, Options3, Options4, Overlap),
  must_be(boolean, Overlap),
  dict_select(strict, Options4, Options5, Strict),
  must_be(boolean, Strict),
  % Check for forbidden combinations of option values.
  (   Directed == false,
      Strict == true
  ->  throw(
        error(
          option_combination(directed(Directed),strict(Strict)),
          dot_graph/3
        )
      )
  ;   true
  ),
  dot_graph_type(Directed, Type),
  format_debug(dot, Out, "~a ~s {", [Type,Name]),
  merge_dicts(
    Options5,
    options{charset: 'UTF-8', colorscheme: svg, compound: true},
    Options6
  ),
  dot_attributes(Options6, GraphAttrsString),
  format_debug(dot, Out, "  graph~s;", [GraphAttrsString]),
  call(Goal_1, Out),
  format_debug(dot, Out, "}").

dot_graph_type(false, graph).
dot_graph_type(true, digraph).



%! dot_html_replace(+Unescaped:string, -Escaped:string) is det.
%
% Replaces the following characters that are not allowed to occur in
% DOT HTML labels with HTML elements: left and right angle bracket,
% ampersand.

dot_html_replace(String1, String2) :-
  string_phrase(html_replace, String1, String2).

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
%! dot_node(+Out:stream, +Term:term, +Options:dict) is det.
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
  dot_node(Out, Term, options{label: Term}).


dot_node(Out, Term, Options) :-
  dot_id(Term, Id),
  dot_node_id(Out, Id, Options).



%! dot_node_id(+Out:stream, +Id:atom) is det.
%! dot_node_id(+Out:stream, +Id:atom, +Options:dict) is det.
%
% @see dot_node/[2,3] allows nodes to be asserted for Prolog terms.

dot_node_id(Out, Id) :-
  dot_node_id(Out, Id, options{}).


dot_node_id(Out, Id, Options) :-
  dot_attributes(Options, String),
  format_debug(dot, Out, "    ~a~s;", [Id,String]).
