:- module(
  gv,
  [
  % EXPORT/VIEW
    gv_export/2,            % +File, :Goal_1
    gv_export/3,            % +File, :Goal_1, +Options
    gv_view/1,              % :Goal_1
    gv_view/2,              % :Goal_1, +Options
  % FORMATS/METHODS
    gv_format/1,            % ?Format
    gv_format_media_type/2, % ?Format, ?MediaType
    gv_format_type/2,       % ?Format, ?Type
    gv_method/1             % ?Method
  ]
).
:- reexport(library(graph/dot)).

/** <module> GraphViz export

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(settings)).

:- use_module(library(call_ext)).

:- discontiguous
    gv_format_synonym__/2,
    gv_format_type__/2,
    gv_format_type__/3.

:- meta_predicate
    gv_export(+, 1),
    gv_export(+, 1, +),
    gv_view(1),
    gv_view(1, +).

:- setting(default_gv_export_format, atom, svg,
           "The default format that is used when exporting a graph using GraphViz.").
:- setting(default_gv_method, atom, dot,
           "The default method that is used when creating a GraphViz visualization.").
:- setting(default_gv_view_format, atom, x11,
           "The default format that is used when viewing a graph using GraphViz.").





% EXPORT/VIEW %

%! gv_export(+File:atom, :Goal_1) is det.
%! gv_export(+File:atom, :Goal_1, +Options:list(compound)) is det.
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

gv_export(File, Goal_1) :-
  gv_export(File, Goal_1, []).


gv_export(File, Goal_1, Options) :-
  gv_export_format_option(File, Format, Type, Options),
  gv_method_option(Method, Options),
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
          dot_graph(ProcIn, Goal_1, Options),
          close(ProcIn)
        ),
        copy_stream_data(ProcOut, Out)
      ),
      close(ProcOut)
    ),
    close(Out)
  ).

%! gv_export_format_option(+File:atom, -Format:atom, -Type:oneof([binary,text]), +Options:list(compound)) is det.

gv_export_format_option(File, Format, Type, Options) :-
  gv_export_format(File, Format, Options),
  (   gv_format_type(Format, Type)
  ->  memberchk(Type, [binary,text])
  ;   type_error(gv_export_format, Format)
  ).

gv_export_format(_, Format, Options) :-
  option(format(Format), Options), !.
gv_export_format(File, Format, _) :-
  file_name_extension(_, Format, File), !.
gv_export_format(_, Format, _) :-
  setting(default_gv_export_format, Format).

gv_method_option(Method, Options) :-
  (   option(method(Method), Options)
  ->  true
  ;   setting(default_gv_method, Method)
  ),
  call_must_be(gv_method, Method).



%! gv_view(:Goal_1) is det.
%! gv_view(:Goal_1, +Options:list(compound)) is det.
%
% Generate a GraphViz graph visualization and open the result in a
% viewer application.
%
% @arg Goal_1 is a unary goal that takes a Prolog output stream that
%      receives DOT formatted messages.
%
% @arg Options is a list that may include any of the options defined
%      for gv_export/3.

gv_view(Goal_1) :-
  gv_view(Goal_1, []).


gv_view(Goal_1, Options) :-
  gv_view_format_option(Format, Options),
  gv_method_option(Method, Options),
  setup_call_cleanup(
    process_create(path(Method), ['-T',Format], [stdin(pipe(ProcIn))]),
    dot_graph(ProcIn, Goal_1, Options),
    close(ProcIn)
  ).

gv_view_format_option(Format, Options) :-
  (   option(format(Format), Options)
  ->  true
  ;   setting(default_gv_view_format, Format)
  ),
  call_must_be(gv_view_format, Format).

gv_view_format(Format) :-
  gv_format_type(Format, viewer).





% FORMATS/METHODS %
%
% GraphViz provides two types of export: binary (e.g., `jpeg`) and
% text (e.g., `svg`).  The third type `viewer` is use to directly open
% GraphViz output in a viewer application (without storing the result
% to a file).

%! gv_format(+Format:atom) is semidet.
%! gv_format(-Format:atom) is nondet.

gv_format(Format) :-
  gv_format_type(Format, _).



%! gv_format_media_type(+Format:atom, +MediaType:compound) is semidet.
%! gv_format_media_type(+Format:atom, -MediaType:compound) is semidet.
%! gv_format_media_type(-Format:atom, +MediaType:compound) is semidet.
%! gv_format_media_type(-Format:atom, -MediaType:compound) is multi.

gv_format_media_type(Format1, MediaType) :-
  ground(Format1), !,
  gv_format_synonym__(Format1, Format2),
  gv_format_type__(Format2, _, MediaType).
gv_format_media_type(Format2, MediaType) :-
  gv_format_type__(Format1, _, MediaType),
  gv_format_synonym__(Format1, Format2).



%! gv_format_type(+Format:atom, +Type:oneof([binary,text,viewer])) is semidet.
%! gv_format_type(+Format:atom, -Type:oneof([binary,text,viewer])) is det.
%! gv_format_type(-Format:atom, +Type:oneof([binary,text,viewer])) is multi.
%! gv_format_type(-Format:atom, -Type:oneof([binary,text,viewer])) is nondet.

gv_format_type(Format1, Type) :-
  gv_format_synonym_(Format1, Format2),
  gv_format_type_(Format2, Type).

gv_format_synonym_(Format1, Format2) :-
  gv_format_synonym__(Format1, Format2).
gv_format_synonym_(Format, Format).

gv_format_type_(Format, Type) :-
  gv_format_type__(Format, Type).
gv_format_type_(Format, Type) :-
  gv_format_type__(Format, Type, _).

gv_format_type__(bmp, binary, media(image/bmp,[])).
% DOT, pretty-printed, no layout.
gv_format_type__(canon, text).
% CGImage, a drawable image object in Core Graphics (the low-level
% procedural drawing API for iOS and Mac OS X).
gv_format_type__(cgimage, binary).
% Client-side imagemap (deprecated).
gv_format_type__(cmap, text).
% Server-side and client-side imagemap.
gv_format_type__(cmapx, text).
% Server-side and client-side imagemap.
gv_format_type__(cmapx_np, text).
% JSON version of `-Txdot' without layout.
gv_format_type__(dot, text, media(text/'vnd.graphviz',[])).
% JSON version of `-Tdot' without layout.
gv_format_type__(dot_json, text, media(application/json,[])).
gv_format_type__(eps, binary, media(image/eps,[])).
gv_format_type__(exr, binary).
gv_format_type__(fig, text).
gv_format_type__(gd, text).
gv_format_type__(gd2, binary).
gv_format_type__(gif, binary, media(image/gif,[])).
gv_format_type__(gtk, viewer).
gv_format_synonym__(gv, dot).
gv_format_type__(ico, binary, media(image/'vnd.microsoft.icon')).
% Server-side and client-side imagemap.
gv_format_type__(imap, text).
gv_format_synonym__(imap_np, imap).
% Server-side and client-side imagemap (deprecated).
gv_format_type__(ismap, text).
gv_format_type__(jp2, binary, media(image/jp2,[])).
gv_format_synonym__(jpe, jpeg).
gv_format_type__(jpeg, binary, media(image/jpeg,[])).
gv_format_synonym__(jpg, jpeg).
% JSON version of `-Tdot'.
gv_format_type__(json, text, media(application/json,[])).
% JSON version of `-Txdot'.
gv_format_type__(json0, text, media(application/json,[])).
gv_format_type__(pct, binary, media(image/'x-pict',[])).
gv_format_type__(pdf, binary, media(application/pdf,[])).
gv_format_type__(pic, text).
gv_format_synonym__(pict, pic).
gv_format_type__(plain, text).
gv_format_synonym__('plain-ext', plain).
gv_format_type__(png, binary, media(image/png,[])).
gv_format_type__(pov, binary).
gv_format_type__(ps, binary, media(application/postscript,[])).
% PostScript output with PDF notations.
gv_format_type__(ps2, binary).
gv_format_type__(psd, binary, media(image/'vnd.adobe.photoshop',[])).
gv_format_type__(sgi, binary, media(image/sgi,[])).
gv_format_type__(svg, text, media(image/'svg+xml',[])).
gv_format_type__(svgz, binary, media(application/gzip,[])).
gv_format_type__(tga, binary, media(image/'x-targa',[])).
gv_format_synonym__(tif, tiff).
gv_format_type__(tiff, binary, media(image,tiff,[])).
gv_format_type__(tk, text).
gv_format_type__(vdx, text).
gv_format_type__(vml, text, media(application,'vnd.openxmlformats-officedocument.vmlDrawing',[])).
gv_format_type__(vmlz, binary).
gv_format_type__(vrml, text, media(model/vrml,[])).
gv_format_type__(wbmp, binary, media(image,'vnd.wap.wbmp',[])).
gv_format_type__(webp, binary, media(image/webp,[])).
gv_format_type__(x11, viewer).
gv_format_type__(xdot, text, media(text/'vnd.graphviz',[])).
gv_format_type__(xdot_json, text, media(application/json,[])).
gv_format_type__('xdot1.2', text, media(text/'vnd.graphviz',[])).
gv_format_type__('xdot1.4', text, media(text/'vnd.graphviz',[])).
gv_format_type__(xlib, viewer).



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
