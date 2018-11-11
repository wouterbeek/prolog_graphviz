:- module(
  dot_html,
  [
    dot_html//1 % +Spec
  ]
).

/** <module> DOT HTML-like labels

Grammar taken from the GraphViz Web site:

```
cell: <TD> (text* | table | <IMG/>) </TD>
text : string
     | <BR/>
     | <FONT> text* </FONT>
     | <I> text* </I>
     | <B> text* </B>
     | <U> text* </U>
     | <O> text* </O>
     | <SUB> text* </SUB>
     | <SUP> text* </SUP>
     | <S> text* </S>
row: <TR> cell ((<VR/>)? cells)? </TR>
table : <FONT> table </FONT>
      | <TABLE> (row (<HR/>)?)* </TABLE>
```

---

@author Wouter Beek
@see http://www.graphviz.org/content/node-shapes#html
@version 2015-2018
*/

:- use_module(library(error)).

:- use_module(library(dcg)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_html)).





%! dot_html(+Spec:compound)// is det.

% b
dot_html(b(Spec)) --> !,
  html_element(b, [], dot_html:dot_html(Spec)).
% br
% - `ALIGN="CENTER|LEFT|RIGHT"`
dot_html(br) --> !,
  dot_html(br([])).
dot_html(br(Attrs0)) --> !,
  {attributes_(Attrs0, Attrs)},
  html_element(br, Attrs, "").
% cell
% - `ALIGN="CENTER|LEFT|RIGHT|TEXT"`
% - `BALIGN="CENTER|LEFT|RIGHT"`
% - `BGCOLOR="color"`
% - `BORDER="value"`
% - `CELLPADDING="value"`
% - `CELLSPACING="value"`
% - `COLOR="color"`
% - `COLSPAN="value"`
% - `FIXEDSIZE="FALSE|TRUE"`
% - `GRADIENTANGLE="value"`
% - `HEIGHT="value"`
% - `HREF="value"`
% - `ID="value"`
% - `PORT="portName"`
% - `ROWSPAN="value"`
% - `SIDES="value"`
% - `STYLE="value"`
% - `TARGET="value"`
% - `TITLE="value"`
% - `TOOLTIP="value"`
% - `VALIGN="MIDDLE|BOTTOM|TOP"`
% - `WIDTH="value"`
dot_html(cell(Attrs0,Spec)) --> !,
  {attributes_(Attrs0, Attrs)},
  html_element(td, Attrs, dot_html:dot_html(Spec)).
dot_html(cell(Spec)) --> !,
  dot_html(cell([],Spec)).
% font
% - `COLOR="color"`
%   Sets the color of the font within the scope of ‘<FONT>…</FONT>’,
%   or the border color of the table or cell within the scope of
%   ‘<TABLE>…</TABLE>’, or ‘<TD>…</TD>’.  This color can be overridden
%   by a ‘COLOR’ attribute in descendents.  By default, the font color
%   is determined by the ‘fontcolor’ attribute of the corresponding
%   node, edge or graph, and the border color is determined by the
%   color attribute of the corresponding node, edge or graph.
% - `FACE="fontname"`
% - `POINT-SIZE="value"`
dot_html(font(Attrs0,Spec)) --> !,
  {attributes_(Attrs0, Attrs)},
  html_element(font, Attrs, dot_html:dot_html(Spec)).
dot_html(font(Spec)) --> !,
  dot_html(font([],Spec)).
% i
dot_html(i(Spec)) --> !,
  html_element(i, [], dot_html:dot_html(Spec)).
% img
% - `SCALE="FALSE|TRUE|WIDTH|HEIGHT|BOTH"`
% - `SRC="value"`
dot_html(img(Attrs0)) --> !,
  {attributes_(Attrs0, Attrs)},
  html_element(img, Attrs).
% o
dot_html(o(Spec)) --> !,
  html_element(o, [], dot_html:dot_html(Spec)).
dot_html(row_([vr|T])) --> !,
  html_element(vr),
  dot_html(row_(T)).
dot_html(row_([H|T])) --> !,
  dot_html(H),
  dot_html(row_(T)).
dot_html(row_([])) --> !, "".
dot_html(rows_([hr|T])) --> !,
  html_element(hr),
  dot_html(rows_(T)).
dot_html(rows_([H|T])) --> !,
  html_element(tr, [], dot_html(row_(H))),
  dot_html(rows_(T)).
dot_html(rows_([])) --> !, "".
% s
dot_html(s(Spec)) --> !,
  html_element(s, [], dot_html:dot_html(Spec)).
% sub
dot_html(sub(Spec)) --> !,
  html_element(sub, [], dot_html:dot_html(Spec)).
% sup
dot_html(sup(Spec)) --> !,
  html_element(sup, [], dot_html:dot_html(Spec)).
% table
% - `ALIGN="CENTER|LEFT|RIGHT"`
% - `BGCOLOR="color"`
% - `BORDER="value"`
% - `CELLBORDER="value"`
% - `CELLPADDING="value"`
% - `CELLSPACING="value"`
% - `COLOR="color"`
% - `COLUMNS="value"`
% - `FIXEDSIZE="FALSE|TRUE"`
% - `GRADIENTANGLE="value"`
% - `HEIGHT="value"`
% - `HREF="value"`
% - `ID="value"`
% - `PORT="portName"`
% - `ROWS="value"`
% - `SIDES="value"`
% - `STYLE="value"`
% - `TARGET="value"`
% - `TITLE="value"`
% - `TOOLTIP="value"`
% - `VALIGN="MIDDLE|BOTTOM|TOP"`
% - `WIDTH="value"`
dot_html(table(Specs)) --> !,
  dot_html(table([],Specs)).
dot_html(table(Attrs0,Rows)) --> !,
  {attributes_(Attrs0, Attrs)},
  html_element(table, Attrs, dot_html:dot_html(rows_(Rows))).
% u
dot_html(u(Spec)) --> !,
  html_element(u, [], dot_html:dot_html(Spec)).
dot_html(String) -->
  {string(String)}, !,
  atom(String).
% error
dot_html(Spec) -->
  syntax_error(dot_html_like_label(Spec)).

attributes_(Attrs, Attrs) :-
  is_list(Attrs), !.
attributes_(Attr, [Attr]).
