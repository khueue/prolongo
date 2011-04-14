:- module(bson_format,
    [
        pp/1,
        pp/3
    ]).

/** <module> Pretty-printing for BSON documents.
 */

:- include(misc(common)).

%%  pp(+Doc) is det.
%
%   Pretty-prints the key/value pairs of Doc. Indentation starts
%   at 0 (no indentation), and each level is indented by an extra
%   two spaces.

pp(Doc) :-
    pp(Doc, 0, '  ').

%%  pp(+Doc, +Level, +Tab) is det.
%
%   Pretty-prints the key/value pairs of Doc. Indentation starts
%   at Level, and each level is indented by an extra Tab.

pp(Doc, Level, Tab) :-
    pp_list(Doc, Level, Tab).

pp_list(List, Level, Tab) :-
    write_indent(Level, Tab), write('['), nl,
    Level1 is Level + 1,
    pp_pairs(List, Level1, Tab),
    write_indent(Level, Tab), write(']').

pp_pairs([Key-Value], Level, Tab) :-
    !,
    pp_pair(Key-Value, Level, Tab), nl.
pp_pairs([Key-Value|Pairs], Level, Tab) :-
    !,
    pp_pair(Key-Value, Level, Tab), write(','), nl,
    pp_pairs(Pairs, Level, Tab).
pp_pairs([Value], _Level, _Tab) :-
    !,
    write_value(Value).
pp_pairs([Value|Values], Level, Tab) :-
    write_value(Value), write(','),
    pp_pairs(Values, Level, Tab).

pp_pair(Key-Value, Level, Tab) :-
    Value = [_=_|_],
    !,
    Level1 is Level + 1,
    write_indent(Level, Tab), write_value(Key), write(' - '), nl,
    pp_list(Value, Level1, Tab).
pp_pair(Key-Value, Level, Tab) :-
    write_indent(Level, Tab), write_value(Key), write(' - '), write_value(Value).

write_value(Atom) :-
    core:atom(Atom),
    Atom \== [],
    !,
    write('\''),
    write(Atom),
    write('\'').
write_value(Other) :-
    write(Other).

write_indent(0, _Tab) :- !.
write_indent(N, Tab) :-
    write(Tab),
    N1 is N - 1,
    write_indent(N1, Tab).
