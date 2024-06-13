:- set_prolog_flag(double_quotes, chars).

:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(format)).

a(a) --> [a].
a(b) --> [b].

remove_comments([]) --> [].
remove_comments([X | Rest]) --> [X], { X \= '#' }, remove_comments(Rest).
remove_comments(Rest) --> ['#'], seq(A), {maplist(\=('\n'), A)},  ['\n'], remove_comments(Rest).

star(_, []) --> [].
star(A, [X | Rest]) --> call(A, X), star(A, Rest).


%% https://penman.readthedocs.io/en/latest/notation.html#formal-grammar

symbol([X | Rest]) --> name_char(X), name_char_star(Rest).
role(X) --> [':'], name_char_star(X).
% cannot find where alignement is being used
% alignment() --> ['~'], ((alphanum(X), ('.' | [] ))   | []), digit_plus, comma_digit_star.
string(X) --> ['"'], seq(X), {maplist(\=('\n'), X)}, ['"'].
name_char(X) --> [X], { \+ member(X, " \n\t\r\f\v()/:~")} .
digit(X) --> [X], { char_code('0', Z), char_code('9', N), char_code(X, Xcode), Z =< Xcode, Xcode =< N } .







% node(node(Var, Label, Relations)) --> ['('], variable(V), nodelabel(Label), relations, [')'].
