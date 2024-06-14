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
%%
%%

node(node(V, Label, R)) --> ['('], variable(V),
                            (nodelabel(Label)
                               | ([], {Label = none})),
                            relation_star(R), [')'].
nodelabel(label(C, A)) --> ['/'], concept(C), (alignment(A) | ([], {A = none})).
concept(concept(C)) --> constant(C).
relation(relation(R, A, V)) --> role(R),
                                (alignment(A)
                                   | ([], {A = none})),
                                (node(V) |
                                    (atom(V) % I'm going to ignore the alignment for now
                                      , ([] | alignment(_))
                                    )) .
atom(atom(X)) --> variable(X) | constant(X).
constant(constant(X)) --> string(X) | symbol(X).
variable(variable(X)) --> symbol(X).

symbol([X | Rest]) --> name_char(X), name_char_star(Rest).
role(X) --> [':'], name_char_star(X).
% cannot find where alignement is being used
% alignment() --> ['~'], ((alphanum(X), (['.'] | [] ))   | []), digit_plus, comma_digit_star.
alignment(X) --> ['~'],
                ((alphanum(X), (['.'] | [] ))   | []),
                digit_plus(X),
                comma_digit_star.
string(X) --> ['"'], seq(X), {maplist(\=('\n'), X)}, ['"'].
name_char(X) --> [X], { \+ member(X, " \n\t\r\f\v()/:~")} .
name_char_star([]) --> [].
name_char_star([X | Rest]) --> name_char(X), name_char_star(Rest).

digit(X) --> [X], { member(X, "1234567890")} .







% node(node(Var, Label, Relations)) --> ['('], variable(V), nodelabel(Label), relations, [')'].
