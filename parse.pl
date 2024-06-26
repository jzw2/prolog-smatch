% :- module(parse, [triples_from_file/2]).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(pio)).



not(X, Y) --> [Y], {X \= Y}.

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
concept(C) --> constant(C).
relation(relation(R, A, V)) --> [role(R)],
                                (alignment(A)
                                   | ([], {A = none})),
                                (node(V) |
                                    (atom(V) % I'm going to ignore the alignment for now
                                      , ([] | alignment(_))
                                    )) .
relation_star([]) --> [].
relation_star([X | Rest]) --> relation(X), relation_star(Rest) .
atom(atom(X)) --> variable(X), ! | constant(X).
constant(constant(X)) --> [string(X)] | [symbol(X)].
variable(variable(X)) --> [symbol(X)].


symbol(symbol([X | Rest])) --> name_char(X), name_char_star(Rest).
role(role(X)) --> [':'], name_char_star(X).
% cannot find where alignement is being used
% alignment() --> ['~'], ((alphanum(X), (['.'] | [] ))   | []), digit_plus, comma_digit_star.
alignment(alignment(X)) --> ['~'],
                ((alphanum(X), (['.'] | [] ))   | []),
                digit_plus(X),
                comma_digit_star.
string(string(X)) --> ['"'], star(not('"'), X), ['"'].
name_char(X) --> [X], { \+ member(X, " \n\t\r\f\v()/:~\"")} .
name_char_star([X | Rest]) --> name_char(X), !,  name_char_star(Rest).
name_char_star([]) --> [].

digit(X) --> [X], { member(X, "1234567890")} .

comment --> ['#'], star(not('\n'), _), ['\n'].


lex([]) --> [].
lex(X) --> ([' '] | ['\n'] | comment), lex(X).
lex([X | Rest]) --> (string(X) | alignment(X) | role(X) | symbol(X) | [X], { member(X, "/()") }), lex(Rest).


sent1("(w / want-01 :ARG0 (b / boy) :ARG1 (b2 / believe-01 :ARG0 (g / girl) :ARG1 b))").

sent2("(p / person :domain (b / boy) :ARG0-of (w / work-01 :manner (h / hard)))").
sent3(" (b / bear-02 :ARG1 (p / poet :name (n / name :op1 \"William\" :op2 \"Shakespeare\")) :location (c / city :name (n2 / name :op1 \"Stratford-upon-Avon\"))) ").


amr_parse_from_file(File, Tree) :-
    phrase_from_file(lex(Lex), File),
    phrase(star(node, Tree), Lex).



triple_relations(Left, relation(Name, _, RelationTarget), [triple(Left, Name, Right) | Rest]) :-
    (RelationTarget = node(Right, _, _), triples(RelationTarget, Rest)) ;
    (RelationTarget = atom(Right), Rest = []).

triples(node(variable(V), label(Label, _), Relations), [triple(V, instance, Label) | FlattenTriples]) :-
  maplist(triple_relations(V), Relations, Triples),
  foldl(append, Triples, [], FlattenTriples).


triples_from_file(File, Triples) :-
    amr_parse_from_file(File, Tree),
    maplist(triples, Tree, Triples).





% node(node(Var, Label, Relations)) --> ['('], variable(V), nodelabel(Label), relations, [')'].
