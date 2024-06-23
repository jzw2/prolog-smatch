:- use_module(parse).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

boy1(X) :- triples_from_file("boy1.txt", X).
boy2(X) :- triples_from_file("boy2.txt", X).


var_in_triples(Triples, X) :-
    member(triple(X, _, _), Triples) ;
    member(triple(_, _, variable(X)), Triples).


var_mapping(_, [], []).
var_mapping(Domain, [Y | RangeRest], [X-Y | Rest]) :-
    select(X, Domain, DomainRest),
    var_mapping(DomainRest, RangeRest, Rest).


variables(Triples, Variables) :-
    setof(X, var_in_triples(Triples, X), Variables).

intersection(Left, Right, Amount) :-
    ord_intersection(Left, Right, Intersect, _),
    length(Intersect, Amount).

precision(Left, Right, Score) :-
    intersection(Left, Right, Numerator),
    length(Left, Denominator),
    Score is Numerator / Denominator.

recall(Left, Right, Score) :- precision(Right, Left, Score).

f1(Left, Right, Score) :-
    precision(Left, Right, P),
    recall(Left, Right, R),
    Score is 2 * (P * R) / (P + R).
