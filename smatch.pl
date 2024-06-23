:- use_module(parse).
:- use_module(library(lists)).

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
