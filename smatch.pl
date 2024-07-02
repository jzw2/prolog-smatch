:- use_module(parse).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(debug)).
:- use_module(library(format)).
:- use_module(library(dif)).
:- use_module(library(reif)).


var_in_triples(Triples, X) :-
    member(triple(X, _, _), Triples) ;
    member(triple(_, _, variable(X)), Triples).


var_mapping(_, [], []).
var_mapping([], _, []).
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

f1(UnsortedLeft, UnsortedRight, Score) :-
    sort(UnsortedLeft, Left),
    sort(UnsortedRight, Right),
    precision(Left, Right, P),
    recall(Left, Right, R),
    (P = 0.0, R = 0.0 -> Score = 0;
    Score is 2 * (P * R) / (P + R)).


apply_left(triple(X, Mid, Right), X, Y, triple(Y, Mid, Right)).
apply_right(triple(Left, Mid, variable(X)), X, Y, triple(Left, Mid, variable(Y))).
% apply a single triple to a single substiution
apply_single(Mapping, Old, New) :-
    member(X-Y, Mapping),
    (apply_left(Old, X, Y, Mid) ; apply_right(Old, X, Y, Mid)), !, apply_single(Mapping, Mid, New).
apply_single(_, Old, Old). % remember the cut


apply_mapping(Old, Mapping, New) :-
    maplist(apply_single(Mapping), Old, New).


name_space_triple(Name, triple(V1, Middle, constant(V2)), triple(NewV1, Middle, constant(V2))) :-
    NewV1 =.. [Name, V1].
name_space_triple(Name, triple(V1, Middle, variable(V2)), triple(NewV1, Middle, variable(NewV2))) :-
    NewV1 =.. [Name, V1],
    NewV2 =.. [Name, V2].

name_space_triples(Left, Right, NewLeft, NewRight) :-
    maplist(name_space_triple(left), Left, NewLeft),
    maplist(name_space_triple(right), Right, NewRight).

smatch(Left, Right, Score) :-
    name_space_triples(Left, Right, NamedLeft, NamedRight),
    variables(NamedLeft, LeftVariables),
    variables(NamedRight, RightVariables),
    $ var_mapping(LeftVariables, RightVariables,_), !, % make sure its teh right direction
    $ findall(M, var_mapping(LeftVariables, RightVariables, M), Mappings),

    maplist(apply_mapping(NamedLeft), Mappings, AppliedTriples),
    maplist(f1(NamedRight), AppliedTriples, F1s),
    list_max(F1s, Score),
    % debug info
    nth0(Index, F1s, Score),
    nth0(Index, Mappings, Map),
    nth0(Index, AppliedTriples, MappedAnswer),
    length(MappedAnswer, MappedLength),
    length(NamedRight, RightLength),
    format("Mapping ~w,  ~n, which resulted in ~w, ~w ~n", [Map, MappedLength, RightLength]).
smatch(Left, Right, Score) :- smatch(Right, Left, Score) .

compare_files(File1, File2, Scores) :-
    triples_from_file(File1, Triples1),
    triples_from_file(File2, Triples2),
    maplist(smatch, Triples1, Triples2, Scores).

constraint(left, V, SumVar, v(V, SumVar)).
constraint(right, V, SumVar, v(SumVar, V)).

sum_constraints(Direction, OtherVariables, V, constraint(ConstrainedVars =< 1)) :-
    maplist(constraint(Direction, V), OtherVariables, ConstrainedVars).

variables_constraints(LeftVariables, RightVariables, Constraints) :-
    maplist(sum_constraints(left, RightVariables), LeftVariables, ConstraintsA),
    maplist(sum_constraints(right, LeftVariables), RightVariables, ConstraintsB),
    append(ConstraintsA, ConstraintsB, Constraints).



pair(X, Y, X - Y).

cartesian_one_side(Vars, Const, Out) :-
    maplist(pair(Const), Vars, Out).

cartesian(X, Y, XY) :-
     maplist(cartesian_one_side(Y), X, Matrix),
     foldl(append, Matrix, [], XY).



% code kind of spaghetti, but basically, since I wanted to use tfilter
% I have to refeify it ,and I also have to split on the cases where
% constants either match or don't match
same_relation_t(triple(_, R1, X) - triple(_, R2, Y), B) :-
    (X = variable(_) ; X = constant(_)),
    (Y = variable(_) ; Y = constant(_)),
    X =.. [TypeX, ArgX],
    Y =.. [TypeY, ArgY],
    (TypeX = variable, TypeY = variable, InnerX = InnerY ;
     (TypeX = constant, TypeY = constant ; dif(TypeX, TypeY)) , InnerX = ArgX, InnerY= ArgY
     ),
    call((R1 = R2,
          TypeX = TypeY,
          ArgX = ArgY,
          InnerX = InnerY
            ), B).

left_vars_constraint(Pair, constraint([t(Pair)] =< v(X, W))) :-
    Pair = triple(X, _, _) - triple(W, _, _).

right_vars_constraints([], []).
right_vars_constraints([Pair | Rest], [RetPair | RetRest]) :-
    Pair = triple(_, _, variable(Y)) - triple(_, _, variable(Z)),
    RetPair = constraint([t(Pair) =< v(Y, Z)]),
    right_vars_constraints(Rest, RetRest).
right_vars_constraints([Pair | Rest] , Ret) :-
    (Pair = triple(_, _, constant(_)) - _ ;
    Pair = _ - triple(_, _, constant(_))),
    right_vars_constraints(Rest, Ret).


triple_constraints(TriplesA, TriplesB, Constraints) :-
    cartesian(TriplesA, TriplesB, Product),
    tfilter(same_relation_t, Product, SameRelation),
    maplist(left_vars_constraint, SameRelation, LeftConstraints),
    right_vars_constraints(SameRelation, RightConstraints),
    append(LeftConstraints, RightConstraints, Constraints).
