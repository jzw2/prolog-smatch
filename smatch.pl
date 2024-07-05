:- use_module(parse).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(debug)).
:- use_module(library(format)).
:- use_module(library(dif)).
:- use_module(library(reif)).
:- use_module(library(simplex)).
:- use_module(library(pairs)).


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
    Score is Numerator rdiv Denominator.

recall(Left, Right, Score) :- precision(Right, Left, Score).

f1_formula(0, 0, 0).
f1_formula(P, R, Score) :-
    dif(P, 0), dif(R, 0),
    Score is 2 * (P * R) rdiv (P + R).


f1(UnsortedLeft, UnsortedRight, Score) :-
    sort(UnsortedLeft, Left),
    sort(UnsortedRight, Right),
    precision(Left, Right, P),
    recall(Left, Right, R),
    f1_formula(P, R, Score).


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

smatch_dumb(Left, Right, Score) :-
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
smatch_dumb(Left, Right, Score) :- smatch_dumb(Right, Left, Score) .

compare_files_dumb(File1, File2, Scores) :-
    triples_from_file(File1, Triples1),
    triples_from_file(File2, Triples2),
    maplist(smatch_dumb, Triples1, Triples2, Scores).

constraint_dir(left, V, SumVar, v(V, SumVar)).
constraint_dir(right, V, SumVar, v(SumVar, V)).

sum_constraints(Direction, OtherVariables, V, constraint(ConstrainedVars =< 1)) :-
    maplist(constraint_dir(Direction, V), OtherVariables, ConstrainedVars).

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
          InnerX = InnerY
            ), B).

left_vars_constraint(Pair, constraint([Pair, -1 * v(X, W)] =< 0)) :-
    Pair = triple(X, _, _) - triple(W, _, _).

right_vars_constraints([], []).
right_vars_constraints([Pair | Rest], [RetPair | RetRest]) :-
    Pair = triple(_, _, variable(Y)) - triple(_, _, variable(Z)),
    RetPair = constraint([Pair, -1 * v(Y, Z)] =< 0),
    right_vars_constraints(Rest, RetRest).
right_vars_constraints([Pair | Rest] , Ret) :-
    (Pair = triple(_, _, constant(_)) - triple(_, _, constant(_))),
    right_vars_constraints(Rest, Ret).

triple_constraints(TriplesA, TriplesB, Constraints, SameRelation) :-
    cartesian(TriplesA, TriplesB, Product),
    tfilter(same_relation_t, Product, SameRelation),
    maplist(left_vars_constraint, SameRelation, LeftConstraints),
    right_vars_constraints(SameRelation, RightConstraints),
    append(LeftConstraints, RightConstraints, Constraints).

my_call(Constraint, Old, New) :-
    % write("doing stuff now"), nl,
    call(Constraint, Old, New).

solve_constraints(TriplesA, TriplesB, NumMatches) :-
    variables(TriplesA, VarsA),
    variables(TriplesB, VarsB),
    variables_constraints(VarsA, VarsB, Var),
    triple_constraints(TriplesA, TriplesB, Triple, MaximizeVars),
    append(Triple, Var, All),
    gen_state(S0),
    foldl(call, All, S0, Folded),
    maximize(MaximizeVars, Folded, Max),
    maplist(variable_value(Max), MaximizeVars, SolvedVals),
    sum_list(SolvedVals, NumMatches),
    format("Solved Match: ~w~n", [NumMatches]).

compare_files(FileA, FileB, Score) :-
    triples_from_file(FileA, TriplesA),
    triples_from_file(FileB, TriplesB),
    maplist(solve_constraints, TriplesA, TriplesB, Matches),
    sum_list(Matches, NumMatch),
    maplist(length, TriplesA, Anums),
    sum_list(Anums, NumA),
    maplist(length, TriplesB, Bnums),
    sum_list(Bnums, NumB),
    format("Matches: ~w,~nTriplesA: ~w~nTriplesB:~w~n", [Matches, Anums, Bnums]),
    f1_formula(NumMatch rdiv NumA, NumMatch rdiv NumB, Score).

pair_match(X, Y, X-_, X-Y).
pair_match(X1, _, X-Z, X-Z) :- dif(X1, X).

% finds a mapping that has left side X, and change it to a new Y
remap(Mapping, X, Y, New) :-
    maplist(pair_match(X, Y), Mapping, New).


% first case: move to an unmapped variable
move_unmapped(Mapping, Unmapped, NewMapping) :-
    member(X-_, Mapping),
    member(Var, Unmapped),
    remap(Mapping, X, Var, NewMapping).

% second case: swap a mapping
swap_mapping(Mapping, NewMapping) :-
    member(X1-Y1, Mapping),
    member(X2-Y2, Mapping),
    dif(X1, X2),
    remap(Mapping, X1, Y2, IntermediateMapping),
    remap(IntermediateMapping, X2, Y1, NewMapping).

neighbor_mappings(Unmapped, Mapping, Neighbors) :-
    findall(X, move_unmapped(Mapping, Unmapped, X), Move),
    findall(X, swap_mapping(Mapping, X), Swap),
    append(Move, Swap, Neighbors).

find_unmapped(Mapping, AllVars, Unmapped) :-
    findall(X, (member(X, AllVars), (\+ member(_-X, Mapping))), Unmapped).


num_matches(Triples1, Triples2, Mapping, Matches) :-
    apply_mapping(Triples1, Mapping, Applied),
    sort(Applied, SortApplied),
    sort(Triples2, SortTriples2),
    intersection(SortApplied, SortTriples2, Matches).

next_optimum_mapping(Triples1, Triples2, Mapping, AllVars, NextMapping, Max) :-
    find_unmapped(Mapping, AllVars, Unmapped),
    neighbor_mappings(Unmapped, Mapping, Neighbors),
    map_list_to_pairs(num_matches(Triples1, Triples2), Neighbors, Pairs), % for some reason it maps the result as a key instead of a value
    pairs_keys(Pairs, Matches),
    list_max(Matches, Max),
    member(Max-NextMapping, Pairs).

optimum_search(Triples1, Triples2, CurrentMapping, AllVars, CurrentMax, Max) :-
    next_optimum_mapping(Triples1, Triples2, CurrentMapping, AllVars, NextMapping, NextMax),
    % format("CurrentMax: ~w NextMax: ~w~n", [CurrentMax, NextMax]),
    (CurrentMax >= NextMax, Max = CurrentMax
    ; CurrentMax < NextMax, optimum_search(Triples1, Triples2, NextMapping, AllVars, NextMax, Max)).

% longer if A is strictly longer than B
longer(A, B) :-
    length(A, LA),
    length(B, LB),
    LA > LB.

hill_climb(Left, Right, Max) :-
    longer(Left, Right),
    hill_climb(Right, Left, Max).
hill_climb(Left, Right, Max) :-
    \+ longer(Left, Right),
    name_space_triples(Left, Right, NamedLeft, NamedRight),
    variables(NamedLeft, LeftVariables),
    variables(NamedRight, RightVariables),
    var_mapping(LeftVariables, RightVariables, InitialMapping),
    optimum_search(NamedLeft, NamedRight, InitialMapping, RightVariables, 0, Max).

compare_files_hill(FileA, FileB, Score) :-
    triples_from_file(FileA, TriplesA),
    triples_from_file(FileB, TriplesB),
    maplist(hill_climb, TriplesA, TriplesB, Matches),
    sum_list(Matches, NumMatch),
    maplist(length, TriplesA, Anums),
    sum_list(Anums, NumA),
    maplist(length, TriplesB, Bnums),
    sum_list(Bnums, NumB),
    format("Matches: ~w,~nTriplesA: ~w~nTriplesB:~w~n", [Matches, Anums, Bnums]),
    f1_formula(NumMatch rdiv NumA, NumMatch rdiv NumB, Score).
