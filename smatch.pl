:- use_module(parse).


boy1(X) :- triples_from_file("boy1.txt", X).
boy2(X) :- triples_from_file("boy2.txt", X).
