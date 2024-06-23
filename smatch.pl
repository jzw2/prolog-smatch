:- use_module(parse).


stuff :- triples_from_file("test_input1.txt", X), write(X).
