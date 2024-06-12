:- set_prolog_flag(double_quotes, chars).
remove_comments([]) --> [].
remove_comments([X | Rest]) --> [X], { X \= '#' }, remove_comments(Rest).
remove_comments(Rest) --> ['#'], comment, remove_comments(Rest).

comment --> ['\n'].
comment --> [X], { X \= '\n' }, comment.



% Helper predicate to process input and output
remove_comments_from_string(Input, Output) :-
    string_codes(Input, Codes),
    phrase(remove_comments(X), Codes, CleanCodes),
    print(X),
    string_codes(Output, CleanCodes).
