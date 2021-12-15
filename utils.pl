is_upper_case(C) :-
    char_code(C, Code),
    Code >= 65, Code =< 90.

is_lower_case(C) :-
    char_code(C, Code),
    Code >= 97, Code =< 122.