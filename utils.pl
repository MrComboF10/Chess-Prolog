to_lower(CharCode, LowerCode) :-
    CharCode >= 65, CharCode =< 90, % is uppercase
    LowerCode is CharCode + 32.
to_lower(CharCode, CharCode) :-
    ((CharCode < 65) ; (CharCode > 90)).