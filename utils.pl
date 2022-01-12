to_lower(CharCode, LowerCode) :-
    CharCode >= 65, CharCode =< 90, % is uppercase
    LowerCode is CharCode + 32.
to_lower(CharCode, CharCode) :-
    ((CharCode < 65) ; (CharCode > 90)).

% digit_atom(+Digit, -Atom)
digit_atom(Digit, Atom) :-
    Digit >= 0,
    Digit =< 9,
    DigitCode is 48 + Digit,
    atom_codes(Atom, [DigitCode]).