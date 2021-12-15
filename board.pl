:- use_module(library(lists)).

% create_row(+N, +Piece, -Row)
create_row(0, _, []).
create_row(N, Piece, [Piece|Row]) :-
    N >= 0,
    N1 is N - 1,
    create_row(N1, Piece, Row).
create_row(_, _, _) :-
    throw('Size of row negative!').

% create_pawn_row(-Row)
create_empty_row(Row) :-
    create_row(8, ' ', Row).

% create_pawns_row(+Player, -Row)
create_pawns_row(1, Row) :-
    create_row(8, 'p', Row).
create_pawns_row(2, Row) :-
    create_row(8, 'P', Row).
create_pawns_row(_, _) :-
    throw('Invalid Player!').

% create_pieces_row(+Player, -Row)
create_pieces_row(1, ['r', 'h', 'b', 'q', 'k', 'b', 'h', 'r']).
create_pieces_row(2, ['R', 'H', 'B', 'Q', 'K', 'B', 'H', 'R']).
create_pieces_row(_, _) :-
    throw('Invalid Arguments!').

% initial_board_aux(+N, -Board)
initial_board_aux(8, []).
initial_board_aux(N, [Row|Board]) :-
    (
        N == 0 -> create_pieces_row(2, Row);
        N == 1 -> create_pawns_row(2, Row);
        N == 6 -> create_pawns_row(1, Row);
        N == 7 -> create_pieces_row(1, Row);
        create_empty_row(Row)
    ),
    N1 is N + 1,
    initial_board_aux(N1, Board).

% initial_board(-Board)
initial_board(Board) :-
    initial_board_aux(0, Board).

    