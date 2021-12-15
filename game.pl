:- use_module(library(lists)).

player_color(1, 'white').
player_color(2, 'black').

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

initial_state((1, Board)) :-
    initial_board(Board).

% display_row(+Row)
display_row([HRow|[]]) :-
    write(HRow), nl.
display_row([HRow|TRow]) :-
    write(HRow), write(' - '),
    display_row(TRow).

% display_intermediate_row_aux(+N)
display_intermediate_row_aux(1) :-
    write('|'), nl.
display_intermediate_row_aux(N) :-
    write('|   '),
    N1 is N - 1,
    display_intermediate_row_aux(N1).

% display_intermediate_row
display_intermediate_row :-
    display_intermediate_row_aux(8).

% display_board(+Board)
display_board([HBoard|[]]) :-
    display_row(HBoard).
display_board([HBoard|TBoard]) :-
    display_row(HBoard),
    display_intermediate_row,
    display_board(TBoard).

display_player(Player) :-
    player_color(Player, Color),
    write('Player turn: '), write(Color), nl.

display_game((Player, Board)) :-
    display_board(Board),
    display_player(Player).
