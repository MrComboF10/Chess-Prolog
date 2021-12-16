:-include('game.pl').

test_display_board :-
    initial_board(Board),
    display_board(Board).

test_display_game :-
    initial_board(Board),
    display_game((1, Board)).

test_get_piece(PosX, PosY, Piece) :-
    initial_state(GameState),
    get_piece(GameState, PosX, PosY, Piece).