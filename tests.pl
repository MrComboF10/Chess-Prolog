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

test_insert_piece_board(PosX, PosY, Piece) :-
    initial_board(Board),
    display_board(Board),
    insert_piece_board(Board, PosX, PosY, Piece, NewBoard),
    display_board(NewBoard).

test_insert_piece(PosX, PosY, Piece) :-
    initial_state(GameState),
    display_game(GameState),
    insert_piece(GameState, PosX, PosY, Piece, NewGameState),
    display_game(NewGameState).

test_move(Move) :-
    initial_state(GameState),
    display_game(GameState),
    move(GameState, Move, NewGameState),
    display_game(NewGameState).

test_move_valid(Move) :-
    initial_state(GameState),
    move_valid(GameState, Move).