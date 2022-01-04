:-include('game.pl').

test_integer_division(A, B, R) :-
    R is div((B - A), abs(B - A)).

test_create_board(Board) :-
    initial_state((_, _, PlayerPieces, OpponentPieces)),
    create_board(PlayerPieces, OpponentPieces, Board).

test_display_empty_board :-
    empty_board(Board),
    display_board(Board).

test_display_game :-
    initial_state(GameState),
    display_game(GameState).

test_get_piece(PosX, PosY, Piece) :-
    initial_board(Board),
    get_piece(Board, PosX, PosY, Piece).

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
    (Player, LastMove, Check, _, _, Board) = GameState,
    insert_piece_board(Board, 1, 5, 'P', NewBoard),
    move_valid((Player, LastMove, Check, _, _, NewBoard), Move).

test_move_direction_valid(Move) :-
    initial_state(GameState),
    move_direction_valid(GameState, Move).

test_find_all_moves(Moves) :-
    initial_state(GameState),
    %insert_piece_board(Board, 1, 5, 'P', NewBoard),
    findall((DestX, DestY), (between(0, 7, DestX), between(0, 7, DestY), move_piece_valid(GameState, (1, 7, DestX, DestY), 'h')), Moves).

test_update_pieces(PlayerPieces, OpponentPieces) :-
    Board = [
        [e, e, e, b_q,  e,    e, e, e   ],
        [e, e, e, e,    e,    e, e, e   ],
        [e, e, e, w_q,  e,    e, e, b_p1],
        [e, e, e, e,    b_b1, e, e, e   ],
        [e, e, e, e,    e,    e, e, e   ],
        [e, e, e, e,    e,    e, e, e   ],
        [e, e, e, b_b2, e,    e, e, e   ],
        [e, e, e, e,    e,    e, e, e   ]
    ],

    update_pieces(Board, 1, PlayerPieces, OpponentPieces).

test_attacked_pieces(Pieces) :-
    Board = [
        [b_r1, e, e, w_q,  e,    e, e, e   ],
        [e, e, e, b_p4,    b_p3,    e, e, e   ],
        [e, e, e, b_q,  e,    e, e, b_p1],
        [e, e, e, e,    b_b1, e, e, e   ],
        [e, e, e, w_r1,    e,    e, e, e   ],
        [e, e, e, e,    e,    e, e, e   ],
        [e, e, e, b_b2, e,    e, e, e   ],
        [w_r2, e, e, e,    e,    e, e, e   ]
    ],
    update_pieces(Board, 1, PlayerPieces, _),
    attacked_pieces(1, (0, 0, 0, 0), PlayerPieces, Board, Pieces).

test_check :-
    Board = [
        [b_r1, e, e, w_q,  e,    e, e, e   ],
        [e, e, e, b_p4,    b_p3,    e, e, e   ],
        [e, e, e, b_q,  e,    e, e, b_p1],
        [e, e, e, e,    b_b1, e, e, e   ],
        [e, e, e, w_r1,    e,    e, e, e   ],
        [e, e, e, e,    b_k,    e, e, e   ],
        [e, e, e, b_b2, e,    e, e, e   ],
        [w_r2, e, e, e,    e,    e, e, e   ]
    ],
    update_pieces(Board, 1, PlayerPieces, _),
    check(1, (0, 0, 0, 0), PlayerPieces, Board).

test_move_valid_2(Move) :-
    Board = [
        [e, e, e, e, b_r2, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, w_b1, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, b_r1],
        [e, e, e, e, w_k, e, e, e]
    ],
    update_pieces(Board, 1, PlayerPieces, OpponentPieces),
    GameState = (1, (0, 0, 0, 0), PlayerPieces, OpponentPieces, Board),
    move_valid(GameState, Move).

test_piece_valid_moves(Moves) :-
    Board = [
        [e, e, e, e, e,    e, e, e],
        [e, e, e, e, e,    e, e, e],
        [e, e, e, e, e,    e, e, e],
        [e, e, e, e, e,    e, e, e],
        [e, e, e, e, w_h1, e, e, e],
        [e, e, e, e, e,    e, e, e],
        [e, e, e, e, e,    e, e, e],
        [e, e, e, e, e,    e, e, e]
    ],
    piece_valid_moves(1, (0, 0, 0, 0), 4, 4, Board, Moves).

test_find_piece_position(Piece, (PieceX, PieceY)) :-
    initial_state((_, _, PlayerPieces, _, _)),
    find_piece_position(PlayerPieces, Piece, PieceX, PieceY).

test_checkmate :-
    Board = [
        [b_r1, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, b_k, e, e, e, e, e],
        [w_k, e, e, e, e, e, e, e]
    ],
    update_pieces(Board, 1, PlayerPieces, OpponentPieces),
    GameState = (1, (0, 0, 0, 0), PlayerPieces, OpponentPieces, Board),
    checkmate(GameState).

test_stalemate :-
    Board = [
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [e, e, e, e, e, e, e, e],
        [b_k, e, e, e, e, e, e, e],
        [b_p1, e, e, e, e, e, e, e],
        [w_k, e, e, e, e, e, e, e]
    ],
    update_pieces(Board, 1, PlayerPieces, OpponentPieces),
    GameState = (1, (0, 0, 0, 0), PlayerPieces, OpponentPieces, Board),
    stalemate(GameState).

test_promote :-
    Piece = w_p1,
    %pawn(Piece).
    promote(Piece, q),
    queen(Piece).

test_valid_moves(ListOfMoves) :-
    initial_state(GameState),
    valid_moves(GameState, ListOfMoves).