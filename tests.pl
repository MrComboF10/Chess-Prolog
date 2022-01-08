:-include('game.pl').

test_integer_division(A, B, R) :-
    R is div((B - A), abs(B - A)).

/*
test_create_board(Board) :-
    initial_state((_, _, PlayerPieces, OpponentPieces)),
    create_board(PlayerPieces, OpponentPieces, Board).

test_display_empty_board :-
    empty_board(Board),
    display_board(Board).
*/
test_display_game :-
    initial_state,
    display_game.

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
    initial_state,
    display_game,
    move(Move),
    display_game.

test_move_valid(Move) :-
    initial_state,
    move_valid(Move).

test_move_direction_valid(Move) :-
    initial_state(GameState),
    move_direction_valid(GameState, Move).

test_find_all_moves(Moves) :-
    initial_state(GameState),
    %insert_piece_board(Board, 1, 5, 'P', NewBoard),
    findall((DestX, DestY), (between(0, 7, DestX), between(0, 7, DestY), move_piece_valid(GameState, (1, 7, DestX, DestY), 'h')), Moves).

test_check :-
    assert(scene(0)),
    assert(player_scene(1, 0)),
    assert(last_move_scene((0, 0, 0, 0), 0)),

    % define pieces
    assert(rook_scene(w_r1, 0)),
    assert(bishop_scene(w_b1, 0)),
    assert(queen_scene(w_q, 0)),

    % build board position
    assert(piece_board_scene(b_k, 4, 4, 0)),
    assert(piece_board_scene(w_r1, 4, 0, 0)),
    assert(piece_board_scene(w_b1, 4, 1, 0)),
    assert(piece_board_scene(w_q, 4, 3, 0)),
    display_game,
    check.

test_piece_valid_moves(Moves) :-
    assert(scene(0)),
    assert(player_scene(1, 0)),
    assert(last_move_scene((0, 0, 0, 0), 0)),

    % define pieces
    assert(knight_scene(w_h1, 0)),

    % build board position
    assert(piece_board_scene(w_h1, 4, 4, 0)),

    piece_valid_moves(4, 4, Moves).

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

test_empty_tile(TileX, TileY) :-
    initial_board,
    empty_tile(TileX, TileY).

test_all_player_pieces(Pieces) :-
    initial_state((Player, _)),
    all_player_pieces(Player, Pieces).

all_pieces_board_scene_aux(Scene, PiecesAcc, Pieces) :-
    piece_board_scene(Piece, PieceX, PieceY, Scene),
    \+ member((Piece, PieceX, PieceY), PiecesAcc),
    all_pieces_board_scene_aux(Scene, [(Piece, PieceX, PieceY)|PiecesAcc], Pieces), !.
all_pieces_board_scene_aux(_, PiecesAcc, PiecesAcc).

all_pieces_board_scene(Scene, Pieces) :-
    all_pieces_board_scene_aux(Scene, [], Pieces).

test_all_pieces_board_scene(Scene, Pieces) :-
    initial_state,
    all_pieces_board_scene(Scene, Pieces).

test_copy_pieces_board_scene :-
    initial_state,
    scene(Scene),
    all_pieces_board_scene(Scene, Pieces),
    write(Pieces), nl,
    next_scene,
    copy_pieces_board_scene,
    scene(NewScene),
    all_pieces_board_scene(NewScene, NewPieces),
    write(NewPieces), nl.

test_remove_pieces_board_scene :-
    initial_state,
    scene(Scene),
    all_pieces_board_scene(Scene, Pieces),
    write(Pieces), nl,
    remove_pieces_board_scene,
    all_pieces_board_scene(Scene, NewPieces),
    write(NewPieces), nl.


create_list(0, []).
create_list(Count, [Count|CountList]) :-
    Count > 0,
    NewCount is Count - 1,
    create_list(NewCount, CountList).