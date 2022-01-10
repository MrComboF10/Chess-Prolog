:- use_module(library(lists)).
:- use_module(library('between')).

:- include('utils.pl').

:- dynamic scene/1.
:- dynamic piece_board_scene/4.
:- dynamic player_scene/2.
:- dynamic last_move_scene/2.
:- dynamic pawn_scene/2.
:- dynamic rook_scene/2.
:- dynamic knight_scene/2.
:- dynamic bishop_scene/2.
:- dynamic queen_scene/2.

% player_color(?Player, ?Color)
player_color(1, 'white').
player_color(2, 'black').

% king(?Char)
king(w_k).
king(b_k).

% player_piece(?Player, ?Piece)
player_piece(1, w_p1).
player_piece(1, w_p2).
player_piece(1, w_p3).
player_piece(1, w_p4).
player_piece(1, w_p5).
player_piece(1, w_p6).
player_piece(1, w_p7).
player_piece(1, w_p8).
player_piece(1, w_r1).
player_piece(1, w_r2).
player_piece(1, w_h1).
player_piece(1, w_h2).
player_piece(1, w_b1).
player_piece(1, w_b2).
player_piece(1, w_q).
player_piece(1, w_k).
player_piece(2, b_p1).
player_piece(2, b_p2).
player_piece(2, b_p3).
player_piece(2, b_p4).
player_piece(2, b_p5).
player_piece(2, b_p6).
player_piece(2, b_p7).
player_piece(2, b_p8).
player_piece(2, b_r1).
player_piece(2, b_r2).
player_piece(2, b_h1).
player_piece(2, b_h2).
player_piece(2, b_b1).
player_piece(2, b_b2).
player_piece(2, b_q).
player_piece(2, b_k).

% opponent(?Player, ?Player)
opponent(1, 2).
opponent(2, 1).

initial_scene :-
    % scene(+Scene)
    assert(scene(0)).

initial_pieces :-
    % pawn_scene(?Char, +Scene)
    assert(pawn_scene(w_p1, 0)),
    assert(pawn_scene(w_p2, 0)),
    assert(pawn_scene(w_p3, 0)),
    assert(pawn_scene(w_p4, 0)),
    assert(pawn_scene(w_p5, 0)),
    assert(pawn_scene(w_p6, 0)),
    assert(pawn_scene(w_p7, 0)),
    assert(pawn_scene(w_p8, 0)),
    assert(pawn_scene(b_p1, 0)),
    assert(pawn_scene(b_p2, 0)),
    assert(pawn_scene(b_p3, 0)),
    assert(pawn_scene(b_p4, 0)),
    assert(pawn_scene(b_p5, 0)),
    assert(pawn_scene(b_p6, 0)),
    assert(pawn_scene(b_p7, 0)),
    assert(pawn_scene(b_p8, 0)),
    % rook_scene(?Char, +Scene)
    assert(rook_scene(w_r1, 0)),
    assert(rook_scene(w_r2, 0)),
    assert(rook_scene(b_r1, 0)),
    assert(rook_scene(b_r2, 0)),
    % knight_scene(?Char, +Scene)
    assert(knight_scene(w_h1, 0)),
    assert(knight_scene(w_h2, 0)),
    assert(knight_scene(b_h1, 0)),
    assert(knight_scene(b_h2, 0)),
    % bishop_scene(?Char, +Scene)
    assert(bishop_scene(w_b1, 0)),
    assert(bishop_scene(w_b2, 0)),
    assert(bishop_scene(b_b1, 0)),
    assert(bishop_scene(b_b2, 0)),
    % queen_scene(?Char, +Scene)
    assert(queen_scene(w_q, 0)),
    assert(queen_scene(b_q, 0)).


initial_board :-
    % piece_board_scene(?Piece, ?PieceX, ?PieceY, +Scene)
    % white pawns
    assert(piece_board_scene(w_p1, 0, 6, 0)),
    assert(piece_board_scene(w_p2, 1, 6, 0)),
    assert(piece_board_scene(w_p3, 2, 6, 0)),
    assert(piece_board_scene(w_p4, 3, 6, 0)),
    assert(piece_board_scene(w_p5, 4, 6, 0)),
    assert(piece_board_scene(w_p6, 5, 6, 0)),
    assert(piece_board_scene(w_p7, 6, 6, 0)),
    assert(piece_board_scene(w_p8, 7, 6, 0)),

    % white pieces
    assert(piece_board_scene(w_r1, 0, 7, 0)),
    assert(piece_board_scene(w_h1, 1, 7, 0)),
    assert(piece_board_scene(w_b1, 2, 7, 0)),
    assert(piece_board_scene(w_q, 3, 7, 0)),
    assert(piece_board_scene(w_k, 4, 7, 0)),
    assert(piece_board_scene(w_b2, 5, 7, 0)),
    assert(piece_board_scene(w_h2, 6, 7, 0)),
    assert(piece_board_scene(w_r2, 7, 7, 0)),

    % black pawns
    assert(piece_board_scene(b_p1, 0, 1, 0)),
    assert(piece_board_scene(b_p2, 1, 1, 0)),
    assert(piece_board_scene(b_p3, 2, 1, 0)),
    assert(piece_board_scene(b_p4, 3, 1, 0)),
    assert(piece_board_scene(b_p5, 4, 1, 0)),
    assert(piece_board_scene(b_p6, 5, 1, 0)),
    assert(piece_board_scene(b_p7, 6, 1, 0)),
    assert(piece_board_scene(b_p8, 7, 1, 0)),

    % black pieces
    assert(piece_board_scene(b_r1, 0, 0, 0)),
    assert(piece_board_scene(b_h1, 1, 0, 0)),
    assert(piece_board_scene(b_b1, 2, 0, 0)),
    assert(piece_board_scene(b_q, 3, 0, 0)),
    assert(piece_board_scene(b_k, 4, 0, 0)),
    assert(piece_board_scene(b_b2, 5, 0, 0)),
    assert(piece_board_scene(b_h2, 6, 0, 0)),
    assert(piece_board_scene(b_r2, 7, 0, 0)).

initial_player :-
    % player_scene(?Player, +Scene)
    assert(player_scene(1, 0)).

initial_last_move :-
    % last_move_scene(?Move, +Scene)
    assert(last_move_scene((0, 0, 0, 0), 0)).

% initial_state(-GameState) GameState: (Player, LastMove)
initial_state :-
    initial_scene,
    initial_pieces,
    initial_board,
    initial_player,
    initial_last_move.

% pawn(?Piece)
pawn(Piece) :-
    scene(Scene),
    pawn_scene(Piece, Scene).
% rook(?Piece)
rook(Piece) :-
    scene(Scene),
    rook_scene(Piece, Scene).
% knight(?Piece)
knight(Piece) :-
    scene(Scene),
    knight_scene(Piece, Scene).
% bishop(?Piece)
bishop(Piece) :-
    scene(Scene),
    bishop_scene(Piece, Scene).
% queen(?Piece)
queen(Piece) :-
    scene(Scene),
    queen_scene(Piece, Scene).

% player(?Player)
player(Player) :-
    scene(Scene),
    player_scene(Player, Scene).

% last_move(?LastMove)
last_move(LastMove) :-
    scene(Scene),
    last_move_scene(LastMove, Scene).

% piece_board(?Piece, ?PieceX, ?PieceY)
piece_board(Piece, PieceX, PieceY) :-
    scene(Scene),
    piece_board_scene(Piece, PieceX, PieceY, Scene).

copy_pieces_board_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    piece_board_scene(Piece, PieceX, PieceY, OldScene),
    assert(piece_board_scene(Piece, PieceX, PieceY, Scene)),
    fail;true.

copy_player_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    player_scene(Player, OldScene),
    assert(player_scene(Player, Scene)).

copy_last_move_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    last_move_scene(LastMove, OldScene),
    assert(last_move_scene(LastMove, Scene)).

copy_pawn_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    pawn_scene(Piece, OldScene),
    assert(pawn_scene(Piece, Scene)),
    fail;true.

copy_rook_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    rook_scene(Piece, OldScene),
    assert(rook_scene(Piece, Scene)),
    fail;true.

copy_knight_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    knight_scene(Piece, OldScene),
    assert(knight_scene(Piece, Scene)),
    fail;true.

copy_bishop_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    bishop_scene(Piece, OldScene),
    assert(bishop_scene(Piece, Scene)),
    fail;true.

copy_queen_scene :-
    scene(Scene),
    OldScene is Scene - 1,
    queen_scene(Piece, OldScene),
    assert(queen_scene(Piece, Scene)),
    fail;true.

copy_scene :-
    copy_pieces_board_scene,
    copy_player_scene,
    copy_last_move_scene,
    copy_pawn_scene,
    copy_rook_scene,
    copy_knight_scene,
    copy_bishop_scene,
    copy_queen_scene.

remove_scene :-
    scene(Scene),
    retractall(piece_board_scene(_, _, _, Scene)),
    retract(player_scene(_, Scene)),
    retract(last_move_scene(_, Scene)),
    retractall(pawn_scene(_, Scene)),
    retractall(rook_scene(_, Scene)),
    retractall(knight_scene(_, Scene)),
    retractall(bishop_scene(_, Scene)),
    retractall(queen_scene(_, Scene)).

% change_scene(+NewScene)
change_scene(NewScene) :-
    retract(scene(_)),
    assert(scene(NewScene)).

next_scene :-
    scene(Scene),
    NewScene is Scene + 1,
    change_scene(NewScene).

previous_scene :-
    scene(Scene),
    NewScene is Scene - 1,
    change_scene(NewScene).

next_player :-
    scene(Scene),
    player_scene(Player, Scene),
    opponent(Player, Opponent),
    retract(player_scene(_, Scene)),
    assert(player_scene(Opponent, Scene)).

change_last_move(NewLastMove) :-
    scene(Scene),
    retract(last_move_scene(_, Scene)),
    assert(last_move_scene(NewLastMove, Scene)).

% empty_tile(+TileX, +TileY)
empty_tile(TileX, TileY) :-
    \+ piece_board(_, TileX, TileY).

% remove_piece_board(+PieceX, +PieceY)
remove_piece_board(PieceX, PieceY) :-
    scene(Scene),
    retract(piece_board_scene(_, PieceX, PieceY, Scene)).

% add_piece_board(+Piece, +PieceX, +PieceY)
add_piece_board(Piece, PieceX, PieceY) :-
    scene(Scene),
    (empty_tile(PieceX, PieceY) -> (
        assert(piece_board_scene(Piece, PieceX, PieceY, Scene))    
    ) ; (
        retract(piece_board_scene(_, PieceX, PieceY, Scene)),
        assert(piece_board_scene(Piece, PieceX, PieceY, Scene))
    )).

% piece_graphic(+Piece, -PieceGraphic)
piece_graphic(Piece, 'p') :-
    player_piece(1, Piece),
    pawn(Piece).
piece_graphic(Piece, 'P') :-
    player_piece(2, Piece),
    pawn(Piece).
piece_graphic(Piece, 'r') :-
    player_piece(1, Piece),
    rook(Piece).
piece_graphic(Piece, 'R') :-
    player_piece(2, Piece),
    rook(Piece).
piece_graphic(Piece, 'h') :-
    player_piece(1, Piece),
    knight(Piece).
piece_graphic(Piece, 'H') :-
    player_piece(2, Piece),
    knight(Piece).
piece_graphic(Piece, 'b') :-
    player_piece(1, Piece),
    bishop(Piece).
piece_graphic(Piece, 'B') :-
    player_piece(2, Piece),
    bishop(Piece).
piece_graphic(Piece, 'q') :-
    player_piece(1, Piece),
    queen(Piece).
piece_graphic(Piece, 'Q') :-
    player_piece(2, Piece),
    queen(Piece).
piece_graphic(Piece, 'k') :-
    player_piece(1, Piece),
    king(Piece).
piece_graphic(Piece, 'K') :-
    player_piece(2, Piece),
    king(Piece).

% display_row_aux(+Col, +Row)
display_row_aux(7, Row) :-
    piece_board(Piece, 7, Row),
    piece_graphic(Piece, PieceGraphic),
    write(PieceGraphic), nl.
display_row_aux(7, Row) :-
    empty_tile(7, Row),
    write(' '), nl.
display_row_aux(Col, Row) :-
    Col < 7,
    piece_board(Piece, Col, Row),
    piece_graphic(Piece, PieceGraphic),
    write(PieceGraphic), write(' - '),
    NewCol is Col + 1,
    display_row_aux(NewCol, Row).
display_row_aux(Col, Row) :-
    Col < 7,
    empty_tile(Col, Row),
    write('  - '),
    NewCol is Col + 1,
    display_row_aux(NewCol, Row).

% display_row(+N, +Col, +Row)
display_row(Col, Row) :-
    N is 8 - Row,
    write(N), write('  '),
    display_row_aux(Col, Row).

% display_intermediate_row_aux(+N)
display_intermediate_row_aux(1) :-
    write('|'), nl.
display_intermediate_row_aux(N) :-
    N > 1,
    write('|   '),
    N1 is N - 1,
    display_intermediate_row_aux(N1).

% display_intermediate_row
display_intermediate_row :-
    write('   '),
    display_intermediate_row_aux(8).

% dipslay_letters_row
display_letters_row :-
    write('   '), write('a   b   c   d   e   f   g   h'), nl.

% display_board_aux(+Col, +Row)
display_board_aux(Col, 7) :-
    display_row(Col, 7).
display_board_aux(Col, Row) :-
    Row < 7,
    display_row(Col, Row),
    display_intermediate_row,
    NewRow is Row + 1,
    display_board_aux(Col, NewRow).

display_board :-
    display_board_aux(0, 0), nl,
    display_letters_row, nl.

display_player :-
    player(Player),
    player_color(Player, Color),
    write('Player turn: '), write(Color), nl.

display_game :-
    display_board,
    display_player.

% move(+Move)
move((StartX, StartY, DestX, DestY)) :- % en passant
    valid_en_passant((StartX, StartY, DestX, DestY)),
    next_scene,
    copy_scene,
    piece_board(Piece, StartX, StartY),
    remove_piece_board(StartX, StartY),
    remove_piece_board(DestX, StartY),
    add_piece_board(Piece, DestX, DestY),
    next_player,
    change_last_move((StartX, StartY, DestX, DestY)).
move((StartX, StartY, DestX, DestY)) :-
    next_scene,
    copy_scene,
    piece_board(Piece, StartX, StartY),
    remove_piece_board(StartX, StartY),
    add_piece_board(Piece, DestX, DestY),
    next_player,
    change_last_move((StartX, StartY, DestX, DestY)).

% move_distance(+Move, -Dist)
move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)) :-
    DistX is abs(DestX - StartX),
    DistY is abs(DestY - StartY).

% player_offset_signal(+OffsetUnsigned, -OffsetSigned)
pawn_offset_signed(OffsetUnsigned, OffsetSigned) :-
    player(Player),
    opponent(Player, Opponent),
    OffsetSigned is OffsetUnsigned * (Player - Opponent).

% pawn_row_index(+PawnRowIndex)
pawn_row_index(6) :-
    player(1).
pawn_row_index(1) :-
    player(2).

% coords_valid(+PosX, +PosY)
coords_valid(PosX, PosY) :-
    PosX >= 0,
    PosX =< 7,
    PosY >= 0,
    PosY =< 7.

% move_direction_valid(+Move)
move_direction_valid(Move) :-
    move_distance(Move, (DistX, DistY)),
    DistX =< 1, DistY =< 1.
% vertical
move_direction_valid((PosX, StartY, PosX, DestY)) :-
    StartY \= DestY,
    NewDestY is DestY - div((DestY - StartY), abs(DestY - StartY)),
    empty_tile(PosX, NewDestY),
    move_direction_valid((PosX, StartY, PosX, NewDestY)).
% horizonal
move_direction_valid((StartX, PosY, DestX, PosY)) :-
    StartX \= DestX,
    NewDestX is DestX - div((DestX - StartX), abs(DestX - StartX)),
    empty_tile(NewDestX, PosY),
    move_direction_valid((StartX, PosY, NewDestX, PosY)).
% diagonal
move_direction_valid((StartX, StartY, DestX, DestY)) :-
    StartX \= DestX, StartY \= DestY,
    move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)),
    DistX == DistY,
    NewDestX is DestX - div((DestX - StartX), abs(DestX - StartX)),
    NewDestY is DestY - div((DestY - StartY), abs(DestY - StartY)),
    empty_tile(NewDestX, NewDestY),
    move_direction_valid((StartX, StartY, NewDestX, NewDestY)).

valid_en_passant((StartX, StartY, DestX, DestY)) :-
    piece_board(Piece, StartX, StartY),
    pawn(Piece),
    StartX \= DestX, StartY \= DestY,
    pawn_offset_signed(1, Offset),
    DestY is StartY + Offset, % verify move in y-axis
    move_distance((StartX, StartY, DestX, DestY), (1, 1)), % verify move in x-axis
    last_move((LastX, LastStartY, LastX, LastDestY)),
    move_distance((LastX, LastStartY, LastX, LastDestY), (_, 2)), % verify if the last move of the opponent was two steps
    DestX == LastX, % verify if the capture is towards the opponent piece column
    StartY == LastDestY. % verify if the opponent pawn is next to player pawn in the beggining of movement

% move_piece_valid_aux(+Move, +Piece)
move_piece_valid_aux(Move, Piece) :-
    king(Piece),
    move_distance(Move, (DistX, DistY)),
    DistX =< 1, DistY =< 1.
move_piece_valid_aux(Move, Piece) :-
    knight(Piece),
    move_distance(Move, (DistX, DistY)),
    ((DistX == 1, DistY == 2) ; (DistX == 2, DistY == 1)).
move_piece_valid_aux((StartX, StartY, DestX, DestY), Piece) :-
    rook(Piece),
    (StartX == DestX ; StartY == DestY),
    move_direction_valid((StartX, StartY, DestX, DestY)).
move_piece_valid_aux(Move, Piece) :-
    bishop(Piece),
    move_distance(Move, (DistX, DistY)),
    DistX == DistY,
    move_direction_valid(Move).
move_piece_valid_aux((StartX, StartY, DestX, DestY), Piece) :-
    queen(Piece),
    move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)),
    (StartX == DestX ; StartY == DestY ; DistX == DistY),
    move_direction_valid((StartX, StartY, DestX, DestY)).
move_piece_valid_aux((PosX, StartY, PosX, DestY), Piece) :- % move one step
    pawn(Piece),
    StartY \= DestY,
    pawn_offset_signed(1, Offset),
    DestY is StartY + Offset,
    empty_tile(PosX, DestY). % verify if there is no piece in DestY
move_piece_valid_aux((PosX, StartY, PosX, DestY), Piece) :- % move two steps
    pawn(Piece),
    StartY \= DestY,
    pawn_offset_signed(2, Offset),
    DestY is StartY + Offset, % verify if DestY is two steps
    pawn_row_index(StartY),
    pawn_offset_signed(1, MiddleOffset),
    MiddleY is StartY + MiddleOffset,
    empty_tile(PosX, MiddleY), % verify if there is no piece in first step
    empty_tile(PosX, DestY). % verify if there is no piece in second step
move_piece_valid_aux((StartX, StartY, DestX, DestY), Piece) :- % regular capture
    pawn(Piece),
    StartX \= DestX, StartY \= DestY,
    pawn_offset_signed(1, Offset),
    DestY is StartY + Offset,
    move_distance((StartX, StartY, DestX, DestY), (1, 1)), % verify if move is one step in diagonal left or right
    piece_board(OpponentPiece, DestX, DestY),
    player(Player),
    opponent(Player, Opponent),
    player_piece(Opponent, OpponentPiece). % verify if the piece to capture is the opponent player
move_piece_valid_aux(Move, _) :- % capture en passant
    valid_en_passant(Move).

% move_piece_valid(+Move)
move_piece_valid((StartX, StartY, DestX, DestY)) :-
    piece_board(Piece, StartX, StartY),
    move_piece_valid_aux((StartX, StartY, DestX, DestY), Piece).


% move_valid(+Move)
move_valid((StartX, StartY, DestX, DestY)) :-
    coords_valid(StartX, StartY),
    coords_valid(DestX, DestY),
    \+ (StartX == DestX, StartY == DestY),
    piece_board(Piece, StartX, StartY),
    player(Player),
    player_piece(Player, Piece),
    opponent(Player, Opponent),
    (
        empty_tile(DestX, DestY);
        (
            \+ empty_tile(DestX, DestY),
            piece_board(DestPiece, DestX, DestY),
            player_piece(Opponent, DestPiece)
        )
    ),

    move_piece_valid((StartX, StartY, DestX, DestY)),
    move((StartX, StartY, DestX, DestY)),
    player(NewPlayer),
    (check(NewPlayer) -> (remove_scene, previous_scene, fail) ; (remove_scene, previous_scene)).

all_player_pieces(Player, Pieces) :-
    Goal = (
        between(0, 7, PieceX),
        between(0, 7, PieceY),
        piece_board(Piece, PieceX, PieceY),
        player_piece(Player, Piece)
    ),
    findall((Piece, PieceX, PieceY), Goal, Pieces).

% attacked_pieces_from_piece(+Player, +PieceX, +PieceY, -Pieces)
attacked_pieces_from_piece(Player, PieceX, PieceY, Pieces) :-
    opponent(Player, Opponent),
    Goal = (
        between(0, 7, DestX),
        between(0, 7, DestY),
        ((PieceX \= DestX, PieceY == DestY) ; (PieceX == DestX, PieceY \= DestY) ; (PieceX \= DestX, PieceY \= DestY)),
        move_piece_valid((PieceX, PieceY, DestX, DestY)),
        piece_board(DestPiece, DestX, DestY),
        player_piece(Opponent, DestPiece)
    ),
    findall(DestPiece, Goal, Pieces).

% attacked_pieces_with_dups(+Player, +PlayerPieces, -Pieces)
attacked_pieces_with_dups(_, [], []).
attacked_pieces_with_dups(Player, [(_, PieceX, PieceY)|TPlayerPieces], Pieces) :-
    attacked_pieces_with_dups(Player, TPlayerPieces, PiecesBefore),
    attacked_pieces_from_piece(Player, PieceX, PieceY, PiecesFromPiece),
    append(PiecesBefore, PiecesFromPiece, Pieces).

% attacked_pieces(+Player, -Pieces)
attacked_pieces(Player, Pieces) :-
    all_player_pieces(Player, PlayerPieces),
    attacked_pieces_with_dups(Player, PlayerPieces, DupPieces),
    remove_dups(DupPieces, Pieces).

check(Player) :-
    attacked_pieces(Player, AttackedPieces),
    opponent(Player, Opponent),
    king(King),
    player_piece(Opponent, King),
    member(King, AttackedPieces).

% all_piece_valid_moves(+PieceX, +PieceY, -Moves)
piece_valid_moves(PieceX, PieceY, Moves) :-
    Goal = (
        between(0, 7, DestX),
        between(0, 7, DestY),
        ((PieceX \= DestX, PieceY == DestY) ; (PieceX == DestX, PieceY \= DestY) ; (PieceX \= DestX, PieceY \= DestY)),
        move_valid((PieceX, PieceY, DestX, DestY))
    ),
    findall((PieceX, PieceY, DestX, DestY), Goal, Moves).

% valid_moves_aux(+PlayerPieces, -ListOfMoves)
valid_moves_aux([], []).
valid_moves_aux([(_, PieceX, PieceY)|T], ListOfMoves) :-
    piece_valid_moves(PieceX, PieceY, Moves),
    append(OldListOfMoves, Moves, ListOfMoves),
    valid_moves_aux(T, OldListOfMoves), !.


% valid_moves(-ListOfMoves)
valid_moves(ListOfMoves) :-
    player(Player),
    all_player_pieces(Player, PlayerPieces),
    valid_moves_aux(PlayerPieces, ListOfMoves).


% promote_scene(+Pawn, +PieceTypeToPromote, +Scene)
promote_scene(Pawn, r, Scene) :-
    assert(rook_scene(Pawn, Scene)).
promote_scene(Pawn, h, Scene) :-
    assert(knight_scene(Pawn, Scene)).
promote_scene(Pawn, b, Scene) :-
    assert(bishop_scene(Pawn, Scene)).
promote_scene(Pawn, q, Scene) :-
    assert(queen_scene(Pawn, Scene)).

% promote(+Pawn, +PieceTypeToPromote)
promote(Pawn, PieceTypeToPromote) :-
    scene(Scene),
    retract(pawn_scene(Pawn, Scene)),
    promote_scene(Pawn, PieceTypeToPromote, Scene).

stalemate :-
    valid_moves([]),
    player(Player),
    opponent(Player, Opponent),
    \+ check(Opponent).

checkmate :-
    valid_moves([]),
    player(Player),
    opponent(Player, Opponent),
    check(Opponent).

% game_over(-Winner)
game_over(0) :-
    stalemate.
game_over(Winner) :-
    checkmate,
    player(Player),
    opponent(Player, Winner).

% valid_move_input_atom(+Input)
valid_move_input_atom([LetterCode, NumberCode]) :-
    LetterCode >= 97, LetterCode =< 104, % verify letter
    NumberCode >= 49, NumberCode =< 56. % verify number

% valid_move_input(+AtomInput)
valid_move_input(AtomInput) :-
    atom(AtomInput),
    atom_codes(AtomInput, Input),
    valid_move_input_atom(Input).

% input_to_coords(+Input, -CoordX, -CoordY)
input_to_coords([LetterCode, NumberCode], CoordX, CoordY) :-
    CoordX is LetterCode - 97, % == LowerCode - 'a'
    CoordY is 7 - (NumberCode - 49). % == NumberCode - '1'

% inputs_to_move(+StartInput, +DestInput, -Move)
inputs_to_move(StartInput, DestInput, (StartX, StartY, DestX, DestY)) :-
    input_to_coords(StartInput, StartX, StartY),
    input_to_coords(DestInput, DestX, DestY).

% input_move_position(-Input)
input_move_position(Input) :-
    read(AtomInput),
    valid_move_input(AtomInput),
    atom_codes(AtomInput, Input), !.
input_move_position(Input) :-
    write('Invalid Input! '), write(Input), nl,
    input_move_position(Input).

% input_move(-Move)
input_move(Move) :-
    write('Start? '), nl,
    input_move_position(StartInput),
    write('Dest? '), nl,
    input_move_position(DestInput),
    inputs_to_move(StartInput, DestInput, Move),
    move_valid(Move), !.
input_move(Move) :-
    write('Invalid Move!'), nl,
    input_move(Move).

game_loop :-
    game_over(0),
    write('draw!').
game_loop :-
    game_over(Winner),
    ((Winner == 1) ; (Winner == 2)),
    player_color(Winner, WinnerColor),
    write(WinnerColor), write(' wins!').
game_loop :-
    display_game,
    input_move(Move),
    move(Move),
    game_loop.

play :-
    initial_state,
    game_loop.