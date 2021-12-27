:- use_module(library(lists)).
:- use_module(library('between')).

% player_color(?Player, ?Color)
player_color(1, 'white').
player_color(2, 'black').

% piece_graphic(+Piece, -PieceGraphic)
piece_graphic(w_p1, 'p').
piece_graphic(w_p2, 'p').
piece_graphic(w_p3, 'p').
piece_graphic(w_p4, 'p').
piece_graphic(w_p5, 'p').
piece_graphic(w_p6, 'p').
piece_graphic(w_p7, 'p').
piece_graphic(w_p8, 'p').
piece_graphic(b_p1, 'P').
piece_graphic(b_p2, 'P').
piece_graphic(b_p3, 'P').
piece_graphic(b_p4, 'P').
piece_graphic(b_p5, 'P').
piece_graphic(b_p6, 'P').
piece_graphic(b_p7, 'P').
piece_graphic(b_p8, 'P').
piece_graphic(w_r1, 'r').
piece_graphic(w_r2, 'r').
piece_graphic(b_r1, 'R').
piece_graphic(b_r2, 'R').
piece_graphic(w_h1, 'h').
piece_graphic(w_h2, 'h').
piece_graphic(b_h1, 'H').
piece_graphic(b_h2, 'H').
piece_graphic(w_b1, 'b').
piece_graphic(w_b2, 'b').
piece_graphic(b_b1, 'B').
piece_graphic(b_b2, 'B').
piece_graphic(w_q, 'q').
piece_graphic(b_q, 'Q').
piece_graphic(w_k, 'k').
piece_graphic(b_k, 'K').
piece_graphic(e, ' ').

% pawn(?Char)
pawn(w_p1).
pawn(w_p2).
pawn(w_p3).
pawn(w_p4).
pawn(w_p5).
pawn(w_p6).
pawn(w_p7).
pawn(w_p8).
pawn(b_p1).
pawn(b_p2).
pawn(b_p3).
pawn(b_p4).
pawn(b_p5).
pawn(b_p6).
pawn(b_p7).
pawn(b_p8).
% rook(?Char)
rook(w_r1).
rook(w_r2).
rook(b_r1).
rook(b_r2).
% knight(?Char)
knight(w_h1).
knight(w_h2).
knight(b_h1).
knight(b_h2).
% bishop(?Char)
bishop(w_b1).
bishop(w_b2).
bishop(b_b1).
bishop(b_b2).
% queen(?Char)
queen(w_q).
queen(b_q).
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

% initial_piece_position(?Piece, ?PosX, ?PosY)
initial_piece_position(w_p1, 0, 6).
initial_piece_position(w_p2, 1, 6).
initial_piece_position(w_p3, 2, 6).
initial_piece_position(w_p4, 3, 6).
initial_piece_position(w_p5, 4, 6).
initial_piece_position(w_p6, 5, 6).
initial_piece_position(w_p7, 6, 6).
initial_piece_position(w_p8, 7, 6).

initial_piece_position(w_r1, 0, 7).
initial_piece_position(w_h1, 1, 7).
initial_piece_position(w_b1, 2, 7).
initial_piece_position(w_q, 3, 7).
initial_piece_position(w_k, 4, 7).
initial_piece_position(w_b2, 5, 7).
initial_piece_position(w_h2, 6, 7).
initial_piece_position(w_r2, 7, 7).

initial_piece_position(b_p1, 0, 1).
initial_piece_position(b_p2, 1, 1).
initial_piece_position(b_p3, 2, 1).
initial_piece_position(b_p4, 3, 1).
initial_piece_position(b_p5, 4, 1).
initial_piece_position(b_p6, 5, 1).
initial_piece_position(b_p7, 6, 1).
initial_piece_position(b_p8, 7, 1).

initial_piece_position(b_r1, 0, 0).
initial_piece_position(b_h1, 1, 0).
initial_piece_position(b_b1, 2, 0).
initial_piece_position(b_q, 3, 0).
initial_piece_position(b_k, 4, 0).
initial_piece_position(b_b2, 5, 0).
initial_piece_position(b_h2, 6, 0).
initial_piece_position(b_r2, 7, 0).

initial_player_pieces(Player, PlayerPieces) :-
    Goal = (
        player_piece(Player, Piece),
        initial_piece_position(Piece, PosX, PosY)    
    ),

    findall((Piece, PosX, PosY), Goal, PlayerPieces).


% empty_row_aux(+Count, -Row)
empty_row_aux(0, []).
empty_row_aux(Count, [e|Row]) :-
    Count > 0,
    NewCount is Count - 1,
    empty_row_aux(NewCount, Row).

% empty_row(-Row)
empty_row(Row) :-
    empty_row_aux(8, Row).

% empty_board_aux(+Count, -Board)
empty_board_aux(0, []).
empty_board_aux(Count, [Row|Board]) :-
    Count > 0,
    empty_row(Row),
    NewCount is Count - 1,
    empty_board_aux(NewCount, Board).

% empty_board(-Board)
empty_board(Board) :-
    empty_board_aux(8, Board).

% get_piece(+Board, +PosX, +PosY, -Piece)
get_piece(Board, PosX, PosY, Piece) :-
    nth0(PosY, Board, Row),
    nth0(PosX, Row, Piece).

% insert_piece_row(+Row, +PosX, +Piece, -NewRow)
insert_piece_row([], _, _, []).
insert_piece_row([_|TRow], 0, Piece, [Piece|NewRow]) :-
    insert_piece_row(TRow, -1, Piece, NewRow).
insert_piece_row([HRow|TRow], PosX, Piece, [HRow|NewRow]) :-
    PosX \= 0,
    NewPosX is PosX - 1,
    insert_piece_row(TRow, NewPosX, Piece, NewRow).

% insert_piece_board(+Board, +PosX, +PosY, +Piece, -NewBoard)
insert_piece_board([], _, _, _, []).
insert_piece_board([Row|TBoard], PosX, 0, Piece, [NewRow|NewBoard]) :-
    insert_piece_row(Row, PosX, Piece, NewRow),
    insert_piece_board(TBoard, PosX, -1, Piece, NewBoard).
insert_piece_board([HBoard|TBoard], PosX, PosY, Piece, [HBoard|NewBoard]) :-
    PosY \= 0,
    NewPosY is PosY - 1,
    insert_piece_board(TBoard, PosX, NewPosY, Piece, NewBoard).

% fill_board(+PlayerPieces, +OpponentPieces, +Board, ?NewBoard, -ResBoard)
fill_board_aux([], [], Board, _, Board).
fill_board_aux([], [(OpponentPiece, OX, OY)|TO], Board, NewBoard, ResBoard) :-
    insert_piece_board(Board, OX, OY, OpponentPiece, NewBoard),
    fill_board_aux([], TO, NewBoard, _, ResBoard).
fill_board_aux([(PlayerPiece, PX, PY)|TP], [], Board, NewBoard, ResBoard) :-
    insert_piece_board(Board, PX, PY, PlayerPiece, NewBoard),
    fill_board_aux(TP, [], NewBoard, _, ResBoard).
fill_board_aux([(PlayerPiece, PX, PY)|TP], [(OpponentPiece, OX, OY)|TO], Board, NewBoard, ResBoard) :-
    insert_piece_board(Board, PX, PY, PlayerPiece, AuxBoard),
    insert_piece_board(AuxBoard, OX, OY, OpponentPiece, NewBoard),
    fill_board_aux(TP, TO, NewBoard, _, ResBoard).

% fill_board(+PlayerPieces, +OpponentPieces, +EmptyBoard, -ResBoard)
fill_board(PlayerPieces, OpponentPieces, EmptyBoard, ResBoard) :-
    fill_board_aux(PlayerPieces, OpponentPieces, EmptyBoard, _, ResBoard).

% update_board(+PlayerPieces, +OpponentPieces, -Board)
update_board(PlayerPieces, OpponentPieces, Board) :-
    empty_board(EmptyBoard),
    fill_board(PlayerPieces, OpponentPieces, EmptyBoard, Board).

update_pieces_row([], _, _, _, [], []).
update_pieces_row([Piece|TRow], PosX, PosY, Player, [(Piece, PosX, PosY)|PlayerPieces], OpponentPieces) :-
    PosX > 0,
    player_piece(Player, Piece),
    NewPosX is PosX - 1,
    update_pieces_row(TRow, NewPosX, PosY, Player, PlayerPieces, OpponentPieces).
update_pieces_row([Piece|TRow], PosX, PosY, Player, PlayerPieces, [(Piece, PosX, PosY)|OpponentPieces]) :-
    PosX > 0,
    opponent(Player, Opponent),
    player_piece(Opponent, Piece),
    NewPosX is PosX - 1,
    update_pieces_row(TRow, NewPosX, PosY, Player, PlayerPieces, OpponentPieces).
update_pieces_row([Piece|TRow], PosX, PosY, Player, PlayerPieces, OpponentPieces) :-
    PosX > 0,
    opponent(Player, Opponent),
    \+ player_piece(Player, Piece),
    \+ player_piece(Opponent, Piece),
    NewPosX is PosX - 1,
    update_pieces_row(TRow, NewPosX, PosY, Player, PlayerPieces, OpponentPieces).

update_pieces_board([], _, _, _, [], []).
update_pieces_board([Row|TBoard], PosX, PosY, Player, NewPlayerPieces, NewOpponentPieces) :-
    PosY > 0,
    NewPosY is PosY - 1,
    update_pieces_board(TBoard, PosX, NewPosY, Player, PlayerPieces, OpponentPieces),
    update_pieces_row(Row, PosX, PosY, Player, PlayerPiecesRow, OpponentPiecesRow),
    append(PlayerPieces, PlayerPiecesRow, NewPlayerPieces),
    append(OpponentPieces, OpponentPiecesRow, NewOpponentPieces).

update_pieces(Board, Player, PlayerPieces, OpponentPieces) :-
    update_pieces_board(Board, 8, 8, Player, PlayerPieces, OpponentPieces).

% initial_state(-GameState) GameState: (Player, LastMove, Check, PlayerPieces, OpponentPieces, Board)
initial_state((1, (0, 0, 0, 0), false, PlayerPieces, OpponentPieces, Board)) :-
    initial_player_pieces(1, PlayerPieces),
    initial_player_pieces(2, OpponentPieces),
    update_board(PlayerPieces, OpponentPieces, Board).

% display_row_aux(+Row)
display_row_aux([HRow|[]]) :-
    piece_graphic(HRow, PieceGraphic),
    write(PieceGraphic), nl.
display_row_aux([HRow|TRow]) :-
    piece_graphic(HRow, PieceGraphic),
    write(PieceGraphic), write(' - '),
    display_row_aux(TRow).

% display_row(+N, +Row)
display_row(N, Row) :-
    write(N), write('  '),
    display_row_aux(Row).

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
    write('   '),
    write('a   b   c   d   e   f   g   h'), nl.


% display_board_aux(+N, +Board)
display_board_aux(N, [HBoard|[]]) :-
    display_row(N, HBoard).
display_board_aux(N, [HBoard|TBoard]) :-
    display_row(N, HBoard),
    display_intermediate_row,
    N1 is N - 1,
    display_board_aux(N1, TBoard).

% display_board(+Board)
display_board(Board) :-
    display_board_aux(8, Board), nl,
    display_letters_row, nl.

/*
find_piece([], _, _, _) :- fail.
find_piece([(Piece, PieceX, PieceY)|_], PieceX, PieceY, Piece).
find_piece([(_, PieceX, PieceY)|T], PosX, PosY, NewPiece) :-
    (PosX \= PieceX ; PosY \= PieceY),
    find_piece(T, PosX, PosY, NewPiece).
*/

% display_player(+Player)
display_player(Player) :-
    player_color(Player, Color),
    write('Player turn: '), write(Color), nl.

% display_game(+GameState)
display_game((Player, _, _, _, _, Board)) :-
    display_board(Board),
    display_player(Player).

% move_board(+Move, +Board, -NewBoard)
move_board((StartX, StartY, DestX, DestY), Board, NewBoard) :-
    get_piece(Board, StartX, StartY, Piece),
    insert_piece_board(Board, StartX, StartY, e, AuxBoard),
    insert_piece_board(AuxBoard, DestX, DestY, Piece, NewBoard).

%move_player_pieces((StartX, StartY, DestX, DestY), PlayerPieces, NewPlayerPieces) :-


% move(+GameState, +Move, -NewGameState)
move((Player, _, false, PlayerPieces, OpponentPieces, Board), Move, (NewPlayer, Move, false, NewBoard)) :-
    opponent(Player, NewPlayer),
    move_board(Move, Board, NewBoard).
    % verify check (create attack predicates)

% move_distance(+Move, -Dist)
move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)) :-
    DistX is abs(DestX - StartX),
    DistY is abs(DestY - StartY).

% player_offset_signal(+Player, +OffsetUnsigned, -OffsetSigned)
pawn_offset_signed(Player, OffsetUnsigned, OffsetSigned) :-
    opponent(Player, Opponent),
    OffsetSigned is OffsetUnsigned * (Player - Opponent).

% coords_valid(+PosX, +PosY)
coords_valid(PosX, PosY) :-
    PosX >= 0,
    PosX =< 7,
    PosY >= 0,
    PosY =< 7.

% move_direction_valid(+Board, +Move)
move_direction_valid(_, Move) :-
    move_distance(Move, (DistX, DistY)),
    DistX =< 1, DistY =< 1.
% vertical
move_direction_valid(Board, (PosX, StartY, PosX, DestY)) :-
    NewDestY is DestY - div((DestY - StartY), abs(DestY - StartY)),
    get_piece(Board, PosX, NewDestY, e),
    move_direction_valid(Board, (PosX, StartY, PosX, NewDestY)).
% horizonal
move_direction_valid(Board, (StartX, PosY, DestX, PosY)) :-
    NewDestX is DestX - div((DestX - StartX), abs(DestX - StartX)),
    get_piece(Board, NewDestX, PosY, e),
    move_direction_valid(Board, (StartX, PosY, NewDestX, PosY)).
% diagonal
move_direction_valid(Board, (StartX, StartY, DestX, DestY)) :-
    move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)),
    DistX == DistY,
    NewDestX is DestX - div((DestX - StartX), abs(DestX - StartX)),
    NewDestY is DestY - div((DestY - StartY), abs(DestY - StartY)),
    get_piece(Board, NewDestX, NewDestY, e),
    move_direction_valid(Board, (StartX, StartY, NewDestX, NewDestY)).

% move_piece_valid(+GameState, +Move, +Piece)
move_piece_valid(_, Move, Piece) :-
    king(Piece),
    move_distance(Move, (DistX, DistY)),
    DistX =< 1, DistY =< 1.

move_piece_valid(_, Move, Piece) :-
    knight(Piece),
    move_distance(Move, (DistX, DistY)),
    DistX == 1, DistY == 2.

move_piece_valid((_, _, _, Board), (StartX, StartY, DestX, DestY), Piece) :-
    rook(Piece),
    (StartX == DestX ; StartY == DestY),
    move_direction_valid(Board, (StartX, StartY, DestX, DestY)).

move_piece_valid((_, _, _, Board), Move, Piece) :-
    bishop(Piece),
    move_distance(Move, (DistX, DistY)),
    DistX == DistY,
    move_direction_valid(Board, Move).

move_piece_valid((_, _, _, Board), (StartX, StartY, DestX, DestY), Piece) :-
    queen(Piece),
    move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)),
    (StartX == DestX ; StartY == DestY ; DistX == DistY),
    move_direction_valid(Board, (StartX, StartY, DestX, DestY)).

move_piece_valid((Player, _, _, Board), (PosX, StartY, PosX, DestY), Piece) :- % move one step
    pawn(Piece),
    StartY \= DestY,
    pawn_offset_signed(Player, 1, Offset),
    DestY is StartY + Offset,
    get_piece(Board, PosX, DestY, e). % verify if there is no piece in DestY
move_piece_valid((Player, _, _, Board), (PosX, StartY, PosX, DestY), Piece) :- % move two steps
    pawn(Piece),
    StartY \= DestY,
    pawn_offset_signed(Player, 2, Offset),
    DestY is StartY + Offset, % verify if DestY is two steps
    PlayerPawnsRowIndex is (8 + Offset) mod 8,
    StartY == PlayerPawnsRowIndex,
    pawn_offset_signed(Player, 1, MiddleOffset),
    MiddleY is StartY + MiddleOffset,
    get_piece(Board, PosX, MiddleY, e), % verify if there is no piece in first step
    get_piece(Board, PosX, DestY, e). % verify if there is no piece in second step
move_piece_valid((Player, _, _, Board), (StartX, StartY, DestX, DestY), Piece) :- % regular capture
    pawn(Piece),
    StartX \= DestX, StartY \= DestY,
    pawn_offset_signed(Player, 1, Offset),
    DestY is StartY + Offset,
    move_distance((StartX, StartY, DestX, DestY), (1, 1)), % verify if move is one step in diagonal left or right
    get_piece(Board, DestX, DestY, OpponentPiece),
    opponent(Player, Opponent),
    player_piece(Opponent, OpponentPiece). % verify if the piece to capture is the opponent player
move_piece_valid((Player, (LastX, LastStartY, LastX, LastDestY), _, _), (StartX, StartY, DestX, DestY), Piece) :- % capture en passant
    pawn(Piece),
    StartX \= DestX, StartY \= DestY,
    pawn_offset_signed(Player, 1, Offset),
    DestY is StartY + Offset, % verify move in y-axis
    move_distance((StartX, StartY, DestX, DestY), (1, 1)), % verify move in x-axis
    move_distance((LastX, LastStartY, LastX, LastDestY), (_, 2)), % verify if the last move of the opponent was two steps
    DestX == LastX, % verify if the capture is towards the opponent piece column
    StartY == LastDestY. % verify if the opponent pawn is next to player pawn in the beggining of movement

% move_valid(+GameState, +Move)
move_valid((Player, LastMove, Check, Board), (StartX, StartY, DestX, DestY)) :-
    coords_valid(StartX, StartY),
    coords_valid(DestX, DestY),
    (StartX \= DestX ; StartY \= DestY),
    get_piece(Board, StartX, StartY, Piece),
    player_piece(Player, Piece),
    get_piece(Board, DestX, DestY, DestPiece),
    (
        DestPiece == e;
        (
            player_piece(PlayerPiece, DestPiece),
            PlayerPiece \= Player
        )
    ),
    move_piece_valid((Player, LastMove, Check, Board), (StartX, StartY, DestX, DestY), Piece).

/*check_goal((Player, LastMove, false, Board), OpponentPiece) :-
    opponent(Player, Opponent),
    between(0, 7, StartX),
    between(0, 7, StartY),
    between(0, 7, DestX),
    between(0, 7, DestY),
    (StartX \= DestX ; StartY \= DestY),
    player_piece(Player, Piece),
    move_piece_valid((Player, LastMove, false, Board), (StartX, StartY, DestX, DestY), Piece),
    get_piece(Board, DestX, DestY, OpponentPiece),
    player_piece(Opponent, OpponentPiece).*/

/*check((Player, LastMove, false, Board), Attacks) :-
    opponent(Player, Opponent),
    Goal = (
        between(0, 7, StartX),
        between(0, 7, StartY),
        between(0, 7, DestX),
        between(0, 7, DestY),
        (StartX \= DestX ; StartY \= DestY),
        player_piece(Player, Piece),
        move_piece_valid((Player, LastMove, false, Board), (StartX, StartY, DestX, DestY), Piece),
        get_piece(Board, DestX, DestY, OpponentPiece),
        player_piece(Opponent, OpponentPiece)
    ),
    findall(OpponentPiece, Goal, Attacks).*/