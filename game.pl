:- use_module(library(lists)).
:- use_module(library('between')).

% player_color(?Player, ?Color)
player_color(1, 'white').
player_color(2, 'black').

% pieces:
% pawn(?Char)
pawn('p').
pawn('P').
% rook(?Char)
rook('r').
rook('R').
% knight(?Char)
knight('h').
knight('H').
% bishop(?Char)
bishop('b').
bishop('B').
% queen(?Char)
queen('q').
queen('Q').
% king(?Char)
king('k').
king('K').

% player_piece(?Player, ?Piece)
player_piece(1, 'p').
player_piece(1, 'r').
player_piece(1, 'h').
player_piece(1, 'b').
player_piece(1, 'q').
player_piece(1, 'k').
player_piece(2, 'P').
player_piece(2, 'R').
player_piece(2, 'H').
player_piece(2, 'B').
player_piece(2, 'Q').
player_piece(2, 'K').

% opponent(?Player, ?Player)
opponent(1, 2).
opponent(2, 1).

% create_row(+N, +Piece, -Row)
create_row(0, _, []).
create_row(N, Piece, [Piece|Row]) :-
    N > 0,
    N1 is N - 1,
    create_row(N1, Piece, Row).

% create_pawn_row(-Row)
create_empty_row(Row) :-
    create_row(8, ' ', Row).

% create_pawns_row(+Player, -Row)
create_pawns_row(1, Row) :-
    create_row(8, 'p', Row).
create_pawns_row(2, Row) :-
    create_row(8, 'P', Row).

% create_pieces_row(+Player, -Row)
create_pieces_row(1, ['r', 'h', 'b', 'q', 'k', 'b', 'h', 'r']).
create_pieces_row(2, ['R', 'H', 'B', 'Q', 'K', 'B', 'H', 'R']).

% initial_board_aux(+N, -Board)
initial_board_aux(8, []).
initial_board_aux(N, [Row|Board]) :-
    N >= 0,
    N < 8,
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

% initial_state(-GameState) GameState: (Player, LastMove, Check, Board)
initial_state((1, (0, 0, 0, 0), false, Board)) :-
    initial_board(Board).

% display_row_aux(+Row)
display_row_aux([HRow|[]]) :-
    write(HRow), nl.
display_row_aux([HRow|TRow]) :-
    write(HRow), write(' - '),
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

% display_player(+Player)
display_player(Player) :-
    player_color(Player, Color),
    write('Player turn: '), write(Color), nl.

% display_game(+GameState)
display_game((Player, _, _, Board)) :-
    display_board(Board),
    display_player(Player).

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

% move(+GameState, +Move, -NewGameState)
move((Player, _, false, Board), (StartX, StartY, DestX, DestY), (NewPlayer, (StartX, StartY, DestX, DestY), false, NewBoard)) :-
    get_piece(Board, StartX, StartY, Piece),
    opponent(Player, NewPlayer),
    insert_piece_board(Board, StartX, StartY, ' ', AuxBoard),
    insert_piece_board(AuxBoard, DestX, DestY, Piece, NewBoard).
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
    get_piece(Board, PosX, NewDestY, ' '),
    move_direction_valid(Board, (PosX, StartY, PosX, NewDestY)).
% horizonal
move_direction_valid(Board, (StartX, PosY, DestX, PosY)) :-
    NewDestX is DestX - div((DestX - StartX), abs(DestX - StartX)),
    get_piece(Board, NewDestX, PosY, ' '),
    move_direction_valid(Board, (StartX, PosY, NewDestX, PosY)).
% diagonal
move_direction_valid(Board, (StartX, StartY, DestX, DestY)) :-
    move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)),
    DistX == DistY,
    NewDestX is DestX - div((DestX - StartX), abs(DestX - StartX)),
    NewDestY is DestY - div((DestY - StartY), abs(DestY - StartY)),
    get_piece(Board, NewDestX, NewDestY, ' '),
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
    get_piece(Board, PosX, DestY, ' '). % verify if there is no piece in DestY
move_piece_valid((Player, _, _, Board), (PosX, StartY, PosX, DestY), Piece) :- % move two steps
    pawn(Piece),
    StartY \= DestY,
    pawn_offset_signed(Player, 2, Offset),
    DestY is StartY + Offset, % verify if DestY is two steps
    PlayerPawnsRowIndex is (8 + Offset) mod 8,
    StartY == PlayerPawnsRowIndex,
    pawn_offset_signed(Player, 1, MiddleOffset),
    MiddleY is StartY + MiddleOffset,
    get_piece(Board, PosX, MiddleY, ' '), % verify if there is no piece in first step
    get_piece(Board, PosX, DestY, ' '). % verify if there is no piece in second step
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
        DestPiece == ' ';
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

check((Player, LastMove, false, Board), Attacks) :-
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
    findall(OpponentPiece, Goal, Attacks).