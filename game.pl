:- use_module(library(lists)).

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

% next_player(?Player, ?Player)
next_player(1, 2).
next_player(2, 1).

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

initial_state((1, Board)) :-
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
display_game((Player, Board)) :-
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

% insert_piece(+GameState, +PosX, +PosY, +Piece, -NewGameState)
insert_piece((Player, Board), PosX, PosY, Piece, (Player, NewBoard)) :-
    insert_piece_board(Board, PosX, PosY, Piece, NewBoard).

% move(+GameState, +Move, -NewGameState)
move((Player, Board), (StartX, StartY, DestX, DestY), (NewPlayer, NewBoard)) :-
    get_piece(Board, StartX, StartY, Piece),
    next_player(Player, NewPlayer),
    insert_piece_board(Board, StartX, StartY, ' ', AuxBoard),
    insert_piece_board(AuxBoard, DestX, DestY, Piece, NewBoard).

move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)) :-
    DistX is abs(DestX - StartX),
    DistY is abs(DestY - StartY).

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
    move_distance(Move, DistX, DistY),
    DistX =< 1, DistY =< 1.

move_piece_valid(_, Move, Piece) :-
    knight(Piece),
    move_distance(Move, DistX, DistY),
    DistX == 1, DistY == 2.

move_piece_valid((_, Board), (StartX, StartY, DestX, DestY), Piece) :-
    rook(Piece),
    (StartX == DestX ; StartY == DestY),
    move_direction_valid(Board, (StartX, StartY, DestX, DestY)).

move_piece_valid((_, Board), Move, Piece) :-
    bishop(Piece),
    move_distance(Move, (DistX, DistY)),
    DistX == DistY,
    move_direction_valid(Board, Move).

move_piece_valid((_, Board), (StartX, StartY, DestX, DestY), Piece) :-
    queen(Piece),
    move_distance((StartX, StartY, DestX, DestY), (DistX, DistY)),
    (StartX == DestX ; StartY == DestY ; DistX == DistY),
    move_direction_valid(Board, (StartX, StartY, DestX, DestY)).

% pawn player 1
move_piece_valid((_, Board), (PosX, StartY, PosX, DestY), 'p') :-
    DestY is StartY - 1,
    get_piece(Board, PosX, DestY, ' ').
move_piece_valid((_, Board), (PosX, StartY, PosX, DestY), 'p') :-
    DestY is StartY - 2,
    StartY == 6,
    MiddleY is StartY - 1,
    get_piece(Board, PosX, MiddleY, ' '),
    get_piece(Board, PosX, DestY, ' ').
move_piece_valid((_, Board), (StartX, StartY, DestX, DestY), 'p') :-
    DestY is StartY - 1,
    move_distance((StartX, StartY, DestX, DestY), (1, 1)),
    get_piece(Board, DestX, DestY, Piece),
    player_piece(2, Piece).

% pawn player 2
move_piece_valid((_, Board), (PosX, StartY, PosX, DestY), 'P') :-
    DestY is StartY + 1,
    get_piece(Board, PosX, DestY, ' ').
move_piece_valid((_, Board), (PosX, StartY, PosX, DestY), 'P') :-
    DestY is StartY + 2,
    StartY == 1,
    MiddleY is StartY + 1,
    get_piece(Board, PosX, MiddleY, ' '),
    get_piece(Board, PosX, DestY, ' ').
move_piece_valid((_, Board), (StartX, StartY, DestX, DestY), 'P') :-
    DestY is StartY + 1,
    move_distance((StartX, StartY, DestX, DestY), (1, 1)),
    get_piece(Board, DestX, DestY, Piece),
    player_piece(1, Piece).


% move_valid(+GameState, +Move)
move_valid((Player, Board), (StartX, StartY, DestX, DestY)) :-
    coords_valid(StartX, StartY),
    coords_valid(DestX, DestY),
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
    move_piece_valid((Player, Board), (StartX, StartY, DestX, DestY), Piece).