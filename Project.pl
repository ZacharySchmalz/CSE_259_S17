% Utilized minimax referenced in slides,

%------------------Minimax implementation-------------------------------%
% see class notes -> we need to implement utility, move, min_to_move, max_to_move
minimax(Pos, BestNextPos, Val) :-             % Pos has successors
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-                        % Pos has no successors
    utility(Pos, Val).

best([Pos], Pos, Val) :-                       % There is no more position to compare
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-    % There are other positions to compare
    minimax(Pos1, _, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !.                            % MAX prefers the greater value

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

%---------------Defining utility, move, min_to_move, max_to_move-----------------%
min_to_move([o,_,_]).
max_to_move([x,_,_]).

utility([o,win,_], 10).
utility([x,win,_], -10).
utility([_,tie,_], 0).

% predicate for a draw/tie, the member function checks to see if there are any empty spaces, if not its a draw/tie
tieGame(_,Board):-
	\+ member(0, Board).

move([P1, start_it, Board], [P2, win, NextBoard]) :-
    nextPlayer(P1, P2),
    move_helper(P1, Board, NextBoard),
    win(NextBoard, P1), !.

move([P1, start_it, Board], [P2, tie, NextBoard]) :-
    nextPlayer(P1, P2),
    move_helper(P1, Board, NextBoard),
    tieGame(P1,NextBoard), !.

move([P1, start_it, Board], [P2, start_it, NextBoard]) :-
    nextPlayer(P1, P2),
    move_helper(P1, Board, NextBoard).

move_helper(P, [0|T1], [P|T1]).

move_helper(P, [H1|T1], [H1|T2]) :-
    move_helper(P, T1, T2).

% determine next player
nextPlayer(o, x).
nextPlayer(x, o).

%------Draws the current state of the board with the list parameter-----%
drawBoard([A,B,C,D,E,F,G,H,I]):-
	nl, write('\t+---+---+---+'), nl,
	write('\t| '), drawMove(A),
	write('| '), drawMove(B),
	write('| '), drawMove(C), write('|'), nl,
	write('\t+---+---+---+'), nl,
	write('\t| '), drawMove(D),
	write('| '), drawMove(E),
	write('| '), drawMove(F), write('|'), nl,
	write('\t+---+---+---+'), nl,
	write('\t| '), drawMove(G),
	write('| '), drawMove(H),
	write('| '), drawMove(I), write('|'), nl,
	write('\t+---+---+---+'), nl, nl.

% If the Pos is 0, no move, print empty space
% The ! prevents the program from printing the move when its not supposed to
drawMove(Pos):-
	Pos = 0, !,
	write('  ').

% Else draw the move to the board
drawMove(Pos):-
	write(Pos),
	write(' ').

%----------------------Start the game------------------------------------------------%
% Start game type "playgame."
playgame:- rules, start_it([x, B, start_it], x).

rules :-
	nl, write('Welcome to Tic-Tac-Toe!'),nl,nl,
	write('You (the human) will be \'x\''),nl,
	write('The computer will be \'o\''),nl,nl,
	write('To place your move, type the number location followed by a period'),nl,
	drawBoard([1,2,3,4,5,6,7,8,9]).

%----------------Handle start of game and player turns, also checks for win ---------%
% B: Board, Player: x or o (check for win before allowing next move), AfterCompBoard: Board after Computer plays their move
bestMove(P, NP):- minimax(P, NP, _).

start_it([x, Board, start_it], x):-
	write('Your move:'), nl,
	read(Move),
	(
	place_move(Move, x, Board, NewBoard),
	drawBoard(NewBoard),
		(
			win(NewBoard, x), !, write('You Win!')
			;
			tieGame(_,NewBoard), !, write('Tie Game!')
			;
			start_it([o, NewBoard, start_it], o)
		);
		write('-> Invalid move!'),nl,nl,
    start_it([x, Board, start_it], x)
	).

start_it([o, Board, start_it], o):-
	write('Computer move:'), nl,
	bestMove([o, start_it, Board], [x, State, NewBoard]),
	drawBoard(NewBoard),
	(
		win(NewBoard, o), !, write('Computer Wins!'), nl
		;
		tieGame(_,NewBoard), !, write('Tie Game!'), nl
		;
		start_it([x, NewBoard, start_it], x)
	).


%------------- Moves for x and o's-----------------------%
place_move(1, Player_Token, [X|Ls], [Player_Token|Ls]) :- !, X = 0.

place_move(Position, Player_Token, [X|Ls], [X|L2s]) :-
    number(Position),
    Position1 is Position - 1,
    place_move(Position1, Player_Token, Ls, L2s).

%--------------Winning functions-----------------------%
win(Matrix, Player):- wColumn(Matrix, Player).
win(Matrix, Player):- wRow(Matrix, Player).
win(Matrix, Player):- wDiagnol(Matrix, Player).

% Winning Conditions
% Columns
wColumn(Matrix, Player) :- Matrix = [Player,_,_,Player,_,_,Player,_,_].
wColumn(Matrix, Player) :- Matrix = [_,Player,_,_,Player,_,_,Player,_].
wColumn(Matrix, Player) :- Matrix = [_,_,Player,_,_,Player,_,_,Player].

% Rows
wRow(Matrix, Player) :- Matrix = [Player,Player,Player,_,_,_,_,_,_].
wRow(Matrix, Player) :- Matrix = [_,_,_,Player,Player,Player,_,_,_].
wRow(Matrix, Player) :- Matrix = [_,_,_,_,_,_,Player,Player,Player].

% Diagnol
wDiagnol(Matrix, Player) :- Matrix = [Player,_,_,_,Player,_,_,_,Player].
wDiagnol(Matrix, Player) :- Matrix = [_,_,Player,_,Player,_,Player,_,_].
