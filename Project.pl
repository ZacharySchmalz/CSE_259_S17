% Example Query ?- browse(user).
browse(File):-
	seeing(Old),
	see(File),
	repeat,
	read(Data),
	process(Data),
	seen,
	see(Old).

process(end_of_file):- !.

process(Data):-
	write(Data),
	nl,
	fail.

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

move([X1, start_it, Board], [X2, win, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    win(NextBoard, X1), !.

move([X1, start_it, Board], [X2, tie, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard),
    tieGame(X1,NextBoard), !.

move([X1, start_it, Board], [X2, start_it, NextBoard]) :-
    nextPlayer(X1, X2),
    move_aux(X1, Board, NextBoard).

move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

% determine next player
nextPlayer(o, x).
nextPlayer(x, o).
%------Draws the current state of the board with the list parameter-----%
drawBoard([A,B,C,D,E,F,G,H,I]):-

	nl, write('+---+---+---+'), nl,
	write('| '), drawMove(A),
	write('| '), drawMove(B),
	write('| '), drawMove(C), write('|'), nl,
	write('+---+---+---+'), nl,
	write('| '), drawMove(D),
	write('| '), drawMove(E),
	write('| '), drawMove(F), write('|'), nl,
	write('+---+---+---+'), nl,
	write('| '), drawMove(G),
	write('| '), drawMove(H),
	write('| '), drawMove(I), write('|'), nl,
	write('+---+---+---+'), nl, nl.

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
% Start game type "playgame"
playgame :- rules, start_it([x, B, start_it], x).
rules :-
	write('To play this game enter the integer associated'),nl,
	write('with the place you would like to occupy by typing'),nl,
	write('the integer associated with your desired move'),nl,
	write('location followed by a period. (Your are x\'s computer is o\'s)'),nl,
	drawBoard([1,2,3,4,5,6,7,8,9]).

%----------------Handle start of game and player turns, also checks for win ---------%
% B: Board, Player: x or o (check for win before allowing next move), AfterCompBoard: Board after Computer plays their move
bestMove(P, NP):- minimax(P, NP, _).

start_it([x, Board, start_it], x):-
	write('Pick your next move'), nl,
	read(Move),nl,
	(
	place_move(Move, x, Board, NewBoard),
	drawBoard(NewBoard),
		(
			win(NewBoard, x), !, write('You are a winner')
			;
			tieGame(_,NewBoard), !, write('Tie game')
			;
			start_it([o, NewBoard, start_it], o)
		);
		write('-> Bad Move !'), nl,
    start_it([x, Board, start_it], x)
	).

start_it([o, Board, start_it], o):-
	write('Computers turn!'), nl,
	bestMove([o, start_it, Board], [x, State, NewBoard]),
	drawBoard(NewBoard),
	(
		win(NewBoard, o), !, write('Computer is the winner'), nl
		;
		tieGame(_,NewBoard), !, write('Tie game'), nl
		;
		start_it([x, NewBoard, start_it], x)
	).


%------------- Moves for x and o's-----------------------%
place_move(1, Player_Token, [X|Ls], [Player_Token|Ls]) :- !, X = 0.

place_move(Position, Player_Token, [X|Ls], [X|L2s]) :-
    number(Position),
    Position1 is Position - 1,
    set1(Position1, Player_Token, Ls, L2s).


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
