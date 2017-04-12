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
	
% Draws the current state of the board with the list parameter
drawBoard([Pos1,Pos2,Pos3,Pos4,Pos5,Pos6,Pos7,Pos8,Pos9]):-
	nl, write('+---+---+---+'), nl,
	write('| '), drawMove(Pos1),
	write('| '), drawMove(Pos2),
	write('| '), drawMove(Pos3), write('|'), nl,
	write('+---+---+---+'), nl,
	write('| '), drawMove(Pos4),
	write('| '), drawMove(Pos5),
	write('| '), drawMove(Pos6), write('|'), nl,
	write('+---+---+---+'), nl,
	write('| '), drawMove(Pos7),
	write('| '), drawMove(Pos8),
	write('| '), drawMove(Pos9), write('|'), nl,
	write('+---+---+---+'), nl.
	
% If the Pos is 0, no move, print empty space
% The ! prevents the program from printing the move when its not supposed to
drawMove(Pos):-
	Pos = 0, !,
	write('  ').
	
% Else draw the move to the board
drawMove(Pos):-
	write(Pos),
	write(' ').