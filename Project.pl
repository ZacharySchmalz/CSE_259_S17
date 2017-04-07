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