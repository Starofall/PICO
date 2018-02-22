% #####################################
% # collection of I/O related utility #
% #####################################

% removes all preview files
cleanPreview:-
    delete_directory_contents("./img/preview").


% filter out valid .ppm files from a folder
filterPPMs([],   Output,Acc):- Output = Acc.
filterPPMs([H|T],Output,Acc):-
    name_value(H,_,"ppm"),
    append(Acc,[H],NewAcc),
    filterPPMs(T,Output,NewAcc).
filterPPMs([_|T],Output,Acc):-
    filterPPMs(T,Output,Acc).


% list all ppm files in the input folder
listPPMInputFiles(InputFiles):-
    directory_files("./img/input",RawInputFiles),
    filterPPMs(RawInputFiles,InputFiles,[]).


% read a word from a stream
% stops at newline, blank or end of file
readWord(InStream,W):-
    get_code(InStream,Char),
    readWordFilter(Char,Chars,InStream),
    atom_codes(W,Chars).
% filter used for readWord ignore chars
readWordFilter(10,[],_):-  !. % newline
readWordFilter(32,[],_):-  !. % blank
readWordFilter(-1,[],_):-  !. % eof
readWordFilter(end_of_file,[],_):-  !.
readWordFilter(Char,[Char|Chars],InStream):-
    get_code(InStream,NextChar),
    readWordFilter(NextChar,Chars,InStream).


% reads a word from a stream
% stops at blank and eof - ignores newlines
readWordIgnoreNewLine(InStream,W):-
    get_code(InStream,Char),
    readWordIgnoreNewLineFilter(Char,Chars,InStream),
    atom_codes(W,Chars).
% filter used for readWordIgnoreNewLine ignore chars
readWordIgnoreNewLineFilter(32,[],_):-  !. % blank
readWordIgnoreNewLineFilter(-1,[],_):-  !. % eof
readWordIgnoreNewLineFilter(end_of_file,[],_):-  !.
readWordIgnoreNewLineFilter(10,Chars,InStream):-
    % this hack prevents newlines from stopping our parsing
    get_code(InStream,NextChar),
    readWordFilter(NextChar,Chars,InStream).
readWordIgnoreNewLineFilter(Char,[Char|Chars],InStream):-
    get_code(InStream,NextChar),
    readWordFilter(NextChar,Chars,InStream).


% reads a complete line from a stream
readLine(InStream,W):-
    get_code(InStream,Char),
    readLineFilter(Char,Chars,InStream),
    atom_codes(W,Chars).
% filter used to for readLine for char filtering
readLineFilter(10,[],_):-  !. % newline
readLineFilter(-1,[],_):-  !. % eof
readLineFilter(end_of_file,[],_):-  !.
readLineFilter(Char,[Char|Chars],InStream):-
    get_code(InStream,NextChar),
    readLineFilter(NextChar,Chars,InStream).
