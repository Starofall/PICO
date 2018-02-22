% ######################################
% # writing a userconfig file with     #
% # forward chaining rules in prolog   #
% ######################################


% writes a list of strings to a new config file at a path
writeConfigFile(Path, Rules):-
    setup_call_cleanup(
        (
         open(Path, write, FileStream,[buffer(full),close_on_abort(true)])
         ),
        (!,writeConfigStream(FileStream,Rules),!),
        (close(FileStream))
    ).

writeConfigStream(FileStream, Rules) :- 
    write(FileStream, ":- set_prolog_flag(verbose, silent)."), nl(FileStream),
    write(FileStream, ":- initialization main."), nl(FileStream),
    write(FileStream, ":- ensure_loaded(forward)."), nl(FileStream),
    write(FileStream, ":- op(1200, xfx, --->)."), nl(FileStream), nl(FileStream),
    writeRules(FileStream, Rules), nl(FileStream),
    write(FileStream, "main :- forward, call(f(b)) -> write('Hello').").

writeRules(FileStream, []).
writeRules(FileStream, [Rule|Rules]) :-
    write(FileStream, Rule), nl(FileStream), writeRules(FileStream, Rules).

