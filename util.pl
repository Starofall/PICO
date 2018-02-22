% ##############################
% # collection of util methods #
% ##############################

% here we can enable different debugging sections in the tool
% if we comment it in, the fact is true, and logging starts
debugEnabled(global).
%debugEnabled(util).
%debugEnabled(imageWriter).
%debugEnabled(imageLoader).
%debugEnabled(imageValues).
%debugEnabled(sobelEdge).
%debugEnabled(imageUtil).

% used to split string into a name and value field, devided by a "."
% this is used to search for all .pmm files in the relevant folder
name_value(String, Name, Value) :-
        sub_string(String, Before, _, After, "."), !,
        sub_string(String, 0, Before, _, Name),
        sub_string(String, _, After, 0, Value).


% different nicely formatted logging functions
logError(Reason) :- ansi_format([fg(red)], 'ERROR: ~w', [Reason]), nl.
log(Reason) :- ansi_format([fg(blue)], '~w', [Reason]), nl.
info(Reason) :- ansi_format([fg(green)], '# ~w', [Reason]), nl.
sinfo(Reason) :- ansi_format([fg(green)], ' ~w', [Reason]).
debug(Reason) :- ansi_format([fg(magenta)], '> ~w', [Reason]), nl.

% advanced logging feature that can be disabled with a global fact
debug(CodeIdentifier,Reason) :-
   % only log if it is enabled in config
   debugEnabled(CodeIdentifier),
   ansi_format([fg(yellow)], '~w |> ~w', [CodeIdentifier,Reason]), nl.
debug(_,_). % ignore it