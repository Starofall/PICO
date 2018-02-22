:- set_prolog_flag(verbose, silent).
:- initialization main.
:- ensure_loaded(forward).
:- op(1200, xfx, --->).


main :- forward, call(f(b)) -> write('Hello').