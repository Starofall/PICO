% ##############################
% #    Filter Testing Main     #
% ##############################

:- set_prolog_flag(verbose, silent).
:- initialization main.
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- ensure_loaded(util).
:- ensure_loaded(fileUtil).
:- ensure_loaded(imageUtil).
:- ensure_loaded(imageFilters).
:- ensure_loaded(imageLoader).
:- ensure_loaded(imageWriter).

% this main is used to test all available filters on a given image
main :-
    !,info('####################'),
    !,info('Loading Image'),
    !,ImageId = '01-512.ppm',
    !,string_concat('./img/input/',ImageId,FilePath),
    !,loadImage(FilePath,ImageId),
    !,info('Describing Image'),
    !,describeImage(ImageId),
    !,info('Applying Filters'),
    forall(availableFilter(X),(
        % process every filter and create and write an image for it
        info(X),
        applyFilter(X,ImageId,ResultId),
        writeOutput(ResultId),
        deleteImage(ResultId)
    )),
    !,info("FINISHED"),
    !,halt(0).

% error case
main :-
    logError("Program Failed by False-Error"),nl,
    halt(1).
