% ######################################################
% #    PICO - Prolog Image Customisation Optimizer     #
% ######################################################

:- set_prolog_flag(verbose, silent).
:- op(1099, xfx, --->).
:- op(1199, xfx, ::).
:- op(999, fx, *>).
:- op(999, fx, ~>).
:- op(999, fx, *<).
:- op(999, fx, ~<).
:- op(999, fx, *>=).
:- op(999, fx, *=<).
:- op(999, fx, *+).
:- op(998, xfx, *+).
:- dynamic((--->)/2).
:- dynamic((::)/2).
:- dynamic(debug_mode/0).
:- dynamic(current_id/1).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- ensure_loaded(util).
:- ensure_loaded(fileUtil).
:- ensure_loaded(imageUtil).
:- ensure_loaded(imageFilters).
:- ensure_loaded(imageLoader).
:- ensure_loaded(imageWriter).
:- ensure_loaded(thresholds).
:- ensure_loaded(executor).
:- initialization main.

% main entrance into the application
% searches for images and processes them one by one
main :- 
    !,log('---------Program-Start----------'), nl,
    !,listPPMInputFiles(ImageIdList),
    !,process(ImageIdList), nl,
    !,info('finished').

% application error case
main :- info("error").

% Main Image Processing Loop
process([]):- info("Gone through all images").
process([ImageId|ImageIdList]) :-

    % Loading the image into Prologs DB
    !,info('####################'),
    !,info('Loading Image'),
    !,string_concat('./img/input/',ImageId,FilePath),
    !,loadImage(FilePath,ImageId), % dateiname ohne ppm als id -> fooImage.bw.sobel.ppm

    % transforming the image into a list of values
    !,info('Describing Image:'),
    !,describeImage(ImageId),
    !,info('Finished Describing Image'),
    !,info('Relevant Values: '),

    % Extract the feature list from the value list 
    % which is created by finding all imagevalues in the DB
    !,findall(imageValue(ImageId,Key,Value),imageValue(ImageId,Key,Value),ValueList),
    !,extractFeatures(ValueList),
    !,forall(feature(ImageId, Feature),info(feature(ImageId, Feature))),

    % Check whether a Rule already exists for the feature list
    !,(checkRules(ImageId, Filter) ->
        (    
            sinfo('Found Exisiting Rule. Applying'), info(Filter)
        );
        (
            makeNewRule(ImageId, Filter), 
            sinfo('Applying'), sinfo(Filter)
        )
    ), nl, nl,

    info('Current Rule Set:'),
    listing(--->),

    % apply the found filter to current Image
    applyFilter(Filter,ImageId,ResultImageId),
    writeOutput(ResultImageId),
    deleteImage(ImageId),
    deleteImage(ResultImageId),
    !,process(ImageIdList).