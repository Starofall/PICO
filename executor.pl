% #######################################################
% # A collection of predicates used by the Logic System #
% # deciding which Filters are used on a specific image #
% #######################################################
:- op(1099, xfx, --->).

/* For each relevant value a single feature is extracted  */
extractFeatures([]):-  info("Extracted Features:").
extractFeatures([imageValue(ImageId,Key,Value)|ValueList]) :-
    extractSingleFeature(imageValue(ImageId,Key,Value)),
    extractFeatures(ValueList).

/* Each value has a key. For each key there is a threshold 
    rule in thresholds.pl. This predicate searches a rule 
    identified by the Key of the current value, that can be 
    unified with chosen Value. The result is features which 
    are immediately added to the Prolog KDB */
extractSingleFeature(imageValue(ImageId,Key,Value)):-
    call(::, Key, (Threshold ---> Feature)),
    applyThreshold(ImageId, Value, Threshold),
    sinfo(Key), sinfo(': '), sinfo(Value), nl,
    assert(feature(ImageId, Feature)).
extractSingleFeature(imageValue(_, _, _)).

/* These are a couple of operations the threshold rule system is able to 
    interpret.
    The simplest operators compare a threshold with the current value  */
applyThreshold(_, Value,  *<Threshold) :-  Value <  Threshold.
applyThreshold(_, Value, *=<Threshold) :-  Value =< Threshold.
applyThreshold(_, Value,  *>Threshold) :-  Value >  Threshold.
applyThreshold(_, Value, *>=Threshold) :-  Value >= Threshold.

/* These more complex operators can be used to compare the current value
    to another value identified by its Key */
applyThreshold(ImageId, Value,  ~<Key) :- 
     imageValue(ImageId, Key, Threshold),   
     Value < Threshold.
applyThreshold(ImageId, Value,  ~>Key) :- 
     imageValue(ImageId, Key, Threshold),   
     Value > Threshold.

/* Other values can be added to the current value. This can be repeated and
    then the sum can be compared to a threshold */ 
applyThreshold(ImageId, Value, (*+Key, B)) :-
    imageValue(ImageId, Key, AnotherValue), 
    NewValue is Value + AnotherValue,
    applyThreshold(ImageId, NewValue, B).

/* A rule can consist of an arbitrary number of conditions */
applyThreshold(ImageId, Value, (A, B)) :- 
    applyThreshold(ImageId, Value, A),
    applyThreshold(ImageId, Value, B).

/* For each image in a list, we search the Prolog KDB for a Rule with the 
    feature vector of the current image at its body.
    This is done by calling a rule in the style of "Features ---> Filters". 
    If Prolog finds a fitting rule, we can apply the found filters,
    if not a new Rule has to asserted into the database. 
    Then if an image with the same feature vector is called, it will find 
    the exisiting rule, which can be applied. 
*/
checkRules(ImageId,Filter) :-
    call(--->, Features, Filter),
    log(Features), nl,
    verifyList(ImageId, Features).

/* Checks if each element in a list is a feature fact in the KDB for the
    current image id */
verifyList(_ ,[]).
verifyList(ImageId, [H|T]) :-
    feature(ImageId, H), 
    verifyList(ImageId,T).

/* Finds all features for the current image and adds them to a list */        
makeList(ImageId, Result) :- findall(X, (feature(ImageId, X)), Result).

/* Procedure that is applied when no rule is found for the current 
    Featureset */
makeNewRule(ImageId, Filter) :-
    makeList(ImageId, Features),
    selectFilters(Filters),
    createPreviewImage(ImageId,Filters),
    userDecision(ImageId, Filters),
    cleanPreview,
    call(newFilter, Filter),
    assert(Features ---> Filter),
    retract(newFilter(Filter)).

/* Returns 5 filters that can be suggested to the user */
selectFilters(Filters) :-
    suggestRandomFilters(Filters, 5).

/* Selects an arbitrary amount of Filters from the list of available
   filters */
suggestRandomFilters(Filters, Amount) :-
    findall(X, availableFilter(X), FilterSet),
    random_permutation(FilterSet, Mixed),
    append(Filters, _, Mixed),
    length(Filters, Amount).

/* Each selected filter is applied to the current image and saved 
    in the preview folder */
createPreviewImage(_ ,[]).
createPreviewImage(ImageId,[Filter|T]) :-
    applyFilter(Filter,ImageId,ResultImageId),
    writePreview(ResultImageId),
    deleteImage(ResultImageId),
    createPreviewImage(ImageId,T).

/* For each preview filter the user is asked whether they like it.
    If they say yes the recursion is broken, the resulting rule can be asserted
    and the chosen filter can be applied.
    If they say no, they are asked the same for the next filter, until none are left. 
    In that case a rule is created that states that for this specific feature set no
    filter is applied. */
userDecision(_, []) :- assert(newFilter(noFilter)).
userDecision(ImageId, [Filter|Filters]) :-
    nl, nl, sinfo('Do you like'), sinfo(Filter), sinfo('? y/n'), nl,
    get_single_char(C),
    (C == 121  -> 
        info("Thank You! Introducing a new Rule based on your decision."), assert(newFilter(Filter));
        userDecision(ImageId, Filters)
    ).
    
    