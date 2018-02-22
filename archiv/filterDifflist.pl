
filterGrayscale((Meta,Pixels),(Meta,SWPixels)) :-  filterGrayscaleImp(Pixels,SWPixels).
filterGrayscaleImp(Pixels,Result) :- filterGrayscaleImp(Pixels,Result, X-X).
filterGrayscaleImp([],Result, DiffAcc) :- diffToList(DiffAcc,Result).
filterGrayscaleImp([(R,G,B)|T], Result, DiffAcc) :-
    BW is round(0.21 * R + 0.72 * G + 0.07 * B),
    addToDiffList(DiffAcc, (BW,BW,BW), NewDiffAcc),
    filterGrayscaleImp(T, Result, NewDiffAcc).


filterMultiply((Meta,Pixels),Multiple,(Meta,SWPixels)) :-  filterMultiplyImp(Pixels,Multiple,SWPixels).
filterMultiplyImp(Pixels,Multiple,Result) :- filterMultiplyImp(Pixels,Multiple,Result, X-X).
filterMultiplyImp([],_,Result, DiffAcc) :- diffToList(DiffAcc,Result).
filterMultiplyImp([(R,G,B)|T],Multiple, Result, DiffAcc) :-
    NR is R*Multiple,
    NG is G*Multiple,
    NB is B*Multiple,
    capColor((NR,NG,NB),NewColor),
    addToDiffList(DiffAcc, NewColor, NewDiffAcc),
    filterMultiplyImp(T,Multiple, Result, NewDiffAcc).