% #######################################################################
% # filters that convert a given ImageId and create a new TargetImageId #
% #######################################################################

% list of implemented filters
availableFilter(filterGrayScale).
availableFilter(filterHighKey).
availableFilter(filterLowKey).
availableFilter(filterAddRed).
availableFilter(filterRemoveRed).
availableFilter(filterAddGreen).
availableFilter(filterRemoveGreen).
availableFilter(filterAddBlue).
availableFilter(filterRemoveBlue).
availableFilter(filterSharpenHard).
availableFilter(filterSharpenSoft).
availableFilter(filterBlurHard).
availableFilter(filterBlurSoft).
availableFilter(filterAddContrast).
availableFilter(filterRemoveContrast).
availableFilter(filterAddBrightness).
availableFilter(filterRemoveBrightness).

% list of filters that can be applied and the string id they create
% also contains configurations for the given filter implementations
applyFilter(filterGrayScale,ImageId,TargetImageId):-
    string_concat(ImageId,".gray.ppm",TargetImageId),
    implementedFilterGrayscale(ImageId,TargetImageId).

applyFilter(filterHighKey,ImageId,TargetImageId):-
    string_concat(ImageId,".highkey.ppm",TargetImageId),
    implementedFilterMultiply(ImageId,TargetImageId,2,2,2).

applyFilter(filterLowKey,ImageId,TargetImageId):-
    string_concat(ImageId,".lowkey.ppm",TargetImageId),
    implementedFilterMultiply(ImageId,TargetImageId,0.5,0.5,0.5).

applyFilter(filterAddRed,ImageId,TargetImageId):-
    string_concat(ImageId,".addred.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,50,0,0).

applyFilter(filterRemoveRed,ImageId,TargetImageId):-
    string_concat(ImageId,".removered.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,-50,0,0).

applyFilter(filterAddGreen,ImageId,TargetImageId):-
    string_concat(ImageId,".addgreen.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,0,50,0).

applyFilter(filterRemoveGreen,ImageId,TargetImageId):-
    string_concat(ImageId,".removegreen.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,0,-50,0).

applyFilter(filterAddBlue,ImageId,TargetImageId):-
    string_concat(ImageId,".addblue.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,0,0,50).

applyFilter(filterRemoveBlue,ImageId,TargetImageId):-
    string_concat(ImageId,".removeblue.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,0,0,-50).

applyFilter(filterSharpenHard,ImageId,TargetImageId):-
    string_concat(ImageId,".sharpenhard.ppm",TargetImageId),
    string_concat(ImageId,".sharpenhard.tmp.ppm",TempImageId),
    implementedFilterSharpen(ImageId,TempImageId),
    implementedFilterSharpen(TempImageId,TargetImageId),
    deleteImage(TempImageId).

applyFilter(filterSharpenSoft,ImageId,TargetImageId):-
    string_concat(ImageId,".sharpensoft.ppm",TargetImageId),
    implementedFilterSharpen(ImageId,TargetImageId).

applyFilter(filterBlurHard,ImageId,TargetImageId):-
    string_concat(ImageId,".blurhard.ppm",TargetImageId),
    string_concat(ImageId,".blurhard.tmp.ppm",TempImageId),
    implementedFilterBlur(ImageId,TempImageId),
    implementedFilterBlur(TempImageId,TargetImageId),
    deleteImage(TempImageId).

applyFilter(filterBlurSoft,ImageId,TargetImageId):-
    string_concat(ImageId,".blursoft.ppm",TargetImageId),
    implementedFilterBlur(ImageId,TargetImageId).

applyFilter(filterAddContrast,ImageId,TargetImageId):-
    string_concat(ImageId,".addcontrast.ppm",TargetImageId),
    implementedFilterContrast(ImageId,TargetImageId,1.5).

applyFilter(filterRemoveContrast,ImageId,TargetImageId):-
    string_concat(ImageId,".removecontrast.ppm",TargetImageId),
    implementedFilterContrast(ImageId,TargetImageId,0.5).

applyFilter(filterAddBrightness,ImageId,TargetImageId):-
    string_concat(ImageId,".addbrightness.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,50,50,50).

applyFilter(filterRemoveBrightness,ImageId,TargetImageId):-
    string_concat(ImageId,".removebrightness.ppm",TargetImageId),
    implementedFilterAdd(ImageId,TargetImageId,50,50,50).

applyFilter(noFilter, ImageId, ImageId).

%--------------------


% list of filters that modify images
% the first one is implemented with concurrency.
%    the benefit is very limited, but it gains some % benefit - also its academically interesting
%    1 thread - % 820,081 inferences, 2.875 CPU in 3.133 seconds (92% CPU, 285246 Lips)
%    1 thread - % 820,081 inferences, 2.656 CPU in 2.989 seconds (89% CPU, 308736 Lips)
%    2 thread - % 2,392,991 inferences, 0.594 CPU in 2.781 seconds (21% CPU, 4030301 Lips)
%    2 thread - % 2,392,991 inferences, 0.547 CPU in 2.743 seconds (20% CPU, 4375755 Lips)
implementedFilterGrayscale(ImageId,TargetImageId) :-
    copyImageMetaData(ImageId,TargetImageId),
    findall(imagePixel(ImageId,H,W,R,G,B), (
        imagePixel(ImageId,H,W,R,G,B),
        BW is round(0.21 * R + 0.72 * G + 0.07 * B),
        assert(imagePixel(TargetImageId,H,W,BW,BW,BW))
    ),Goals),
    concurrent(2, Goals, []).

implementedFilterMultiply(ImageId,TargetImageId,RMultiple,GMultiple,BMultiple) :-
    copyImageMetaData(ImageId,TargetImageId),
    forall(imagePixel(ImageId,H,W,R,G,B), (
        NR is R*RMultiple, NG is G*GMultiple, NB is B*BMultiple,
        capColor((NR,NG,NB),(TR,TG,TB)),
        assert(imagePixel(TargetImageId,H,W,TR,TG,TB))
    )).

implementedFilterAdd(ImageId,TargetImageId,RAdd,GAdd,BAdd) :-
    copyImageMetaData(ImageId,TargetImageId),
    forall(imagePixel(ImageId,H,W,R,G,B), (
        NR is R+RAdd, NG is G+GAdd, NB is B+BAdd,
        capColor((NR,NG,NB),(TR,TG,TB)),
        assert(imagePixel(TargetImageId,H,W,TR,TG,TB))
    )).

implementedFilterSharpen(ImageId,TargetImageId) :-
    copyImageMetaData(ImageId,TargetImageId),
    forall(imagePixel(ImageId,H,W,R,G,B),
        implementedFilterSharpenStep(imagePixel(ImageId,H,W,R,G,B),TargetImageId)
    ).
implementedFilterSharpenStep(imagePixel(ImageId,H,W,R,G,B),TargetImageId):-
    pixelKernel(ImageId,H,W,_,_,_,RMT,GMT,BMT,_,_,_,
                RLM,GLM,BLM,RRM,GRM,BRM,_,_,_,RMB,GMB,BMB,_,_,_),
    SharpenedR = (5*R) - RLM - RRM - RMT - RMB,
    SharpenedG = (5*G) - GLM - GRM - GMT - GMB,
    SharpenedB = (5*B) - BLM - BRM - BMT - BMB,
    capColor((SharpenedR,SharpenedG,SharpenedB),(TR,TG,TB)),
    assert(imagePixel(TargetImageId,H,W,TR,TG,TB)).
implementedFilterSharpenStep(imagePixel(_,H,W,R,G,B),TargetImageId):-
    assert(imagePixel(TargetImageId,H,W,R,G,B)).


implementedFilterBlur(ImageId,TargetImageId) :-
    copyImageMetaData(ImageId,TargetImageId),
    forall(imagePixel(ImageId,H,W,R,G,B),
        implementedFilterBlurStep(imagePixel(ImageId,H,W,R,G,B),TargetImageId)
    ).
implementedFilterBlurStep(imagePixel(ImageId,H,W,R,G,B),TargetImageId):-
    pixelKernel(ImageId,H,W,RLT,GLT,BLT,RMT,GMT,BMT,RRT,GRT,BRT,
                RLM,GLM,BLM,RRM,GRM,BRM,RLB,GLB,BLB,RMB,GMB,BMB,RRB,GRB,BRB),
    BlurR = (R + RLT + RMT + RRT + RLM + RRM + RLB + RMB + RRB) / 9,
    BlurG = (G + GLT + GMT + GRT + GLM + GRM + GLB + GMB + GRB) / 9,
    BlurB = (B + BLT + BMT + BRT + BLM + BRM + BLB + BMB + BRB) / 9,
    capColor((BlurR,BlurG,BlurB),(TR,TG,TB)),
    assert(imagePixel(TargetImageId,H,W,TR,TG,TB)).
implementedFilterBlurStep(imagePixel(_,H,W,R,G,B),TargetImageId):-
    assert(imagePixel(TargetImageId,H,W,R,G,B)).

implementedFilterContrast(ImageId,TargetImageId,Amount) :-
    copyImageMetaData(ImageId,TargetImageId),
    forall(imagePixel(ImageId,H,W,R,G,B), (
        NR is (Amount * (R - 128) + 128),
        NG is (Amount * (G - 128) + 128),
        NB is (Amount * (B - 128) + 128),
        capColor((NR,NG,NB),(TR,TG,TB)),
        assert(imagePixel(TargetImageId,H,W,TR,TG,TB))
    )).
