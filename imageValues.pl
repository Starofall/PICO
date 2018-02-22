% #############################################################
% # reads out image values that we use for feature extraction #
% #############################################################

:- dynamic imageFeature/3.

% for a given ImageId, calcualte the values and assert them
calculateImageValues(ImageId):-
    % calculate image values
    info("Calculate Image Values..."),
    assertImageValuesForImageId(ImageId),
    % do sobel convertion for sharpness detection
    sobelEdgeDetection(ImageId,SobelImageId),
    assertImageValuesForImageId(SobelImageId),
    % use avgBrighness of sobel as sharpnessfactor
    imageValue(SobelImageId,avgBrightness,SharpnessFactor),
    assert(imageValue(ImageId,sharpness,SharpnessFactor)),
    % remove sobel image from memory
    deleteImage(SobelImageId),
    % debug all image values
    forall(imageValue(ImageId, Key, Value), debug(imageValues,imageValue(ImageId, Key, Value))).

assertImageValuesForImageId(ImageId):-
    imageMeta(ImageId, _,_,Depth,_,TotalPixelCount),
    % PREPAIR FACTS FOR ITERATION STEP
    debug(imageValues,"Prepare Facts"),
    assert(imageValue(ImageId,minBrightness,Depth)),
    assert(imageValue(ImageId,maxBrightness,0)),
    assert(imageValue(ImageId,totalR,0)),
    assert(imageValue(ImageId,totalG,0)),
    assert(imageValue(ImageId,totalB,0)),
    % ITERATION STEP #1
    debug(imageValues,"Start first iteration"),
    !,forall(imagePixel(ImageId,H,W,R,G,B),
        (!,
        % MIN BRIGHTNESS
        imageValue(ImageId,minBrightness,CurMinBrightness),!,
        MinBrightness is floor((R+G+B) / 3),!,
        NewMinBrightness is min(CurMinBrightness,MinBrightness),!,
        retract(imageValue(ImageId,minBrightness,_)),!,
        assert(imageValue(ImageId, minBrightness, NewMinBrightness)),!,

        % MAX BRIGHTNESS
        imageValue(ImageId,maxBrightness,CurMaxBrightness),!,
        MaxBrightness is floor((R+G+B) / 3),!,
        NewMaxBrightness is max(CurMaxBrightness,MaxBrightness),!,
        retract(imageValue(ImageId,maxBrightness,_)),!,
        assert(imageValue(ImageId, maxBrightness, NewMaxBrightness)),!,

        % ADD TOTALS
        imageValue(ImageId,totalR,CurR), NewR is CurR + R,!,
        retract(imageValue(ImageId,totalR,_)),!,
        assert(imageValue(ImageId, totalR, NewR)),!,
        imageValue(ImageId,totalG,CurG), NewG is CurG + G,!,
        retract(imageValue(ImageId,totalG,_)),!,
        assert(imageValue(ImageId, totalG, NewG)),!,
        imageValue(ImageId,totalB,CurB), NewB is CurB + B,!,
        retract(imageValue(ImageId,totalB,_)),!,
        assert(imageValue(ImageId, totalB, NewB)),!
        )
    ),!,
    debug(imageValues,"First iteration finished"),
    debug(imageValues,"Start AvgCalculations"),
    % AVERAGE CALCULATIONS
    imageValue(ImageId,totalR,TotalR),
    imageValue(ImageId,totalG,TotalG),
    imageValue(ImageId,totalB,TotalB),
    AvgR is TotalR / TotalPixelCount,
    AvgG is TotalG / TotalPixelCount,
    AvgB is TotalB / TotalPixelCount,
    assert(imageValue(ImageId,avgR,AvgR)),
    assert(imageValue(ImageId,avgG,AvgG)),
    assert(imageValue(ImageId,avgB,AvgB)),
    AvgBrightness is (AvgR+AvgG+AvgB) / 3,
    assert(imageValue(ImageId, avgBrightness, AvgBrightness)),

    % VARIANCE CALCULATIONS
    % calc variance with two-pass algoritm
    assert(imageValue(ImageId,stdSumR,0)),
    assert(imageValue(ImageId,stdSumG,0)),
    assert(imageValue(ImageId,stdSumB,0)),
    assert(imageValue(ImageId,stdSumBrightness,0)),
    debug(imageValues,"Second Iteration Started"),
    !,
    forall(imagePixel(ImageId,H,W,R,G,B),(!,
        imageValue(ImageId, stdSumR, StdSumROld),!,
        SdtSumRNew is StdSumROld + ((R-AvgR) * (R-AvgR)),!,
        retract(imageValue(ImageId, stdSumR, _)),!,
        assert(imageValue(ImageId, stdSumR, SdtSumRNew)),!,

        imageValue(ImageId, stdSumG, StdSumGOld),!,
        SdtSumGNew is StdSumGOld + ((G-AvgG) * (G-AvgG)),!,
        retract(imageValue(ImageId, stdSumG, _)),!,
        assert(imageValue(ImageId, stdSumG, SdtSumGNew)),!,

        imageValue(ImageId, stdSumB, StdSumBOld),!,
        SdtSumBNew is StdSumBOld + ((B-AvgB) * (B-AvgB)),!,
        retract(imageValue(ImageId, stdSumB, _)),!,
        assert(imageValue(ImageId, stdSumB, SdtSumBNew)),!,

        imageValue(ImageId, stdSumBrightness, StdSumBrightnessOld),!,
        Brigthness is floor((R+G+B)/3),!,
        SdtSumBrightnessNew is StdSumBrightnessOld + ((Brigthness-AvgBrightness) * (Brigthness-AvgBrightness)),!,
        retract(imageValue(ImageId, stdSumBrightness, _)),!,
        assert(imageValue(ImageId, stdSumBrightness, SdtSumBrightnessNew)),!
    )),!,
    debug(imageValues,"Second Iteration Done"),

    % calc std and variance
    imageValue(ImageId, stdSumR, StdSumR),!,
    StdR is StdSumR / (TotalPixelCount - 1), VarR is sqrt(StdR),!,
    assert(imageValue(ImageId, stdR, StdR )),!,
    assert(imageValue(ImageId, varR, VarR )),!,

    imageValue(ImageId, stdSumG, StdSumG),!,
    StdG is StdSumG / (TotalPixelCount - 1), VarG is sqrt(StdG),!,
    assert(imageValue(ImageId, stdG, StdG )),!,
    assert(imageValue(ImageId, varG, VarG )),!,

    imageValue(ImageId, stdSumB, StdSumB),!,
    StdB is StdSumB / (TotalPixelCount - 1), VarB is sqrt(StdB),!,
    assert(imageValue(ImageId, stdB, StdB )),!,
    assert(imageValue(ImageId, varB, VarB )),!,

    imageValue(ImageId, stdSumBrightness, StdSumBrightness),!,
    StdBrightness is StdSumBrightness / (TotalPixelCount - 1), VarBrightness is sqrt(StdBrightness),!,
    assert(imageValue(ImageId, stdBrightness, StdBrightness )),!,
    assert(imageValue(ImageId, varBrightness, VarBrightness )),
    !.


% edge detection with sobel
% idea: https://medium.com/@enzoftware/how-to-build-amazing-images-filters-with-python-median-filter-sobel-filter-%EF%B8%8F-%EF%B8%8F-22aeb8e2f540
sobelEdgeDetection(ImageId,SobelImageId) :-
    string_concat(ImageId,".sobel.ppm",SobelImageId),
    copyImageMetaData(ImageId,SobelImageId),
    forall(imagePixel(ImageId,H,W,R,G,B), sobelEdgeDetectionStep(imagePixel(ImageId,H,W,R,G,B),SobelImageId)).
% step that is performed on each pixel, can fail and goto default
sobelEdgeDetectionStep(imagePixel(ImageId,H,W,_,_,_),SobelImageId):-
    imageMeta(ImageId, _,_,Depth,_,_),
    pixelKernel(ImageId,H,W,RLT,GLT,BLT,RMT,GMT,BMT,RRT,GRT,BRT,
                RLM,GLM,BLM,RRM,GRM,BRM,RLB,GLB,BLB,RMB,GMB,BMB,RRB,GRB,BRB),
    LT is RLT+GLT+BLT,!, MT is RMT+GMT+BMT,!,
    RT is RRT+GRT+BRT,!, LM is RLM+GLM+BLM,!,
    RM is RRM+GRM+BRM,!, LB is RLB+GLB+BLB,!,
    MB is RMB+GMB+BMB,!, RB is RRB+GRB+BRB,!,
    Gx is (-LT) + (-MT - MT) + (-RT) + LB + (MB+MB) + RB,
    Gy is (-LT) + RT + (-LM - LM) + (RM+RM) + (-LB) + RB,
    Temp1 is Gx*Gx, Temp2 is Gy*Gy, Temp3 is Temp1 + Temp2,
    Temp4 is sqrt(Temp3), Temp5 is Temp4 / 4328 * Depth,
    Length is floor(Temp5),
    assert(imagePixel(SobelImageId,H,W,Length,Length,Length)).
% we have no fail case here - so we need a fallback for the corner cases
sobelEdgeDetectionStep(imagePixel(_,H,W,_,_,_),SobelImageId) :-
    assert(imagePixel(SobelImageId,H,W,0,0,0)).
