% ################################
% # utility for image processing #
% ################################

:- ensure_loaded(fileUtil).
:- ensure_loaded(imageValues).

% copys the metadata from a given ImageId to a new ImageId
copyImageMetaData(SourceImageId,TargetImageId):-
    imageMeta(SourceImageId, Width,Height,Depth,Path,TotalPixelCount),
    assert(imageMeta(TargetImageId, Width,Height,Depth,Path,TotalPixelCount)).

% removes all data from an ImageId from memory
deleteImage(ImageId):-
    retractall(imageMeta(ImageId, _,_,_,_,_)),
    retractall(imagePixel(ImageId, _,_,_,_,_)),
    retractall(imageValue(ImageId, _,_ )),
    debug(imageUtil,"Image is deleted").

% prints the currently cache images by ImageId
printStoredImages:-
    write("Currently Stored Images:"),nl,
    forall(
        (
          imagePixel(ImageId,0,0,_,_,_),
          imageMeta(ImageId,_,_,_,_,_)
        ),
        (
        write("> "), write(ImageId),nl
        )
    ).

% writes the image metadata to stdout
describeImage(ImageId) :-
    imageMeta(ImageId, H,W,Depth,Path,_),
    write('> Image Name:  '), write(Path), nl,
    write('> Image Size:  '), write(H), write(' x '), write(W), nl,
    write('> Image Depth: '), write(Depth), nl,
    calculateImageValues(ImageId).

% makes sure a rgb color is in a valid range (0-255) and rounded
capColor((R,G,B),(NR,NG,NB)) :-
    capRGB(R,NR),
    capRGB(G,NG),
    capRGB(B,NB).

% caps a single color to (0-255) and rounds the value
capRGB(X,Y) :-
    X =< 255,
    X >= 0,
    Y is round(X).
capRGB(X,Y) :-
    X >= 255,
    Y = 255.
capRGB(X,Y) :-
    X =< 0,
    Y = 0.

% reads the 8 next pixels for a given pixel to allow kernel calculations
pixelKernel(ImageId,H,W,RLT,GLT,BLT,RMT,GMT,BMT,RRT,GRT,BRT,
            RLM,GLM,BLM,RRM,GRM,BRM,RLB,GLB,BLB,RMB,GMB,BMB,RRB,GRB,BRB):-
    imageMeta(ImageId, ImageWidth,ImageHeight,_,_,_),
    WidthMinus is ImageWidth - 1,
    HeightMinus is ImageHeight - 1,
    H @> 0,W @> 0,H @< HeightMinus,W @< WidthMinus,!,
    HMinus is H - 1,WMinus is W - 1,
    HPlus  is H + 1,WPlus  is W + 1,
    imagePixel(ImageId,HMinus,WMinus,RLT,GLT,BLT),
    imagePixel(ImageId,HMinus,W     ,RMT,GMT,BMT),
    imagePixel(ImageId,HMinus,WPlus ,RRT,GRT,BRT),
    imagePixel(ImageId,H     ,WMinus,RLM,GLM,BLM),
    imagePixel(ImageId,H     ,WPlus ,RRM,GRM,BRM),
    imagePixel(ImageId,HPlus ,WMinus,RLB,GLB,BLB),
    imagePixel(ImageId,HPlus ,W     ,RMB,GMB,BMB),
    imagePixel(ImageId,HPlus ,WPlus ,RRB,GRB,BRB).