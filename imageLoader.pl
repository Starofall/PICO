% ######################################################
% # loading of PPM images into prolog using diff lists #
% ######################################################


% Loads a Image file at path and returns the Image in a custom format
% open and closing the file is independent of the processed result
loadImage(Path,Image):-
    setup_call_cleanup(
        (
         debug(imageLoader,'Opening File'),
         open(Path, read, FileStream,[buffer(full),close_on_abort(true)])
         ),
        (!,processLoadImageStream(FileStream,Path, Image),!),
        (debug(imageLoader,'Closing File'),close(FileStream))
    ).


% loads a filestream into an image and stores it into
% 1) imageMeta(ImageId,width,height,depth) 2) imagePixels(ImageId,x,y,r,g,b)
processLoadImageStream(FileStream,Path,ImageId) :-
    debug(imageLoader,'Loading Header'),
    readWord(FileStream,FileBang),
    FileBang == 'P3', % check filebang
    debug(imageLoader,'Loading Comment Field'),
    readLine(FileStream,_),
    debug(imageLoader,'Loading Image Spec'),
    readWord(FileStream,WidthStr),  atom_number(WidthStr,  Width),
    readWord(FileStream,HeightStr), atom_number(HeightStr, Height),
    readWord(FileStream,DepthStr),  atom_number(DepthStr,  Depth),
    % save metadata
    TotalPixelCount is Width*Height,
    assert(imageMeta(ImageId, Width,Height,Depth,Path,TotalPixelCount)),
    debug(imageLoader,'Loading Pixel Data'),
    % loading image data
    !, loadAssertAllPixels(FileStream,TotalPixelCount,0,ImageId,Width).


% stop if pixelcount is length of accumulator
loadAssertAllPixels(_,PixelCount,PixelCount,_,_):-
    debug(v,"Finished loading Pixel Data").
loadAssertAllPixels(FileStream, PixelCount,CurrentPixelCount,ImageId,Width):-
    % while stream contains data read them
    !,readRGBTuple(FileStream,(R,G,B)),
    !,X is mod(CurrentPixelCount,Width),
    !,Y is floor(CurrentPixelCount / Width),
    !,assert(imagePixel(ImageId, X,Y,R,G,B)),
    !,NewPixelCount is CurrentPixelCount + 1,
    !,loadAssertAllPixels(FileStream, PixelCount,NewPixelCount,ImageId,Width).


% reads a rgb tuple from the file stream
readRGBTuple(FileStream,(R,G,B)):-
    !,readWordIgnoreNewLine(FileStream,RStr),!, atom_number(RStr, R),
    !,readWordIgnoreNewLine(FileStream,GStr),!, atom_number(GStr, G),
    !,readWordIgnoreNewLine(FileStream,BStr),!, atom_number(BStr, B).
