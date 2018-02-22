% #####################################
% # writing of PPM images from prolog #
% #####################################

% writes a given ImageId into the output folder
writeOutput(ImageId):-
    !,string_concat('./img/output/',ImageId,FilePath),
    !,writeImage(FilePath,ImageId).

% writes a given ImageId into the preview folder
writePreview(ImageId):-
    !,string_concat('./img/preview/',ImageId,FilePath),
    !,writeImage(FilePath,ImageId).

% writes the given image to a given path
writeImage(Path,ImageId):-
    setup_call_cleanup(
        (
         debug(imageWriter,'Opening File'),
         open(Path, write, FileStream,[buffer(full),close_on_abort(true)])
         ),
        (!,debug(imageWriter,'Start writing image'),writeImageStream(FileStream,ImageId),!),
        (debug(imageWriter,'Closing File'),close(FileStream))
    ).

% writes the image into the open filestream
writeImageStream(FileStream,ImageId) :-
    !,debug(imageWriter,'Write Header'),
    !,imageMeta(ImageId, Width,Height,Depth,_,TotalPixelCount),
    !,write(FileStream,'P3\n'),
    !,debug(imageWriter,'Write Comment Field'),
    !,write(FileStream,'# Written by Prolog\n'),
    !,debug(imageWriter,'Write Image Spec'),
    !,write(FileStream,Width),
    !,write(FileStream,' '),
    !,write(FileStream,Height),
    !,write(FileStream,'\n'),
    !,write(FileStream,Depth),
    !,write(FileStream,'\n'),
    !,debug(imageWriter,'Writing Pixel Data'),
    !, iterativeWrite(FileStream,ImageId,Width,0,TotalPixelCount).

% recursive writing until all images are written
iterativeWrite(_,_,_     ,TotalPixelCount,TotalPixelCount).
iterativeWrite(FileStream,ImageId,Width,Counter,TotalPixelCount):-
        !,CurX is mod(Counter,Width),
        !,CurY is floor(Counter / Width),
        !,NewCounter is Counter + 1,
        !,imagePixel(ImageId, CurX,CurY,R,G,B),
        !,writeRGBTuple(FileStream,(R,G,B)),
        !,iterativeWrite(FileStream,ImageId,Width,NewCounter,TotalPixelCount).

% writes a single rgb image to filestream
writeRGBTuple(FileStream,(R,G,B)):-
    write(FileStream,R),
    write(FileStream," "),
    write(FileStream,G),
    write(FileStream," "),
    write(FileStream,B),
    write(FileStream," \n").

