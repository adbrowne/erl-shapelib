-module(shapelib).

-export([get_shapes/1]).

get_polygon(S, Offset, ContentLength) ->
  {ok, Content} = file:pread(S,Offset,ContentLength * 2),
  io:format("~p~n",[Offset]),
  ContentBits = ContentLength * 2 * 8 - 32 * 7,
  <<RecordShapeType:32/little,Xmin:32/little-float,Ymin:32/little-float, Xmax:32/little-float, Ymax:32/little-float,NumParts:32, NumPoints:32,_:ContentBits>> = Content,
 {Xmin,Ymin,Xmax,Ymax}.

get_polygons(S,Offset,FileLength) ->
  io:format("~w~n",[Offset]),
  case file:pread(S, Offset, 8) of
    {ok, <<RecordNumber:32/big,ContentLength:32/big>>} -> 
      [{RecordNumber, get_polygon(S,Offset+8,ContentLength)} | get_polygons(S,Offset + ContentLength * 2 + 8,FileLength)];
    eof -> []
  end.

get_shapes(File) ->
  case file:open(File, [read, binary,raw]) of
    {ok, S} ->
      {ok, <<9994:32/big,_:160,FileLength:32/big,Version:32/little,ShapeType:32/little,Xmin:32/little-float,Ymin:32/little-float,Xmax:32/little-float,Ymax:32/little-float,_:384>>} = file:pread(S, 0, 100),
      % ensure polygon type
      5 = ShapeType,
      Polygons = get_polygons(S,100,FileLength),
      file:close(S),
      {ok,Version,Polygons};
    _Error ->
      error
  end. 
