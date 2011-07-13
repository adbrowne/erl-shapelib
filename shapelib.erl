-module(shapelib).

-export([get_shapes/1,write_json/4]).

get_parts(NumParts,Parts) ->
  [X || <<X:32/little-integer>> <= Parts].

get_points(Points) ->
  [{X, Y} || <<X:64/little-float,Y:64/little-float>> <= Points].

get_partpoints(PartList, PointList) ->
  case PartList of
    [H1] -> {_, This} = lists:split(H1, PointList), [This];
    [H1, H2 | T] -> 
      Len = H2 - H1,
      Start = H1 + 1,
      %io:format("~p ~p ~p~n",[Start, Len, length(PointList)]), 
      [lists:sublist(PointList, Start, Len) | get_partpoints([H2|T], PointList)]
  end.

get_polygon(S, Offset, ContentLength) ->
  {ok, Content} = file:pread(S,Offset,ContentLength * 2),
  %io:format("~p~n",[Offset]),
  ContentBits = ContentLength * 2 * 8 - 64 * 4 - 32 * 3,
  <<RecordShapeType:32/little,Xmin:64/little-float,Ymin:64/little-float, Xmax:64/little-float, Ymax:64/little-float,NumParts:32/little-integer, NumPoints:32/little-integer,PartsAndPoints:ContentBits/binary-unit:1>> = Content,
  io:format("ContentBits: ~p  NumParts: ~p  NumPoints: ~p~n",[ContentBits, NumParts, NumPoints]),
  PartBits = NumParts * 32,
  PointBits = ContentBits - PartBits,
  <<Parts:PartBits/binary-unit:1,Points:PointBits/binary-unit:1>> = PartsAndPoints,
  PartList = get_parts(NumParts,Parts), 
  PointList = get_points(Points),
  PartPoints = get_partpoints(PartList, PointList),
  {Xmin,Ymin,Xmax,Ymax,PartList,PartPoints}.

get_polygons(S,Offset,FileLength) ->
  io:format("~w of ~w~n",[Offset,FileLength]),
  case file:pread(S, Offset, 8) of
    {ok, <<RecordNumber:32/big,ContentLength:32/big>>} -> 
      [{RecordNumber, get_polygon(S,Offset+8,ContentLength)} | get_polygons(S,Offset + ContentLength * 2 + 8,FileLength)];
    eof -> []
  end.

get_shapes(File) ->
  case file:open(File, [read, binary,raw]) of
    {ok, S} ->
      {ok, <<9994:32/big,_:160,FileLength:32/big,Version:32/little,ShapeType:32/little,Xmin:64/little-float,Ymin:64/little-float,Xmax:64/little-float,Ymax:64/little-float,_:(352-96)>>} = file:pread(S, 0, 100),
      io:format("~p ~p ~p ~p~n",[Xmin,Xmax,Ymin,Ymax]),
      % ensure polygon type
      5 = ShapeType,
      Polygons = get_polygons(S,100,FileLength),
      file:close(S),
      {ok,Version,{Xmin,Ymin,Xmax,Ymax},Polygons};
    _Error ->
      error
  end. 

format_points([],notfirst) -> [];
format_points([{X,Y}|T], notfirst) ->
  [io_lib:format("L~p,~p",[X,Y])|format_points(T,notfirst)].

format_points([]) -> [];
format_points([{X,Y}|T]) ->
  [io_lib:format("M~p,~p",[X,Y])|format_points(T,notfirst)].

get_scaled_string(Part,{Scale,Xmin,Ymin,Height}) ->
  ScaledPoints = [{(X-Xmin)*Scale, Height-(Y-Ymin) * Scale} || {X,Y} <- Part],
  io:format(".",[]),
  format_points(ScaledPoints).

get_scaled_strings_for_polygon(Polygon, ScaleFactors,WriterPid) ->
  {_, {Xmin, Ymin, Xmax, Ymax, Parts, Points}} = Polygon,
  PolygonString = [get_scaled_string(X,ScaleFactors) || X <- Points],
  WriterPid ! {line,io_lib:format("\"~s\",~n",[PolygonString])}.

write_json(ShapeFile, JsonFile,Width, Height) ->
  Shapes = get_shapes(ShapeFile),
  {_,_,{Xmin,Ymin,Xmax,Ymax},Polygons} = Shapes,
  XScale = Width/(Xmax - Xmin),
  YScale = Height/(Ymax - Ymin),
  Scale = max(XScale,YScale),
  {ok, S} = file:open(JsonFile, write),
  io:format(S,"MapPoints = [",[]),
  WriterPid = spawn(fun() -> loop(S) end),
  [spawn(fun() -> get_scaled_strings_for_polygon(X,{Scale,Xmin, Ymin, Height},WriterPid) end) || X <- Polygons].
  %WriterPid ! {line,"];"},
  %WriterPid ! close.

loop(S) ->
  receive
    {line, Text} ->
      io:format(S,"~s",[Text]),
      loop(S)
  after 5000 -> 
      io:format(S,"];",[]),
      file:close(S)
  end.
