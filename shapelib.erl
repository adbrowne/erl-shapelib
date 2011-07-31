-module(shapelib).

-export([get_shapes/1,write_json/4]).

-compile(export_all).

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

format_points([{X,Y}|T],Result, notfirst) ->
  %PointString = lists:flatten(["L", integer_to_list(X), ",", integer_to_list(Y)]),
  PointString = io_lib:format("L~p,~p",[X,Y]),
  format_points(T,[PointString|Result],notfirst);
format_points([],Result,notfirst) -> lists:reverse(Result).

format_points([]) -> [];
format_points([{X,Y}|T]) ->
  PointString = lists:concat(["M",X,",",Y]),
  format_points(T,[PointString],notfirst).

get_scaled_string(Part,{Scale,Xmin,Ymin,Height}) ->
  ScaledPoints = [{(X-Xmin)*Scale, Height-(Y-Ymin) * Scale} || {X,Y} <- Part],
  io:format(".",[]),
  format_points(ScaledPoints).

get_scaled_points(Part,{Scale,Xmin,Ymin,Height}) ->
  [{(X-Xmin)*Scale, Height-(Y-Ymin) * Scale} || {X,Y} <- Part].

get_scaled_strings_for_polygon(Polygon, ScaleFactors,WriterPid) ->
  io:format("Polygon:~n",[]),
  {_, {Xmin, Ymin, Xmax, Ymax, Parts, PartPoints}} = Polygon,
  io:format("Post Polygon:~n",[]),
  [WriterPid ! {points,get_scaled_points(X,ScaleFactors),self()} || X <- PartPoints],
  receive
    printed -> ok
  end,
  ok.

write_json(ShapeFile, JsonFile,Width, Height) ->
  Shapes = get_shapes(ShapeFile),
  {_,_,{Xmin,Ymin,Xmax,Ymax},Polygons} = Shapes,
  XScale = Width/(Xmax - Xmin),
  YScale = Height/(Ymax - Ymin),
  Scale = max(XScale,YScale),
  {ok, S} = file:open(JsonFile, [write,delayed_write]),
  io:format(S,"MapPoints = [",[]),
  WriterPid = spawn(fun() -> loop(S) end),
  PolygonGroups = split_into_groups(Polygons, 128),
  io:format("Groups: ~p~n",[length(PolygonGroups)]),
  pmap1(fun(PSet) -> [get_scaled_strings_for_polygon(X,{Scale,Xmin, Ymin, Height},WriterPid) || X <- PSet] end, PolygonGroups),
  %[spawn(fun() -> get_scaled_strings_for_polygon(X,{Scale,Xmin, Ymin, Height},WriterPid) end) || X <- Polygons].
  %WriterPid ! {line,"];"},
  WriterPid ! close.

write_polygon_to_file(Polygon, FileName, {Scale, Width, Height, Xmin, Ymin, Xmax, Ymax}) ->
  io:format("~s~n",[FileName]),
  {ok, S} = file:open(FileName, [write,delayed_write]),
  io:format(S,"MapPoints = [",[]),
  WriterPid = spawn(fun() -> loop(S) end),
  get_scaled_strings_for_polygon(Polygon,{Scale,Xmin, Ymin, Height},WriterPid),
  WriterPid ! close.

write_polygons_to_dir([], _, _,_) ->
  ok;

write_polygons_to_dir([H|T], OutputDir, Specs, Num) ->
  write_polygon_to_file(H, OutputDir ++ "/"++integer_to_list(Num)++".js", Specs),
  write_polygons_to_dir(T, OutputDir, Specs, Num+1).
  
write_polygons_to_dir(Polygons, OutputDir, Specs) ->
  write_polygons_to_dir(Polygons, OutputDir, Specs, 0).

write_json_multiple_files(ShapeFile, OutputDir, Width, Height) ->
  Shapes = get_shapes(ShapeFile),
  {_,_,{Xmin,Ymin,Xmax,Ymax},Polygons} = Shapes,
  XScale = Width/(Xmax - Xmin),
  YScale = Height/(Ymax - Ymin),
  Scale = max(XScale,YScale),
  Specs = {Scale, Width, Height, Xmin, Ymin, Xmax, Ymax},
  write_polygons_to_dir(Polygons, OutputDir, Specs).

print_points([{X,Y}|T],S,notfirst) ->
  io:format(S,"L~p,~p",[X,Y]),
  print_points(T,S,notfirst);
print_points([],_,notfirst) -> ok.

print_points([],S) -> [];
print_points([{X,Y}|T],S) ->
  io:format(S,"M~p,~p",[X,Y]),
  Groups = split_into_groups(T, 8),
  Output = lists:map(fun(PSet) -> format_points(PSet,[],notfirst) end, Groups),
  [io:format(S,"~s",[X]) || X <- Output].
  %print_points(T,S,notfirst).

loop(S) ->
  receive
    {points, Points, Parent} ->
      io:format(S,"\"",[]),
      print_points(Points,S),
      io:format(S,"\",~n",[]),
      Parent ! printed,
      loop(S);
    {line, Text} ->
      io:format(S,"\"",[]),
      io:put_chars(S,Text),
      io:format(S,"\",~n",[]),
      loop(S);
    close ->
      io:format(S,"];",[]),
      file:close(S)
  end.

split_into_groups_of_size([], _Size) -> [];
split_into_groups_of_size(List, Size) ->
  {List1,List2} = lists:split(min(length(List),Size),List),
  [List1 | split_into_groups_of_size(List2, Size)].

split_into_groups(List, NumberOfGroups) ->
  Length = length(List),
  Size = Length div NumberOfGroups + 1,
  split_into_groups_of_size(List,Size).

pmap1(F, L) -> 
  S = self(),
  Ref = erlang:make_ref(),
  lists:foreach(fun(I) -> 
        spawn(fun() -> do_f1(S, Ref, F, I) end)
    end, L),
  %% gather the results
  gather1(length(L), Ref, []).

do_f1(Parent, Ref, F, I) ->					    
  Parent ! {Ref, (catch F(I))}.

gather1(0, _, L) -> L;
gather1(N, Ref, L) ->
  receive
    {Ref, Ret} -> gather1(N-1, Ref, [Ret|L])
  end.

pbuild_string(F, L) ->
  Groups = split_into_groups(L,8),
  [F(X) || X <- L].
