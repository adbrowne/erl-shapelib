-module(writetest).

-compile(export_all).


accumulate_string([], _, CurrentList) ->
  lists:reverse(CurrentList);
  
accumulate_string([H|T], F, CurrentList) ->
  accumulate_string(T, F, [F(H)| CurrentList]).

accumulate_string(InputList, F) ->
  accumulate_string(InputList, F, []).
  
write_pairs(JsonFile,Max) ->
  {ok, S} = file:open(JsonFile, [write,delayed_write]),
  List = lists:seq(1,Max),
  EndlineBinary = list_to_binary(io_lib:format("~n",[])),
  Result = writetest:accumulate_string(List,fun(I) -> Num = list_to_binary(integer_to_list(I)), <<Num/binary,EndlineBinary/binary>> end),
  file:write(S,Result),
  file:close(S).