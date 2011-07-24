-module(writetest).

-compile(export_all).


accumulate_string([], _, CurrentList) ->
  lists:reverse(CurrentList);

accumulate_string([H|T], F, CurrentList) ->
  accumulate_string(T, F, [F(H)| CurrentList]).

accumulate_string(InputList, F) ->
  accumulate_string(InputList, F, []).

integer_to_binary_string(I,EOL) ->
  Num = list_to_binary(integer_to_list(I)), 
  <<Num/binary,EOL/binary>>.

write_list(JsonFile,Max,ProcessFun) ->
  {ok, S} = file:open(JsonFile, [write,delayed_write]),
  Writer = spawn(writetest, file_writer, [S]),
  List = lists:seq(1,Max),
  ProcessFun(List,Writer),
  Writer ! {close}. 

%write_file(List,Writer, AccumulateFun) ->
%  Writer ! {write, writetest:accumulate_string(List, AccumulateFun)}.

write_list_allatonce(JsonFile,Max) ->
  EndlineBinary = list_to_binary(io_lib:format("~n",[])),
  AccumulateStringFun = fun(I) -> integer_to_binary_string(I, EndlineBinary) end,
  WriteFileFun = fun(List,Writer) -> Writer ! {write,  writetest:accumulate_string(List,AccumulateStringFun)} end,
  write_list(JsonFile, Max, WriteFileFun).

process_list_group(_, _, _, 0) -> ok;

process_list_group(List, Process, GroupSize, ListLength) -> 
  NumberToProcess = min(ListLength, GroupSize),
  {ToProcess, Remaining} = lists:split(NumberToProcess, List),
  Process(ToProcess),
  process_list_group(Remaining, Process, GroupSize, ListLength - NumberToProcess).

process_list_group(List, Process, GroupSize) -> 
  process_list_group(List, Process, GroupSize, length(List)).
  
write_list_fewatatime(JsonFile,Max,GroupSize) -> 
  EndlineBinary = list_to_binary(io_lib:format("~n",[])),
  AccumulateStringFun = fun(I) -> integer_to_binary_string(I, EndlineBinary) end,
  WriteFileFun = fun(List,Writer) -> Writer ! {write,  writetest:accumulate_string(List,AccumulateStringFun)} end,
  ProcessFun = fun(List,Writer) -> writetest:process_list_group(List, fun(L) -> WriteFileFun(L,Writer) end, GroupSize) end,
  write_list(JsonFile, Max, ProcessFun).

%write_list_parallel_fewatatime(JsonFile, Max, GroupSize, Processes) ->

file_writer(S) -> 
  receive
    {write, Output} -> 
      file:write(S,Output),
      file_writer(S);
    {close} -> 
      file:close(S)
  end.
