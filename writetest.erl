-module(writetest).

-compile(export_all).


accumulate_string([], _, CurrentList) ->
lists:reverse(CurrentList);

accumulate_string([H|T], F, CurrentList) ->
Value = F(H),
      accumulate_string(T, F, [Value| CurrentList]).

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

	write_out(_, _, 1000) -> 
	ok;
	write_out(Writer, Output, Count) -> 
	Writer ! {write, Output},
	write_out(Writer, Output, Count + 1).

	write_list(JsonFile,Max) ->
{ok, S} = file:open(JsonFile, [write,delayed_write]),
	Writer = spawn(writetest, file_writer, [S]),
	List = lists:seq(1,Max,1000),
	AccumulateFun = create_accumulate_string_fun(),
	Result = writetest:accumulate_string(List, AccumulateFun),
	write_out(Writer, Result, 1),
	Writer ! {close}. 

	write_file(List,Writer, AccumulateFun) ->
	Writer ! {write, writetest:accumulate_string(List, AccumulateFun)}.

	eol() -> 
	list_to_binary(io_lib:format("~n",[])).

	create_accumulate_string_fun() -> 
	fun(I) -> integer_to_binary_string(I, eol()) end.

	create_write_file_fun() -> 
	AccumulateStringFun = create_accumulate_string_fun(),
	fun(List,Writer) -> writetest:write_file(List,Writer,AccumulateStringFun) end.

	create_process_group_fun(GroupSize) -> 
	WriteFileFun = create_write_file_fun(),
	fun(List,Writer) -> writetest:process_list_group(List, fun(L) -> WriteFileFun(L,Writer) end, GroupSize) end.

	write_list_allatonce(JsonFile,Max) ->
	write_list(JsonFile, Max, create_write_file_fun()).

	process_list_group(_, _, _, 0) -> ok;

	process_list_group(List, Process, GroupSize, ListLength) -> 
	NumberToProcess = min(ListLength, GroupSize),
{ToProcess, Remaining} = lists:split(NumberToProcess, List),
	Process(ToProcess),
	process_list_group(Remaining, Process, GroupSize, ListLength - NumberToProcess).

	process_list_group(List, Process, GroupSize) -> 
	process_list_group(List, Process, GroupSize, length(List)).

	write_list_fewatatime(JsonFile,Max,GroupSize) -> 
	ProcessFun = create_process_group_fun(GroupSize),
	write_list(JsonFile, Max, ProcessFun).

	write_list_parallel_fewatatime(JsonFile, Max, GroupSize, Processes) ->
	ProcessFun = fun(List,Writer) -> parallel_process(List,Writer,GroupSize, Processes) end,
	%ProcessFun = fun(List,Writer) -> io:format("Writer this ~p~n",[Writer]),some_fun(Writer) end,
	write_list(JsonFile, Max, ProcessFun).

	some_fun(Writer) ->
	io:format("Writer some_fun : ~p~n",[Writer]).

	spawn_processor({list,List,ListLength}, GroupSize) -> 
	NumberToProcess = min(ListLength, GroupSize),
{ToProcess, Remaining} = lists:split(NumberToProcess, List),
	Sender = self(),
	Ref = make_ref(),
	spawn_link(writetest,processor_task, [ToProcess, Sender, Ref]),
{{list,Remaining, ListLength - NumberToProcess}, Ref}.

processor_task(List, Sender, Ref) -> 
CreateString = create_accumulate_string_fun(),
	     Sender ! {result, Ref, writetest:accumulate_string(List, CreateString)}.

	     dequeue(List,Writer, GroupSize, MaxProcesses, CurrentProcesses, ProcessQueue) ->
	     QueueResult = queue:out(ProcessQueue),
	     case QueueResult of
{{value, Ref}, RemainingQueue} ->
receive
{result, Ref, Output} -> 
Writer ! {write, Output},
       parallel_process(List, Writer, GroupSize, MaxProcesses, CurrentProcesses - 1, RemainingQueue)
	end;
{empty, _} -> 
ok
end.

parallel_process({list,List,0}, Writer, GroupSize, MaxProcesses, CurrentProcesses, ProcessQueue) -> 
dequeue({list,List,0},Writer, GroupSize, MaxProcesses, CurrentProcesses-1, ProcessQueue);

parallel_process(List, Writer, GroupSize, MaxProcesses, CurrentProcesses, ProcessQueue) -> 
	if(CurrentProcesses < MaxProcesses) -> 
{ListTail, NewProcessRef} = spawn_processor(List, GroupSize),
	parallel_process(ListTail, Writer, GroupSize, MaxProcesses, CurrentProcesses + 1, queue:in(NewProcessRef, ProcessQueue))
	; CurrentProcesses >= MaxProcesses -> 
dequeue(List,Writer, GroupSize, MaxProcesses, CurrentProcesses, ProcessQueue)
	end.

	parallel_process(List, Writer, GroupSize, Processes) -> 
	parallel_process({list,List,length(List)}, Writer, GroupSize, Processes, 0, queue:new()).

	file_writer(S) -> 
	receive
{write, Output} -> 
file:write(S,Output),
	file_writer(S);
{close} -> 
file:close(S)
	end.
