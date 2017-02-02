-module (print).
-compile (export_all).

printDict(IoDevice, Dict, ValuePrint) ->
	Keylist = dict:fetch_keys(Dict),
	lists:foreach(fun(Key) ->
			io:format(IoDevice, "Key = ~p,", [Key]),
			Value = dict:fetch(Key, Dict),
			ValuePrint(IoDevice, Value)
	end, Keylist).

printDict(IoDevice, Dict, ValuePrint, Num) ->
	Keylist = dict:fetch_keys(Dict),
	lists:foreach(fun(Key) ->
			io:format(IoDevice, "Key = ~p,", [Key]),
			Value = dict:fetch(Key, Dict),
			ValuePrint(IoDevice, Value)
	end, lists:sublist(Keylist, Num)).



printSet(IoDevice, Set, ItemPrint) ->
   io:format(IoDevice, "[", []),
   lists:foreach(fun(Item) ->
          ItemPrint(IoDevice, Item)
   end, sets:to_list(Set)),
   io:format(IoDevice, "]", []).

debug(S, L, Debug) ->
    case Debug of
        true ->
            io:format(S, L);
        false ->
            ok
    end.


printListSample(List) ->
	if
		length(List)<20 ->
			io:format("~p:~p~n", [erlang:now(),List]);
		true ->
			io:format("~p:length = ~p, first = ~p, last = ~p~n", [erlang:now(),length(List), lists:nth(1, List), lists:last(List)])
	end.

printTime(S, L) ->
	{ok, Fp} = file:open("../data/runlog", [append]),
	io:format(Fp, "~p:", [erlang:now()]),
	io:format(Fp, S, L),
	file:close(Fp).