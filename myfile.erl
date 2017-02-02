-module (myfile).
-compile (export_all).
-author(baoning).

% pagenew
% the only splitors are , and \n
% three head lines, read once line, write once line, others
% read/write once : number,inode1,index1,time1,inode2,index2,time2...
% other line : number,inode1,index1,inode2,index2...
% all the pages in other line, same order
% one page one line
% inode,index,time,read,write...

readLines(_Fp, 0) -> ok;
readLines(Fp, N) -> io:get_line(Fp, ''), readLines(Fp, N-1).


% attention : needed to be reversed
parseNumList(_, [], L) -> L;
parseNumList(Num, StringList, PageList) ->
	Length = length(StringList),
	if
		Length>Num * 10000 ->
			print:debug("separate 10000~n", [], false),
			PartList = parseNumList(Num, lists:sublist(StringList, Num*10000), []),
			print:debug("finish 10000~n", [], false),
			parseNumList(Num, lists:sublist(StringList, Num*10000+1, Length-Num*10000), PartList ++ PageList);
		true ->
	
			HeadList = lists:sublist(StringList, Num),
			TailList = lists:sublist(StringList, Num+1, length(StringList)-Num),
			TupleList = lists:foldl(fun(String, List) -> 
				{Number, _} = string:to_integer(String), 
				[Number|List]
			end, [], HeadList),
			Tuple = list_to_tuple(lists:reverse(TupleList)),
			parseNumList(Num, TailList, [Tuple|PageList])
	end.



parseLine(Fp, Tokens, HeadNum) ->
	Line = io:get_line(Fp, ''),
	TokenList = string:tokens(Line, Tokens),
	lists:sublist(TokenList, HeadNum+1, length(TokenList)-HeadNum).

changePageFile(ReadFn, WriteFn) ->
	{ok, Fp} = file:open(ReadFn, read),
	HeadLine = parseLine(Fp, " ,{}[]page\n", 1),
	PageList = lists:reverse(myfile:parseNumList(2, HeadLine, [])),
	io:format("~p~n", [length(PageList)]),	
	{Rl, Wl, Ol, Tl} = changePageFileCore(Fp, PageList, [], [], [], []),
	file:close(Fp),
	{ok, Fpw} = file:open(WriteFn, write),
	RlSorted = lists:sort(Rl),
	WlSorted = lists:sort(Wl),
	OlSorted = lists:sort(Ol),
	TlSorted = sortList(OlSorted, Ol, Tl, []),
	changePageFilePrintHead(Fpw, RlSorted, 3),
	changePageFilePrintHead(Fpw, WlSorted, 3),
	changePageFilePrintHead(Fpw, OlSorted, 2),
	changePageFilePrintTime(Fpw, OlSorted, TlSorted),
	file:close(Fpw).

changePageFile2(ReadFn, WriteFn) ->
	{ok, Fp} = file:open(ReadFn, read),
	{RlParsed, WlParsed, OlParsed} = readHead(Fp, pagenew),
	{ok, Fw} = file:open(WriteFn, write),
	io:format(Fw, "~p,", [length(RlParsed)]),
	lists:foreach(fun(Tuple) -> io:format(Fw, "~p,~p,", [element(1, Tuple), element(2, Tuple)]) end, RlParsed),
	io:format(Fw, "~n", []),
	io:format(Fw, "~p,", [length(WlParsed)]),
	lists:foreach(fun(Tuple) -> io:format(Fw, "~p,~p,", [element(1, Tuple), element(2, Tuple)]) end, WlParsed),
	io:format(Fw, "~n", []),
	io:format(Fw, "~p,", [length(OlParsed)]),
	lists:foreach(fun(Tuple) ->
		Line = parseLine(Fp, ",\n", 2),
		NumberList = parseNumList(3, Line, []),
		{Rsum2, Wsum2} = lists:foldr(fun({_, R, W}, {Rsum, Wsum}) -> {Rsum+R, Wsum+W} end, {0,0}, NumberList),
		io:format(Fw, "~p,~p,~p,~p,", [element(1, Tuple), element(2, Tuple), Rsum2, Wsum2])
		 end, OlParsed),
	file:close(Fp),
	file:close(Fw).

sortList([], _, _, List) -> lists:reverse(List);
sortList([H|T], Ol, Tl, L) ->
	N = ta_result_ana:listFind(H, Ol, 1),
	sortList(T, Ol, Tl, [lists:nth(N, Tl)|L]).

changePageFilePrintTime(_, [], []) -> ok;
changePageFilePrintTime(Fpw, [Page|To], [Time|Tt]) ->
	io:format(Fpw, "~p,~p,", tuple_to_list(Page)),
	lists:foreach(fun(Tuple) -> io:format(Fpw, "~p,~p,~p,", tuple_to_list(Tuple)) end,
		Time),
	io:format(Fpw, "~n", []),
	changePageFilePrintTime(Fpw, To, Tt).

changePageFilePrintHead(Fpw, List, 2) ->
	io:format(Fpw, "~p,", [length(List)]),
	lists:foreach(fun(Tuple) -> io:format(Fpw, "~p,~p,", tuple_to_list(Tuple)) end,
		List),
	io:format(Fpw, "~n", []);
changePageFilePrintHead(Fpw, List, 3) ->
	io:format(Fpw, "~p,", [length(List)]),
	lists:foreach(fun({Inode, Index, Time}) -> io:format(Fpw, "~p,~p,~p,", [Inode, Index, Time]) end,
		List),
	io:format(Fpw, "~n", []).

changePageFileCore(_, [], Rl, Wl, Ol, Tl) -> {Rl, Wl, Ol, Tl};
changePageFileCore(Fp, [Page|T], Rl, Wl, Ol, Tl) ->
	Line = parseLine(Fp, " {},\n", 2),
	TimeList = parseNumList(3, Line, []),
	{Inode, Index} = Page,
	% if
	% 	Page =:= {8958,0} ->
	% 		print:printListSample(TimeList),
	% 		exit(-1);
	% 	true ->
	% 		ok
	% end,
	if
		length(TimeList) =:= 1 ->
			TimeTuple = lists:nth(1, TimeList),
			{Time, R, W} = TimeTuple,
			case page_temperature:isReadOnce(R, W) of
				true ->
					changePageFileCore(Fp, T, [{Inode, Index, Time}|Rl], Wl, Ol, Tl);
				false -> 
					case page_temperature:isWriteOnce(R,W) =:= true of
						true ->
							changePageFileCore(Fp, T, Rl, [{Inode, Index, Time}|Wl], Ol, Tl);
						false ->
							changePageFileCore(Fp, T, Rl, Wl, [{Inode, Index}|Ol], [TimeList|Tl])
					end
			end;				
		true ->
			changePageFileCore(Fp, T, Rl, Wl, [{Inode, Index}|Ol], [TimeList|Tl])
	end.

changeAllPageFiles() ->
	% L =  [28, 88, 474, 534, 594, 654, 714, 774, 834, 896, 957],
	L =  [474, 534, 594, 654, 714, 774, 834],
	loopEveryFile(L).

getFile1(Type, T1, T2) -> "../data/nd_filebench2_" ++ atom_to_list(Type) ++ "_" ++ integer_to_list(T1) ++ 
	"_" ++ integer_to_list(T2) ++ "_" ++ ".log".

getFile2(Type, T1, T2) -> "../data/nd_filebench2_" ++ atom_to_list(Type) ++ "new_" ++ integer_to_list(T1) ++ 
	"_" ++ integer_to_list(T2) ++ "_" ++ ".log".

getFile3(Type, T1, T2) -> "../data/nd_filebench2_" ++ atom_to_list(Type) ++ "final_" ++ integer_to_list(T1) ++ 
	"_" ++ integer_to_list(T2) ++ "_" ++ ".log".

loopEveryFile([_H]) -> ok;
loopEveryFile([T1, T2|T]) ->
	changePageFile2(getFile2(page, T1, T2), getFile3(page, T1, T2)),
	loopEveryFile([T2|T]).
	

readHead(S, file) ->
	Line = io:get_line(S, ''),
	[_Numstr|Res] = string:tokens(Line, " ,[]\n"),
	% {_Num, _} = string:to_integer(_Numstr),
	lists:map(fun(X) ->
			[A|_] = string:tokens(X, "\""),
			A
	end, Res);
readHead(S, pid) ->
	Line = io:get_line(S, ''),
	[_Numstr|Res] = string:tokens(Line, " ,[]\n"),
	% {_Num, _} = string:to_integer(_Numstr),
	lists:map(fun(X) ->
			{Num, _} = string:to_integer(X), 
			Num
	end, Res);
readHead(Fp, page) ->
	Line = io:get_line(Fp, ''),
	[_Numstr|Res] = string:tokens(Line, " ,{}[]page\n"),
	% {_Num, _} = string:to_integer(_Numstr),
	myfile:parseNumList(2,Res, []);
readHead(Fp, pagenew) ->
	Rl = parseLine(Fp, ",\n", 1),
	% io:format("#test#read ~p~n",[length(Rl)]),
	RlParsed = lists:reverse(parseNumList(3, Rl, [])),
	% io:format("#test#write~n",[]),
	Wl = parseLine(Fp, ",\n", 1),
	WlParsed = lists:reverse(parseNumList(3, Wl, [])),
	% io:format("#test#other~n",[]),
	Ol = parseLine(Fp, ",\n", 1),
	OlParsed = lists:reverse(parseNumList(2, Ol, [])),
	print:printListSample(RlParsed),
	print:printListSample(WlParsed),
	print:printListSample(OlParsed),
	{RlParsed, WlParsed, OlParsed}.

genFnList([_], L, _, _) -> lists:reverse(L);
genFnList([T1, T2|T], L, Type, Fun) ->
	genFnList([T2|T], [Fun(Type, T1, T2)|L], Type, Fun).