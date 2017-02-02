-module (ta_result_ana).
% -export([start/0, add_period/2]).
-compile (export_all).
-author(baoning).
% time 10/9/2016

% *****************************************************
% pid file type
% head line, number + pid list
% each pid, two lines
% line 1, pid, number of pages, page set : {inode, index}
% line 2, pid, time list : {time, r, w}

% process relevant page information extraction
% get all pids, their relevant pages and active period
% ****************************************************

% page file type
% head line, number + page list {page, inode, index}
% each page, one line
% inode, index, time list {time, r, w} ...
% 1441817,2,{833,303,0},{832,2721,0},{831,3148,0},{830,486,0}


% pid-time-process dict
% key = pid
% value = {pageset, period}, period = {start, end}
-record (pageInfor, {page, time, result={0,0}}).

getPageOfPageInfor(Tuple) -> Tuple#pageInfor.page.
getTimeOfPageInfor(Tuple) -> Tuple#pageInfor.time.
getResultOfPageInfor(Tuple) -> Tuple#pageInfor.result.
genPageInfor(Page, Time, Result) ->
	#pageInfor{page = Page, time = Time, result = Result}.
addResultOfPageInfor(Result, Tuple) ->
	{OldR, OldW} = getResultOfPageInfor(Tuple),
	{R, W} = Result,
	Tuple#pageInfor{result={OldR+R, OldW+W}}.


getPath() -> "../data/".
getFileName() -> "nd_filebench2".
% test
getFile() -> getPath() ++ getFileName().
getWriteFile(Type, T1, T2) -> getFile() ++ "_" ++ atom_to_list(Type) ++ "_" ++ integer_to_list(T1) ++ 
	"_" ++ integer_to_list(T2) ++ "_result" ++ ".log".

getfilepath(Type, T1, T2) -> getFile() ++ "_" ++ atom_to_list(Type) ++ "_" ++ integer_to_list(T1) ++ 
	"_" ++ integer_to_list(T2) ++ "_" ++ ".log".

getReadT() -> 0.1.
getWriteT() -> 0.9.


readFile(Sr, Sw, file) ->
	
	L = myfile:readHead(Sr, file),
	lists:foreach(fun(X) ->
			dealWithPid(Sr, Sw, X)
	end, L);
readFile(Sr, Sw, pid) ->
	L = myfile:readHead(Sr, pid),
	lists:foreach(fun(X) ->
			dealWithPid(Sr, Sw, X)
	end, L).






% function : read lines of pid X,
% calculate r/w, (r+w)/s
% output result
dealWithPid(Sr, Sw, X) ->
% discard the first line, pageSet
	io:get_line(Sr, ''),
	Line = io:get_line(Sr, ''),
	[_Numstr|Res] = string:tokens(Line, " ,{}\n"),
	L = lists:map(fun(Str) ->
			{Num, _} = string:to_integer(Str), 
			Num
	end, Res),
	{T, R, W} = dealWithTimeLine(L, 0, 0, 0),
	Ratio = R/(W+R),
	io:format(Sw, "~p,~p,~p~n", [X, Ratio, (R+W)/T]),
	put("overall", get("overall")+1),
	case Ratio=<getReadT() of
		true ->
			put("read", get("read")+1);
		false ->
			case Ratio>=getWriteT() of
				true ->
					put("write", get("write")+1);
				false ->
					ok
			end
	end.

dealWithTimeLine([], T, R, W) -> {T, R, W};
dealWithTimeLine([_T,R,W|L], Ot, Or, Ow) ->
	dealWithTimeLine(L, Ot+1, Or+R, Ow+W). 

start() ->
   % L =  [28, 88, 474, 534, 594, 654, 714, 774, 834, 896, 957],
   L = [28, 88],
   Fnlist = genFnList(L, [], pid),
   % Fnlist = genFnList(["../data/test1.log"], [], pid),
   Dict = extractInformationOfFiles(Fnlist, dict:new(), pid),

   % io:format("~p~n", [Dict]),
   print:printDict(standard_io, Dict, fun ta_result_ana:printDictItem/2, 10).
% {set, {min, max}}
printDictItem(IoDevice, Item) ->
	{Set, {Min, Max}} = Item,
	print:printListSample(sets:to_list(Set)),	
	io:format(IoDevice, "Period = ~p,~p~n", [Min, Max]).

genFnList([_], L, _) -> lists:reverse(L);
genFnList([T1, T2|T], L, Type) ->
	genFnList([T2|T], [getfilepath(Type, T1, T2)|L], Type).


genFTimeList([_], L) -> lists:reverse(L);
genFTimeList([T1, T2|T], L) ->
	genFTimeList([T2|T], [{T1, T2}|L]).

getrwratioOfPidorFileType() ->
	L =  [28, 88, 474, 534, 594, 654, 714, 774, 834, 896, 957],
    erase(),
    put("read", 0),
    put("write", 0),
    put("overall", 0),
    parseList(file, L).



parseList(_, [_]) -> ok;
parseList(Type, [T1, T2|T]) ->

	{ok, Sr} = file:open(getfilepath(Type, T1, T2), read),
	{ok, Sw} = file:open(getWriteFile(Type, T1, T2), write),
	readFile(Sr, Sw, Type),
	if
		T=:=[] ->
			io:format(Sw, "~p,~p~n", [get("read")/get("overall"), get("write")/get("overall")]);
		true -> ok
	end,
	file:close(Sr),
	file:close(Sw),
	parseList(Type, [T2|T]).



% function : traverse the whole file list to get information
% if type = pid, get both page set information + active period from file
% args : file name list (ordered by time), dict
extractInformationOfFiles([], Information, _) -> Information;
extractInformationOfFiles([Fn|T], Infor, Type) ->
	io:format("extract file ~p, ~p~n", [Fn, erlang:now()]),
	NewInfor = extractInformationOfFile(Fn, Infor, Type),
	extractInformationOfFiles(T, NewInfor, Type).


extractInformationOfFile(Fn, D, pid) ->
	% {_, Start, _} = erlang:now(),
	{ok, Fp} = file:open(Fn, read),
	PidList = myfile:readHead(Fp, pid),
	print:debug("file name = ~p, pid list = ~p~n", [Fn, length(PidList)], false),
	NewDict = getInformationForList(PidList, Fp, D, pid, {pageset, period}),
	file:close(Fp),
	% {_, Stop, _} = erlang:now(),		
	% io:format("Time = ~p~n", [Stop-Start]),
	NewDict;
extractInformationOfFile(Fn, PageInfor, page) ->
	{ok, Fp} = file:open(Fn, read),
	PageList = myfile:readHead(Fp, page),
	N = listFind(getPageOfPageInfor(PageInfor), PageList, 1),
	if
		N=<0 ->
			file:close(Fp),
			NewPageInfor = PageInfor;
		true->	
			myfile:readLines(Fp, N-1),
			Line = io:get_line(Fp, ''),
			[_InodeStr, _IndexStr|Res] = string:tokens(Line, " ,{}\n"),
			TimeTupleList = myfile:parseNumList(3, Res, []),
			NewPageInfor = addNeededTime(PageInfor, TimeTupleList),	
			 % print:debug("page list length = ~p, first element = ~p~n", [length(PageList), lists:nth(1, PageList)], false),
			file:close(Fp)
			
	end,
	NewPageInfor.

addNeededTime(PageInfor, []) -> PageInfor;
addNeededTime(PageInfor, [{Time, R, W}|T]) ->
	{StartTime, EndTime} = getTimeOfPageInfor(PageInfor),
	if
		Time > EndTime ->
			PageInfor;
		Time < StartTime ->
			addNeededTime(PageInfor, T);
		true ->
			addNeededTime(addResultOfPageInfor({R,W}, PageInfor), T)
	end.

listFind(_, [], _) -> -1;
listFind(Page, [Page|_T], Loc) -> Loc;
listFind(Page, [_|T], Loc) ->
	listFind(Page, T, Loc+1).

getInformationForList([], _, D, _, _) -> D;
getInformationForList([H|T], Fp, D, pid, {pageset, period}) ->
	PageList = getPageList(Fp, pid),
	Period = getPeriod(Fp, pid),
	case dict:find(H, D) of
		{ok, {PageSet, OldPeriod}} ->
			NewPageSet = sets_add_list(PageList, PageSet),
			NewPeriod = add_period(Period, OldPeriod),
			Value = {NewPageSet, NewPeriod};
		error ->
			Value = {sets:from_list(PageList), Period}
	end,
	print:debug("page set = ~p, Period = ~p~n", [PageList, Period], false),
	% exit(-1),
	getInformationForList(T, Fp, dict:store(H, Value, D), pid, {pageset, period}).

getPageList(Fp, pid) ->
	Line = io:get_line(Fp, ''),
	[_, _Numstr|Res] = string:tokens(Line, " ,[]{}\n"),
	% {_Num, _} = string:to_integer(_Numstr),
	myfile:parseNumList(2, Res, []).




getPeriod(Fp, pid) ->
	Line = io:get_line(Fp, ''),
	[_Numstr|Res] = string:tokens(Line, " ,{}\n"),
	{Max,_} = string:to_integer(lists:nth(1, Res)),
	{Min,_} = string:to_integer(lists:nth(length(Res)-2, Res)),
	{Min, Max}.

sets_add_list([], Set) -> Set;
sets_add_list([H|T], Set) ->
	sets_add_list(T, sets:add_element(H, Set)).

add_period(Period, OldPeriod) ->
	{M1, M2} = Period,
	{M3, M4} = OldPeriod,
	L = [M1, M2, M3, M4],
	{lists:min(L), lists:max(L)}.


% getPageRW(Page, StartTime, EndTime, TimeList) ->
% 	NeededTimeList = getTimeListForTimePeriod(StartTime, EndTime, TimeList, []),
% 	Fnlist = genFnList(NeededTimeList, [], page),
% 	Time = {StartTime, EndTime},
% 	Result = {0,0},
% 	PageInfor = genPageInfor(Page, Time, Result),
% 	extractInformationOfFiles(Fnlist, PageInfor, page).

getTimeListForTimePeriod(_StartTime, _EndTime, [FileEndTime], ResultList) ->
	lists:reverse([FileEndTime|ResultList]);
getTimeListForTimePeriod(StartTime, EndTime, [FileStartTime, FileEndTime|T], ResultList) ->
	case EndTime < FileStartTime of
	 	true ->
	 		lists:reverse([FileStartTime|ResultList]);
	 	false when StartTime >= FileEndTime ->
	 		getTimeListForTimePeriod(StartTime, EndTime, [FileEndTime|T], ResultList);
	 	false ->
	 		getTimeListForTimePeriod(StartTime, EndTime, [FileEndTime|T], [FileStartTime|ResultList])
	 end. 
		

calProcessPageTemperature() ->
   % All time list must in increasing order.
   % FileTimeList = [474, 534, 594, 654, 714, 774, 834],
   FileTimeTupleList = genFTimeList([534, 594, 654, 714], []),
   Fnlist = genFnList([594, 654], [], pid),
   % Fnlist = genFnList(["../data/test1.log"], [], pid),
   io:format("process information extraction start,~p~n", [erlang:now()]),
   Dict = extractInformationOfFiles(Fnlist, dict:new(), pid),
   io:format("process information extraction finished,~p~n", [erlang:now()]),
   calProcessPageTemperatureLoopFile(FileTimeTupleList, Dict).

calProcessPageTemperatureLoopFile([], Dict) -> 
	Keylist = dict:fetch_keys(Dict),
	lists:foreach(fun(Key) -> Value = dict:fetch(Key, Dict),
		calProcessPageTemperaturePrint(Key, element(2,Value), length(element(1,Value)))
	end, Keylist);
calProcessPageTemperatureLoopFile([{T1, T2}|T], Dict) ->
	io:format("file is ~p, ~p, ~p~n", [T1, T2,erlang:now()]),
	Keylist = dict:fetch_keys(Dict),
	{NewDict, ReqList} = calProcessPageTemperatureLoopPid(Keylist, {T1, T2}, Dict, []),
	io:format("request list generation finished, req list = ~p,~p~n", [length(ReqList), erlang:now()]),
	NewDict2 = calProcessPageTemperatureDealReqs(myfile:getFile2(page, T1, T2), ReqList, NewDict),
	io:format("deal with request finished,~p~n", [erlang:now()]),
	calProcessPageTemperatureLoopFile(T, NewDict2).

calProcessPageTemperatureLoopPid([], _, Dict, ReqList) -> {Dict, ReqList};
calProcessPageTemperatureLoopPid([Key|T], {T1, T2}, Dict, ReqList) ->
	Value = dict:fetch(Key, Dict),
	{PageSet, Time} = Value,
	if
		% not been changed into time list 
		is_tuple(Time) ->
			TimeList = genDictTimeList(Time),			
			PageList = sets:to_list(PageSet),
			NewDict = dict:store(Key, {PageList, TimeList}, Dict);
		true ->
			TimeList = Time,
			PageList = PageSet,
			NewDict = Dict
	end,
	
	NewTimeList = isDiscard({T1, T2}, TimeList),
	if
		NewTimeList=:=error ->
			calProcessPageTemperaturePrint(Key, TimeList, length(PageList)),
			NewDict2 = dict:erase(Key, NewDict),
			calProcessPageTemperatureLoopPid(T, {T1,T2}, NewDict2, ReqList);
		true ->
			ReqListAdd = listMultiZip(Key, NewTimeList, PageList, []),
			calProcessPageTemperatureLoopPid(T, {T1,T2}, NewDict, ReqList++ReqListAdd)
	end.

calProcessPageTemperaturePrint(Key, TimeList, PageNumber) -> 
	{ok, Fp} = file:open("../data/test.log", [append]),
	io:format(Fp, "~p,", [Key]),
	lists:foreach(fun({Start, End, R, W})  ->
		Tem = page_temperature:getTemperature(R, W) / (End - Start + 1) / PageNumber,
		io:format(Fp, "~p,~p,~p,", [Start, End, Tem])
	end, TimeList),		
	io:format(Fp, "~n", []),
	file:close(Fp).


listMultiZip(_, [], _, ResultList) -> ResultList;
listMultiZip(Pid, [Head|Tail], List, ResultList) ->
	Added = lists:map(fun(Ele) -> {Head, Ele, Pid} end, List),
	listMultiZip(Pid, Tail, List, ResultList ++ Added).

% if all time window is previous of T1, return error
% return all time windows inside {T1, T2}
% File valid time is T1 - T2-1
% For time list, valid time is Tstart - Tend
% Timelist is ordered
isDiscard({T1, T2}, TimeList) ->
	% io:format("#test#~p,~p,~p~n", [T1, T2, TimeList]),
	R = lists:foldr(fun(Ele, List) ->
		Tstart = element(1, Ele),
		Tend = element(2, Ele),
		if
			T1 > Tend ->
				List;
			T2-1 < Tstart ->
				List;
			true ->
				% io:format("#test#~p~n", [[{max(T1, Tstart), min(Tend, T2-1)}|List]]),
				[{max(T1, Tstart), min(Tend, T2-1)}|List]
		end end, [], TimeList),
	case element(2, lists:last(TimeList)) < T1 of
		true ->
			error;
		false ->
			R
	end.
	

genDictTimeList(Time) ->	
	Tpre =  page_temperature:getPreviousTimeWindow(Time),	
	{Start2, End2} = Time,
	Taft = page_temperature:getNextTimeWindow(Time),
	if
		Taft=:=error ->
			L1 = [];
		true ->
			{Start3, End3} = Taft,
			L1 = [{Start3, End3, 0, 0}]

	end,
	L2 = [{Start2, End2, 0, 0}|L1],
	if
		Tpre=:=error ->
			L3 = L2;
		true ->
			{Start1, End1} = Tpre,
			L3 = [{Start1, End1, 0, 0}|L2]

	end,
	L3.
% ReqList = {Time, Page, Pid}
calProcessPageTemperatureDealReqs(Fn, ReqList, Dict) -> 
	{ok, Fp} = file:open(Fn, read),
	Head = myfile:readHead(Fp, pagenew),
	Rl = element(1,Head),
	Wl = element(2, Head),
	Ol = element(3, Head),
	ListSort = lists:keysort(2, ReqList),
	NewDict = calProcessPageTemperatureLoopReq(Fp, Rl, Wl, Ol, ListSort, Dict),
	file:close(Fp),
	NewDict.	
calProcessPageTemperatureLoopReq(_, _, _, _, [], Dict) -> Dict;
calProcessPageTemperatureLoopReq(Fp, Rl, Wl, Ol, ListSort, Dict) ->
	Page = element(2, lists:nth(1, ListSort)),
	{PageList, RemainList} = splitTupleList(Page, ListSort, []),
	case isInTupleList(Page, Rl) of
		{true,Time} ->
			NewDict = calProcessPageTemperatureAddResult([{Time, 1, 0}], PageList, Dict),
			calProcessPageTemperatureLoopReq(Fp, Rl, Wl, Ol, RemainList, NewDict);
		false ->
			case isInTupleList(Page, Wl) of
				{true,Time} ->
					NewDict = calProcessPageTemperatureAddResult([{Time, 0, 1}], PageList, Dict),
					calProcessPageTemperatureLoopReq(Fp, Rl, Wl, Ol, RemainList, NewDict);
				false ->
					Loc = listFind(Page, Ol, 1),
					if
						Loc < 1 ->
							calProcessPageTemperatureLoopReq(Fp, Rl, Wl, Ol, RemainList, Dict);
						true ->						
							NewOl = lists:sublist(Ol, Loc+1, length(Ol)-Loc),
							myfile:readLines(Fp, Loc-1),
							Line = myfile:parseLine(Fp, ",\n", 2),
							TimeList = myfile:parseNumList(3, Line, []),
							NewDict = calProcessPageTemperatureAddResult(TimeList, PageList, Dict),
							calProcessPageTemperatureLoopReq(Fp, Rl, Wl, NewOl, RemainList, NewDict)
					end
			end

	end.

calProcessPageTemperatureAddResult(TimeList, ReqList, Dict) ->
	lists:foldr(fun(TimeEle, Dict1) -> 
		lists:foldr(fun(Req, Dict2) -> 
			Time = element(1, TimeEle),
			ReqTimeWindow = element(1, Req),
			Pid = element(3, Req),
			case isInRange(Time, ReqTimeWindow) of
				false ->
					Dict2;
				true ->
					Value = dict:fetch(Pid, Dict2),
					DictTimeWindow = element(2, Value),
					NewDicttw = getProperDictTimeWindow(TimeEle, DictTimeWindow, []),
					if
						NewDicttw=:=error ->
							exit(error);
						true ->
							dict:store(Pid, setelement(2, Value, NewDicttw), Dict2)
					end					
			end
		end, Dict1, ReqList) end, Dict, TimeList).

isInRange(Time, TimeWindow) ->
	Start = element(1, TimeWindow),
	End = element(2, TimeWindow),
	if
		Time < Start ->
			false;
		Time > End ->
			false;
		true ->
			true
	end.

getProperDictTimeWindow(_, [], _) -> error;
getProperDictTimeWindow(TimeEle, [DictTime|T], Result) ->
	% io:format("test:~p,~p,~p,~p~n", [TimeEle, DictTime, T, Result]),
	Time = element(1, TimeEle),
	case isInRange(Time, DictTime) of
		false ->
			getProperDictTimeWindow(TimeEle, T, [DictTime|Result]);
		true ->
			R = element(2, TimeEle) + element(3, DictTime),
			W = element(3, TimeEle) + element(4, DictTime),
			NewDictTime = setelement(4,setelement(3, DictTime, R),W),
			lists:reverse(lists:reverse(T) ++ [NewDictTime|Result])
	end.

isInTupleList({Inode, Index}, [{Inode, Index, Time}|_]) -> {true, Time};
isInTupleList(_, []) -> false;
isInTupleList(Page, [_|T]) ->	isInTupleList(Page, T).

splitTupleList(Page, [{Time, Page, Pid}|T], PageList) ->
	splitTupleList(Page, T, [{Time, Page, Pid}|PageList]);
splitTupleList(_, ListSort, PageList) ->
	{PageList, ListSort}.

getCoarsePageTem([_], _, _) -> ok;
getCoarsePageTem([Htime|Ttime], Fw, DictTuple) ->
	[Fn|T] = myfile:genFnList([Htime|Ttime], [], page, fun myfile:getFile3/3),
 	[Pidfn|_] = myfile:genFnList([Htime|Ttime], [], pid, fun myfile:getFile1/3),
	print:printTime("start load dict,~p~n", [Fn]),
	DictTuple2 = loadDict([Fn|T], DictTuple),
	print:printTime("load dict finished~n", []),
	{ok, Fp} = file:open(Pidfn, read),
	PidList = myfile:readHead(Fp, pid),
	% print:printTime("start loop pid~n", []),
	lists:foreach(fun(Pid) ->
		% print:printTime("start loop pid~p~n", [Pid]),
		PageList = getPageList(Fp, pid),
		io:get_line(Fp,''),
		Sum = lists:foldr(fun(Page, SumTuple) -> 
			case dict:find(Page, element(1, DictTuple2)) of
				{ok, Tem} ->
					OldTem = element(1, SumTuple),
					
					Ele1 = OldTem+Tem;
				error ->
					Ele1 = element(1, SumTuple)
			end,
			case dict:find(Page, element(2, DictTuple2)) of
				{ok, Tem2} ->
					OldTem2 = element(2, SumTuple),					
					Ele2 = OldTem2 + Tem2;
				error ->
					Ele2 = element(2, SumTuple)
			end,
			{Ele1, Ele2}
			 end, {0,0}, PageList),
		Length = length(PageList),
		% print:printTime("pid result ~p, ~p~n", [element(1,Sum)/Length, element(2, Sum)/Length]),
		io:format(Fw, "~p,~p,~p~n", [Pid, element(1,Sum)/Length, element(2, Sum)/Length])
	end, PidList),
	file:close(Fp),
	getCoarsePageTem(Ttime, Fw, DictTuple2).	




loadDict([], _) -> ok;
loadDict([_], DictTuple) ->
	{element(2, DictTuple), element(1, DictTuple)};
loadDict([File1, File2|_], DictTuple) ->
	NewDict = loadDict(File2),
	case dict:size(element(1, DictTuple)) of
		0 ->
			NewDict2 = loadDict(File1),
			{NewDict2, NewDict};
		_ ->
			{element(2, DictTuple), NewDict}
	end.

loadDict(Fn) ->
	{ok, Fp} = file:open(Fn, read),
	Rl = myfile:parseLine(Fp, ",\n", 1),
	% io:format("#test#read ~p~n",[length(Rl)]),
	RlParsed = lists:reverse(myfile:parseNumList(2, Rl, [])),
	% io:format("#test#write~n",[]),
	Dict1 = lists:foldr(fun(Page, Dict) -> dict:store(Page, page_temperature:getTemperature(1,0), 
		Dict) end, dict:new(), RlParsed),
	print:printListSample(RlParsed),
	Wl = myfile:parseLine(Fp, ",\n", 1),
	WlParsed = lists:reverse(myfile:parseNumList(2, Wl, [])),
	Dict2 = lists:foldr(fun(Page, Dict) -> dict:store(Page, page_temperature:getTemperature(0,1), 
		Dict) end, Dict1, WlParsed),
	print:printListSample(WlParsed),
	% io:format("#test#other~n",[]),
	Ol = myfile:parseLine(Fp, ",\n", 1),
	OlParsed = lists:reverse(myfile:parseNumList(4, Ol, [])),
	Dict3 = lists:foldr(fun(Page, Dict) -> dict:store({element(1,Page), element(2, Page)}, 
		page_temperature:getTemperature(element(3,Page), element(4, Page)), Dict) end, 
	Dict2, OlParsed),
	
	print:printListSample(OlParsed),
	file:close(Fp),
	Dict3.
	
