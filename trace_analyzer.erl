
-module (trace_analyzer).
-export ([start/0, parse/1, cutRequest/1, getTimeOfReq/1, getFile/0]).
-author(baoning).

-record (requestItem, {timestamp, type, filename, inode, count, pos, isize=-1, pid}).
-record(request, {pid=-1, time=-1, filename="", inode=-1, index=-1, type=0}).

isDebug() -> false.

getTimeOfReq(R) -> R#requestItem.timestamp.
getTypeOfReq(R) -> R#requestItem.type.
getFnOfReq(R) -> R#requestItem.filename.
getInodeOfReq(R) -> R#requestItem.inode.
getCountOfReq(R) -> R#requestItem.count.
getPosOfReq(R) -> R#requestItem.pos.
getIsizeOfReq(R) -> R#requestItem.isize.
getPidOfReq(R) -> R#requestItem.pid.


getPath() -> "../data/".
getFileName() -> "nd_filebench2".
% test
getFile() -> getPath() ++ getFileName().
getTimeWindow() -> 60.
getMaxTime() -> 10000.
getReadLength() -> 1000.
getPageShift() -> 12.
% attention : this is the not state of C code.
% In C, page mask = ~(Page Size -1). When used, Num & ~page_mask
getPageMask() -> 1 bsl 12-1.
getPageSize() -> 4096.

start() ->
Start = erlang:now(),
{ok, Sr} = file:open(getFile(),read),
   % jump the head
   print:readLines(Sr, 11),
   % readLines(Sr, 445272),
    erase(),
    put("time", -1),
    put("line",0),
    put("num", 0),
   DT = readFile(Sr, {dict:new(), dict:new(), dict:new(), dict:new()}),   
   file:close(Sr),
    io:format("load file finished, start output~n", []),

   outputResult(DT, get("time"), getMaxTime()),
   Stop = erlang:now(),
   io:format("time consumed = ~ps~n", [(Stop - Start)/1000000]).

readFile(S, DT) ->
   R = readLines(S, getReadLength(), []),
   NewDT = dealWithRequests(R, DT),
   case length(R)=:=getReadLength() of
      true ->
          readFile(S, NewDT);
      false ->
          NewDT
   end.

dealWithRequests([], DT) -> DT;
dealWithRequests([H|T], DT) ->
    T1 = get("time"),
    T2 = ta_data_structure:transTime(getTimeOfReq(H)),
    case T1=:=-1 of
        true ->
            put("time", T2),
            MiddleDT = DT;
        false->
            case T2-T1>=getTimeWindow() of
                true ->
                    put("time", T2),
                    outputResult(DT, T1, T2),
                    MiddleDT = {dict:new(), dict:new(), dict:new(), dict:new()},
                    {D1, D2, D3, D4} = MiddleDT,
                    S = dict:size(D1) + dict:size(D2) + dict:size(D3) + dict:size(D4),
                    io:format("~p, ~p~n", [S, H]);
                false ->
                    MiddleDT = DT
            end
    end,
   L = cutRequest(H),
   NewDT = dealWithPageRequests(L, MiddleDT),
   % outputSize(H, NewDT),
   dealWithRequests(T, NewDT).



outputSize(H, NewDT) ->
	{D1, D2, D3, D4} = NewDT,
	S = dict:size(D1) + dict:size(D2) + dict:size(D3) + dict:size(D4),
	N = get("num"),
	case S>N*1000 of
		true ->
			io:format("~p, ~p~n", [S, H]),
			put("num", trunc(S/1000)+1);
			% exit(test);
		false ->
			ok
	end.

dealWithPageRequests([], DT) -> DT;
dealWithPageRequests([H|T], DT) ->
   {Dpage, Dpid, Dfile, Dtime} = DT,
   NDpage = ta_data_structure:recordPageDict(H, Dpage),
   NDpid = ta_data_structure:recordPidDict(H, Dpid),
   NDfile = ta_data_structure:recordFileDict(H, Dfile),
   NDtime = ta_data_structure:recordTimeDict(H, Dtime),
   dealWithPageRequests(T, {NDpage, NDpid, NDfile, NDtime}).





% return a list of request items
readLines(_S, 0, L) ->
   N = get("line"),
   put("line", N+1),
   io:format("finished ~p * ~p lines~n", [N+1,getReadLength()]),
   lists:reverse(L);
readLines(S, N, L) ->
   %
   Line = io:get_line(S, ''),
   case Line of
      eof ->
          L;
      _->
          
          H = parse(Line),
          
          case tuple_size(H) of
              0 ->
                  readLines(S, N, L);
              _->
                  % io:format("#test# ~p~n",[H]),        
                  outputRequestItem(H),
                  readLines(S, N-1, [H|L])
          end
   end.


outputRequestItem(H) ->
    case isDebug() of
        true ->
            io:format("timestamp=~p, type=~p, filename=~p, inode=~p, count=~p, pos=~p, isize=~p, pid=~p~n",
                [getTimeOfReq(H), getTypeOfReq(H), getFnOfReq(H), getInodeOfReq(H), getCountOfReq(H), getPosOfReq(H),
                getIsizeOfReq(H), getPidOfReq(H)]);
        false ->
            ok
    end.

debug(S, L) ->
    case isDebug() of
        true ->
            io:format(S, L);
        false ->
            ok
    end.
% ftrace
% 1 task, 2 pid, 3 cpu, 4 signs, 5 timestamp, 6 function
% read (6)
% trace_printk("%lu,%lld,%lld,%zu,%llu,%s\n",
%            inode->i_ino, i_size_read(inode), *ppos, iter->count, time, filp->f_path.dentry->d_iname);
% write (5)
% trace_printk("%lu,%lld,%zu,%llu,%s\n",
                  % mapping->host->i_ino, pos, i->count,time,file->f_path.dentry->d_iname);
parse(Line) ->
   % there may be : and - in task. Since the index of the '[' of cpu is constant
   % Separate the head and tail
   Loc = string:chr(Line, $[),
   if
       Loc =:= 0 ->
           io:format("~p~n", [Line]),
           {};
       true ->
   
   Head = string:substr(Line, 1, Loc-1),
   Tail = Line -- Head,
   Index = string:rchr(Head, $-),
   Task = string:substr(Head, 1, Index-1),
   Pid_str = string:substr(Head, Index+1),
   Res = [Task, Pid_str|string:tokens(Tail," []<>,:\n\r\t")],
   debug("#test# #Res# ~n~p~n", [Line]),   
   debug("#test# ~p~n", [length(Res)]),
  
   case length(Res) of
      8 ->
          {};
      Len ->
          {Time, _} = string:to_float(lists:nth(5, Res)),
          {Pid, _} = string:to_integer(lists:nth(2, Res)),
          {Inode, _} = string:to_integer(lists:nth(7, Res)),
          if
              Len=:=12 ->
                  {Isize, _} = string:to_integer(lists:nth(8, Res)),
                  {Pos, _} = string:to_integer(lists:nth(9, Res)),
                  {Count, _} = string:to_integer(lists:nth(10, Res)),
                  Path = lists:nth(12, Res),
                  #requestItem{timestamp=Time, type=0, filename=Path, inode=Inode, count=Count,
                   pos=Pos, isize=Isize, pid=Pid};
              Len=:=11 ->
                  {Pos, _} = string:to_integer(lists:nth(8, Res)),   
                  {Count, _} = string:to_integer(lists:nth(9, Res)),
                  Path = lists:nth(11, Res),
                  #requestItem{timestamp=Time, type=1, filename=Path, inode=Inode, count=Count,
                   pos=Pos, pid=Pid};
             true ->
                 {ok, S} = file:open(getPath() ++ "debug.log", [append]),
                 io:format(S, "~p~n", [Line]),
                 file:close(S),
                 {}
          end
          
   end
    end.

cutRequest(R) ->
   Pid = getPidOfReq(R),
   Time = getTimeOfReq(R),
   Fn = getFnOfReq(R),
   Inode = getInodeOfReq(R),
   Type = getTypeOfReq(R),
   Pos = getPosOfReq(R),
   Count = getCountOfReq(R),
   Index = Pos bsr getPageShift(),
   Offset = Pos band getPageMask(),
   case Type of
      0 ->
          Isize = getIsizeOfReq(R),           
          EndIndex = (Isize-1) bsr getPageShift(),
          L = cutReadReq(Index, Offset, 0, Count, EndIndex, Isize, []),
          lists:map(fun(X)->
                  #request{pid=Pid, time=Time, filename=Fn, inode=Inode, index=X, type=0}
          end, L);
      1 ->
          L = cutWriteReq(Index, Offset, Count, []),
          lists:map(fun(X)->
                  #request{pid=Pid, time=Time, filename=Fn, inode=Inode, index=X, type=1}
          end, L)
   end.

cutWriteReq(_Index, _Offset, 0, L) -> L;
cutWriteReq(Index, Offset, C, L) ->

   Copied = min(getPageSize()-Offset, C),
   % io:format("#test# Index = ~p, Offset = ~p, C = ~p, Copied = ~p~n",[Index, Offset, C, Copied]),
   cutWriteReq(Index+1, 0, C-Copied, [Index|L]).

cutReadReq(EndIndex, Offset, Written, Count, EndIndex, Isize, L) ->
   Nr = (Isize-1) band getPageMask() - Offset,
   if
      Nr=<0 ->
          L;
      Written>=Count->
          L;
      true ->
          [EndIndex|L]

   end;
cutReadReq(Index, Offset, Written, Count, EndIndex, Isize, L) ->
   % io:format("Written = ~p Count = ~p~n",[Written, Count]),
   if
      Written>=Count ->
          L;
      true->   
          Nr = min(getPageSize() - Offset, Count-Written),
          cutReadReq(Index+1, 0, Written+Nr, Count, EndIndex, Isize, [Index|L])
   end.

outputResult(D, Type, T1, T2) ->
   {ok, Sw} = file:open(getFile() ++ "_" ++ atom_to_list(Type) ++ "_" ++ integer_to_list(T1) ++
    "_" ++ integer_to_list(T2) ++ "_" ++ ".log", write),
   Keys = lists:sort(dict:fetch_keys(D)),
   io:format(Sw, "~p, [", [length(Keys)]),
   lists:foreach(fun(X)  ->
           io:format(Sw, "~p,",[X])
   end, Keys),
   io:format(Sw, "]~n", []),
   lists:foreach(fun(X) ->
          V = dict:fetch(X, D),
          ta_data_structure:outputResult(Sw, X, V, Type)
   end, Keys),
   file:close(Sw).

outputResult({Dpage, Dpid, Dfile, Dtime}, T1, T2) ->
   outputResult(Dpage, page, T1, T2),
   outputResult(Dpid, pid, T1, T2),
   outputResult(Dfile, file, T1, T2),
   outputResult(Dtime, time, T1, T2).
    

