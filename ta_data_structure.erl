-module (ta_data_structure).
-export([recordPageDict/2, recordPidDict/2, recordFileDict/2, recordTimeDict/2, outputResult/4, transTime/1]).
-author(baoning).
-record(request, {pid=-1, time=-1, filename="", inode=-1, index=-1, type=0}).
-record(timetag, {time=-1, r=0, w=0}).
-record (page, {inode = -1, index = -1}).
-record(pidDictItem, {time=-1, r=0, w=0}).
-record (timeDictItem, {r = 0, w=0, pageSet=sets:new()}).

% 9/30/2016
% recordPageDict(Request, Dpage)
% Dpage key=#page, value = [{#timetag}]



% start() ->
%    R = #request{pid = 1, time = 3.111222, filename = "a", inode = 1023, index = 5, type = 0},
%    K = getPidOfReq(R),
%    D1 = recordPidDict(R, dict:new()),
%    io:format("~p~n", [dict:find(K, D1)]),
%    D2 = recordPidDict(R#request{time = 3.222333, index = 8}, D1),
%    io:format("~p~n", [dict:find(K, D2)]),
%    D3 = recordPidDict(R#request{time = 4.222333}, D2),
%    io:format("~p~n", [dict:find(K, D3)]),
%    D4 = recordPidDict(R#request{time = 4.222333, type=1}, D3),
%    io:format("~p~n", [dict:find(K, D4)]).

outputSet(S, Set) ->
   io:format(S, "[", []),
   lists:foreach(fun(X) ->
          io:format(S, "{~p,~p},", [getInodeOfPage(X), getIndexOfPage(X)])
   end, sets:to_list(Set)),
   io:format(S, "]", []).

outputResult(S, K, V, page) ->
   io:format(S, "~p,~p", [getInodeOfPage(K), getIndexOfPage(K)]),
   lists:foreach(fun(X) ->
          io:format(S, ",{~p,~p,~p}", [getTimeOfTime(X), getROfTime(X), getWOfTime(X)])
   end, V),
   io:format(S, "~n", []);
outputResult(S, K, V, time) ->
   io:format(S, "~p", [K]),
   lists:foreach(fun(X) ->
          io:format(S, ",{~p,~p,", [getROfTimeDict(X), getWOfTimeDict(X)]),
          outputSet(S, getPageSetOfTimeDict(X)),
          io:format(S, ",}", [])
   end, V),
   io:format(S, "~n", []);
outputResult(S, K, V, _T) ->
% outputResult(S, K, V, file) ->
   % io:format("#test# ~p, ~p,~p~n", [K, V, T]),
   {PageSet, List} = V,
   io:format(S, "~p, ~p, ", [K, sets:size(PageSet)]),
   outputSet(S, PageSet),
   io:format(S, "~n", []),
   io:format(S, "~p", [K]),
   lists:foreach(fun(X) ->
          io:format(S, ",{~p,~p,~p}", [getTimeOfPid(X), getROfPid(X), getWOfPid(X)])
          
   end, List),
   io:format(S, "~n", []).




getPageOfReq(R) ->
    #page{inode = R#request.inode, index = R#request.index}.
getPidOfReq(R) -> R#request.pid.
getTimeOfReq(R) -> R#request.time.
getFileOfReq(R) -> R#request.filename.
getTypeOfReq(R) -> R#request.type.


getInodeOfPage(K) -> K#page.inode.
getIndexOfPage(K) -> K#page.index.


getTimeOfTime(T) -> T#timetag.time.
getROfTime(T) -> T#timetag.r.
getWOfTime(T) -> T#timetag.w.
addReqOfTime(0, H) ->
   H#timetag{r=getROfTime(H)+1};
addReqOfTime(1, H) ->
   H#timetag{w=getWOfTime(H)+1}.
genTimeTag(0, T) ->
   #timetag{time=T, r=1};
genTimeTag(1, T) ->
   #timetag{time=T, w=1}.



getTimeOfPid(P) -> P#pidDictItem.time.
getROfPid(P) -> P#pidDictItem.r.
getWOfPid(P) -> P#pidDictItem.w.
addReqOfPid(Type, Page, S, H, T) ->  
   if
      Type=:=0 ->
          H1 = H#pidDictItem{r=getROfPid(H)+1};
      Type=:=1 ->
          H1 = H#pidDictItem{w=getWOfPid(H)+1}
   end,
   {sets:add_element(Page, S), [H1|T]}.

genPid(Time, Type, Page) ->
    H = #pidDictItem{time = Time, r=1-Type, w=Type},
    {sets:add_element(Page, sets:new()), [H]}.
genPid(Time, Type, Page, S, H, T) ->
   H1 = #pidDictItem{time = Time, r=1-Type, w=Type},
   {sets:add_element(Page, S), [H1, H|T]}.


getROfTimeDict(H) -> H#timeDictItem.r.
getWOfTimeDict(H) -> H#timeDictItem.w.
getPageSetOfTimeDict(H) -> H#timeDictItem.pageSet.
addReqOfTimeDict(Type, Page, H) ->
   H1 = H#timeDictItem{r = getROfTimeDict(H) + 1 - Type, w = getWOfTimeDict(H)+Type,
    pageSet = sets:add_element(Page, getPageSetOfTimeDict(H))},
   H1.
genTimeDict(Type, Page) ->
   H = #timeDictItem{r = 1 - Type, w = Type,
    pageSet = sets:add_element(Page, sets:new())},
   H.

transTime(T) -> trunc(T).
transFT(Filename) ->
   N = string:rchr(Filename, $.),
   if
      N=:=0 ->
          "no_type";
      true ->
          string:sub_string(Filename, N+1)
   end.

recordPageDict(R, D) ->
   K = getPageOfReq(R),
   Type = getTypeOfReq(R),
   Time = transTime(getTimeOfReq(R)),
   case dict:find(K, D) of
      {ok, [H|T]} ->
          PresentTime = getTimeOfTime(H),
          if
              PresentTime=:=Time ->
                  NewH = addReqOfTime(Type, H),
                  NewL = [NewH|T];
              true ->
                  NewH = genTimeTag(Type, Time),
                  NewL = [NewH, H|T]
          end;
      error ->
          NewH = genTimeTag(Type, Time),
          NewL = [NewH]

   end,
   dict:store(K, NewL, D).


recordPidDict(R, D) ->
   K = getPidOfReq(R),
   Page = getPageOfReq(R),
   Type = getTypeOfReq(R),
   Time = transTime(getTimeOfReq(R)),
   case dict:find(K, D) of
      {ok, {S,[H|T]}} ->
          PresentTime = getTimeOfPid(H),
          if
              PresentTime=:=Time ->
                  NewL = addReqOfPid(Type, Page, S, H, T);
              PresentTime<Time ->
                 NewL = genPid(Time, Type, Page, S, H, T);
              true ->
                 NewL = [],
                   io:format("~p,~p~n", [R, H]),
                   exit(time_error)
          end;
      error ->
          NewL = genPid(Time, Type, Page)
   end,
   dict:store(K, NewL, D).

recordFileDict(R, D) ->
   K = transFT(getFileOfReq(R)),
   Page = getPageOfReq(R),
   Type = getTypeOfReq(R),
   Time = transTime(getTimeOfReq(R)),
   case dict:find(K, D) of
       {ok, {S,[H|T]}} ->
          PresentTime = getTimeOfPid(H),
          if
              PresentTime=:=Time ->
                  NewL = addReqOfPid(Type, Page, S, H, T);
              true ->
                 NewL = genPid(Time, Type, Page, S, H, T)
          end;
      error ->
          NewL = genPid(Time, Type, Page)
   end,
   dict:store(K, NewL, D).

recordTimeDict(R, D) ->   
   Page = getPageOfReq(R),
   Type = getTypeOfReq(R),
   K = transTime(getTimeOfReq(R)),
   case dict:find(K, D) of
      {ok, [H|T]} ->
          NewH = addReqOfTimeDict(Type, Page, H),
          NewL = [NewH|T];
          
      error ->
          NewH = genTimeDict(Type, Page),
          NewL = [NewH]
   end,
   dict:store(K, NewL, D).

