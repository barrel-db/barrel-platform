#!/usr/bin/env escript
%% -*- erlang -*-
main([File]) ->
  {ok, Config0} = file:consult(File),
  Config1 = proplists:delete(deps, Config0),


  IoList = lists:foldl(fun(Line, Acc) ->
                        [io_lib:format("~p\.~n", [Line]) | Acc]
                       end, [], Config1),



  file:write_file(File, lists:reverse(IoList)).

