-file("eqc-1.41.2/loadexamples/resman.erl", 0).
%%% @author Thomas Arts 
%%% @copyright (C) 2014, Thomas Arts
%%% @doc Simple resource manager to serve as an example for load testing
%%%
%%% @end
%%% Created :  7 Aug 2014 by Thomas Arts <thomas.arts@quviq.com>
-module(resman).

-export([start/0]).

start() ->
  spawn_link(fun() -> loop(0,0) end).

loop(Users,New) ->
  receive
    use  -> case Users + New < 1000 of 
	      true -> loop(Users+1,New+1);
	      false -> exit(crash)
	    end;
    free -> loop(Users-1,0);
    stop -> Users
  end.







