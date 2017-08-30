-module(barrel_ctl).

%% API exports
-export([
  new_snapshot/1,
  restore_from_snapshot/1
]).


new_snapshot([DbName, Path]) ->
  case filelib:is_dir(Path) of
    true ->
      io:format("ERROR: ~p alreaday exits.~n", [Path]),
      error;
    false ->
      case barrel_backup:new_snapshot(list_to_binary(DbName), Path) of
        ok ->
          io:format("SUCCESS: ~p snapshot created at ~p~n", [DbName, Path]),
          ok;
        {error, Reason} ->
          io:format("ERROR: ~p.~n", [Reason]),
          error
      end
  end;
new_snapshot(_) ->
  io:format("ERROR: invalid arguments.~n", []),
  error.

restore_from_snapshot([DbName, Path]) ->
  case barrel_backup:restore_from_snapshot(list_to_binary(DbName), Path) of
    {ok, OldPath} ->
      io:format("SUCCESS: ~s", [OldPath]),
      ok;
    {error, Reason} ->
      io:format("ERROR: ~p.~n", [Reason]),
      error
  end;
restore_from_snapshot(_) ->
  io:format("ERROR: invalid arguments.~n", []),
  error.

