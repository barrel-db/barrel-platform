-file("eqc-1.41.2/examples/putcluster_eqc.erl", 0).
%%% @author  <John@JOHNSTABLET2014>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2015 by  <John@JOHNSTABLET2014>

-module(putcluster_eqc).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_cluster.hrl").
-compile(export_all).

components() ->
  [putline_eqc,putchar_eqc].

api_spec() -> eqc_cluster:api_spec(?MODULE).

prop_putcluster() ->
  ?SETUP(fun() -> 
             eqc_mocking:start_mocking(api_spec()),  
             fun() -> ok end
         end, 
         ?FORALL(Cmds, commands(?MODULE),
                 begin
                   {H, S, Res} = run_commands(?MODULE,Cmds),
                   pretty_commands(?MODULE, Cmds, {H, S, Res},
                                   measure(length, length(Cmds),
                                           aggregate(command_names(Cmds),
                                                     %%eqc_statem:call_features(H),
                                                     features(eqc_statem:call_features(H),
                                                              Res == ok))))
                 end)).

